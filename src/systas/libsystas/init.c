/* init.c - starting the interpreter
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <setjmp.h>
#include "hackerlab/mem/mem.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/vu/vfdbuf.h"
#include "systas/libsystas/init.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/init.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/appinit.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/async.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/fdsocket.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/hash.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/load.h"
#include "systas/libsystas/macros.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/promise.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/rgx.h"
#include "systas/libsystas/unexec.h"
#include "systas/libsystas/scheme.h"
#include "systas/libsystas/socket.h"
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/stime.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/struct.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/variable.h"
#include "systas/libsystas/weaks.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/vuprocs.h"
#include "systas/libsystas/md5s.h"


/************************************************************************
 * Entering the Interpreter Internals
 *
 * When the interpreter is first entered, initialization functions
 * for each module are called.  These construct symbols, built-in 
 * functions, internal data structures and so forth.  If the program
 * linked against this library has supplied a function "scm_appinit",
 * that becomes the last initialization function called.
 *
 * The root continutation is constructed.  The root continutation is
 * a stackless continuation (no stack is copied to the heap).  It is
 * used by GC and call-with-current-continuation to find the bottom
 * of the stack.  It is only ever invoked, in scm_ithrow, for the purpose
 * of handling unhandled exceptions by exitting Scheme.
 *
 * The initialization function is passed a (C) string which is read and
 * evaluated.  The value of that evaluation is converted to a string
 * and returned to the caller.
 * 
 * Before evaluation begins, Scheme signal handlers are installed, the 
 * garbage collector is unblocked, and interrupts are enabled.  When 
 * evaluation ends, those steps are reversed.
 */



SCM_SYMBOL (s_continuation, "continuation");
SCM_SYMBOL (s_boot_systas, "boot_systas");
SCM_SYMBOL (s_apply_to_argv, "apply_to_argv");



/* scm_start_stack
 * 
 * Reinitialize an existing root continuation (a continuation which exits
 * Scheme).
 */
void
scm_restart_stack (void * base)
{
  SCM_INTS_UNKNOWN;

  scm_stack_base = (SCM_STACKITEM *)base;
  scm_dynwinds = SCM_EOL;
  scm_debug_info = 0;
  SCM_REGS (scm_rootcont)->dynwind = SCM_EOL;
  SCM_REGS (scm_rootcont)->throw_value = SCM_EOL;
  SCM_REGS (scm_rootcont)->base = (SCM_STACKITEM *)base;
}


/* scm_start_stack
 * 
 * Create and initialize a root continuation (a continuation which exits
 * Scheme).
 */
static void
scm_start_stack (void * base)
{
  SCM_INTS_ENABLED;

  scm_stack_base = (SCM_STACKITEM *)base;

  scm_cur_inp = SCM_BOOL_F;
  scm_cur_outp = SCM_BOOL_F;
  scm_cur_errp = SCM_BOOL_F;
  scm_cur_loadp = SCM_BOOL_F;

  scm_progargs = SCM_BOOL_F;	/* vestigial */
  scm_exitval = SCM_BOOL_F;	/* vestigial */

  scm_top_level_lookup_thunk_var = SCM_BOOL_F;

  /* Create an object to hold the root continuation.
   */
  SCM_NEWCELL (scm_rootcont);
  SCM_REGS (scm_rootcont) = (scm_regs *)scm_must_malloc ((long) sizeof (scm_regs));
  SCM_CAR (scm_rootcont) = scm_tc7_contin;
  /* The root continuation is further initialized by scm_restart_stack. */

  /* The remainder of stack initialization is factored out to another function so that
   * if this stack is ever exitted, it can be re-entered using scm_restart_stack.
   */
  scm_restart_stack (base);
}



/* scm_boot_inits
 * 
 * Initialize all built-in functions.
 */
static void
scm_boot_inits (SCM_STACKITEM * i,
		int in, int out, int err,
		int argc, char ** argv)
{
  SCM_INTS_ENABLED;
  static int once = 0;

  if (once)
    return;

  once = 1;

  scm_smob_prehistory ();
  scm_init_storage ();
  scm_start_stack (i);
  scm_init_procs ();
  scm_init_init ();
  scm_init_kw ();		/* must be early for SCM_KEYWORD */
  scm_init_alist ();
  scm_init_async ();
  scm_init_boolean ();
  scm_init_chars ();
  scm_init_continuations ();
  scm_init_dynwind ();
  scm_init_eq ();
  scm_init_error ();
  scm_init_system ();
  scm_init_filesys ();
  scm_init_fdsocket ();
  scm_init_socket ();
  scm_init_gc ();
  scm_init_hash ();
  scm_init_hashtree ();
  scm_init_list ();
  scm_init_numbers ();
  scm_init_pairs ();
  scm_init_ports ();
  scm_init_vuprocs ();
  scm_init_stackchk ();
  scm_init_struct ();
  scm_init_symbols ();
  scm_init_load ();
  scm_init_macros ();
  scm_init_promise ();
  scm_init_read_print ();
  scm_init_rgx ();
  scm_init_unexec ();
  scm_init_md5s ();
  scm_init_scheme ();
  scm_init_stime ();
  scm_init_strings ();
  scm_init_throw ();
  scm_init_variable ();
  scm_init_vectors ();
  scm_init_weaks ();
  scm_init_eval ();

  /* Create standard ports from stdio files, if requested to do so.
   */

  if (in < 0)
    {
      scm_def_inp = SCM_BOOL_F;
    }
  else
    {
      scm_def_inp = scm_makefd (in, scm_fd_is_open | scm_close_fd_on_gc);
      safe_buffer_fd (in, 0, O_RDONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
    }

  if (out < 0)
    {
      scm_def_outp = SCM_BOOL_F;
    }
  else
    {
      scm_def_outp = scm_makefd (out, scm_fd_is_open | scm_close_fd_on_gc);
      safe_buffer_fd (out, 0, O_WRONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
    }

  if (err < 0)
    {
      scm_def_errp = SCM_BOOL_F;
    }
  else
    {
      scm_def_errp = scm_makefd (err, scm_fd_is_open | scm_close_fd_on_gc);
      safe_buffer_fd (err, 0, O_WRONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
    }

  scm_cur_inp = scm_def_inp;
  scm_cur_outp = scm_def_outp;
  scm_cur_errp = scm_def_errp;

  scm_appinit ();
  scm_progargs = scm_argv2scm (argc, (t_uchar **)argv);  
  scm_argc = argc;
  scm_argv = (t_uchar **)argv;
}



/****************************************************************
 * Entering Scheme
 *
 * This is the main entry point for the Systas Scheme interpreter.
 *
 */


/* scm_boot_systas
 *
 * Fire up Scheme.
 *
 * argc and argv are made into a list which is the return values 
 * of `(program-arguments)'.
 *
 * in, out, and err, if not NULL, become the standard ports.
 *	If NULL is passed, your "scm_appinit" should set up the 
 *      standard ports.
 *
 * boot_cmd is a string containing a Scheme expression to evaluate
 *      to get things rolling.
 *
 * result is returned a string containing a printed result of evaluating
 * 	the boot command.   
 *
 * the return value is:
 *	scm_boot_ok       - evaluation concluded normally
 *	scm_boot_error    - evaluation concluded with a Scheme error
 *	scm_boot_emem     - allocation error mallocing *result
 *	scm_boot_ereenter - scm_boot_systas was called re-entrantly, which is prohibited.
 */
int
scm_boot_systas (char ** result,
		 int argc, char ** argv,
		 int in, int out, int err,
		 char * boot_cmd)
{
  SCM_INTS_ENABLED;

  static int initialized = 0;
  static int live = 0;
  SCM_STACKITEM i;
  int setjmp_val;
  int stat;

  scm_mask_ints = 0;

  if (live)			/* This function is not re-entrant. */
    {
      return scm_boot_ereenter;
    }

  live = 1;

  scm_ints_disabled = 1;
  scm_block_gc = 1;

  if (initialized)
    {
      safe_buffer_fd (in, 0, O_RDONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
      safe_buffer_fd (out, 0, O_WRONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
      safe_buffer_fd (err, 0, O_WRONLY, vfdbuf_add_zero_byte | vfdbuf_auto_shift);
      scm_restart_stack (&i);
      scm_progargs = scm_argv2scm (argc, (t_uchar **)argv);  
      scm_argc = argc;
      scm_argv = (t_uchar **)argv;
    }
  else
    {
      scm_boot_inits (&i, in, out, err, argc, argv);
      initialized = 1;
    }

  scm_block_gc = 0;		/* permit the gc to run */
  /* ints still disabled */

  {
    SCM command;

    command = scm_makfromstr0 (boot_cmd);

    setjmp_val = setjmp (SCM_REGS (scm_rootcont)->jmpbuf);

    if (!setjmp_val)
      {
	SCM last;
	scm_init_signals ();

	{
	  SCM p;
	  SCM form;

	  p = scm_sys_make_string_port (SCM_MAKINUM (O_RDONLY), command);
	  if (!scm_is_port (p))
	    panic ("unable to create string port");
	  last = SCM_EOL;
	  while (1)
	    {
	      form = scm_read (p, SCM_BOOL_F, SCM_BOOL_F, SCM_EOL);
	      if (SCM_EOF_VAL == form)
		break;
	      last = scm_eval_x (form);
	    }

	}

	scm_restore_signals ();
	/* This tick gives any pending
	 * asyncs a chance to run.  This must be done after
	 * the call to scm_restore_signals.
	 */
	SCM_ASYNC_TICK;

	scm_ints_disabled = 1;	/* Hopefully redundant but just to be sure. */

	{
	  SCM str_answer;

	  str_answer = scm_sys_write_to_string (last, SCM_BOOL_F);
	  if (!scm_is_string (str_answer))
	    panic ("unable to write result to string");
	  *result = (char *)malloc (1 + SCM_RO_LENGTH (str_answer));
	  if (!*result)
	    stat = scm_boot_emem;
	  else
	    {
	      mem_move ((t_uchar *)*result,
			SCM_RO_UCHARS (str_answer),
			SCM_RO_LENGTH (str_answer));
	      (*result)[SCM_RO_LENGTH (str_answer)] = 0;
	      stat = scm_boot_ok;
	    }
	}
      }
    else
      {
	/* This is reached if an unhandled throw terminated Scheme.
	 * Such an occurence should be extremely unlikely -- it indicates
	 * a programming error in the boot code.
	 *
	 * Details of the bogus exception are stored in scm_exitval even 
	 * though that isn't currently reflected in the return value.
	 */

	scm_restore_signals ();
	/* This tick gives any pending
	 * asyncs a chance to run.  This must be done after
	 * the call to scm_restore_signals.
	 *
	 * Note that an unhandled exception during signal handling
	 * will put as back at the call to scm_restore_signals immediately
	 * preceeding.   A sufficiently bogus signal handler could
	 * conceivably cause an infinite loop here.
	 */
	SCM_ASYNC_TICK;

	scm_ints_disabled = 1;	/* Hopefully redundant but just to be sure. */

	{
	  SCM str_answer;

	  str_answer = scm_sys_write_to_string (scm_exitval, SCM_BOOL_F);
	  if (!scm_is_string (str_answer))
	    panic ("unable to write result to string");
	  *result = (char *)malloc (1 + SCM_LENGTH (str_answer));
	  if (!*result)
	    stat = scm_boot_emem;
	  else
	    {
	      mem_move (*result, SCM_RO_CHARS (str_answer), SCM_RO_LENGTH (str_answer));
	      (*result)[SCM_LENGTH (str_answer)] = 0;
	      stat = scm_boot_error;
	    }
	}
      }
  }

  scm_ints_disabled = 1;	/* Hopefully redundant but just to be sure. */
  scm_block_gc = 1;
  live = 0;
  return stat;
}



/*s
 * program-arguments
 * 
 * Return a list of the command line arguments to the process.
 */
SCM_PROC(s_program_arguments, "program-arguments", 0, 0, 0, scm_program_arguments);
SCM 
scm_program_arguments (void)
{
  SCM_INTS_UNKNOWN;

  return scm_progargs;
}



void
scm_init_init (void)
{
#include "systas/libsystas/init.x"
}

