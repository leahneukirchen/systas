/* load.c - evaluating expressions from a file
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vfdbuf.h"
#include "systas/libsystas/load.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/boolean.h"


/************************************************************************
 *(h0 "Evaluating Expressions in Files")
 *
 */


/*(c load)
 * (load filename)
 * 
 * Try to load a file by searching the `load-path'.  Both `filename' and
 * `filename' with the various `scheme-file-suffixes' appended are
 * tried.
 * 
 * If the file can not be found, an error is signalled.
 * 
 * `load-verbosely' controls the printed output
 * of this procedure.
 */

/*(c load-with-path)
 * (load-with-path filename path)
 * 
 * Like `load', but searches the specified path instead of the
 * default path.  `path' is a list of directory names.
 */

/*(c load-verbosely :category variable)
 * load-verbosely
 * 
 * If not `#f', then print informative messages on the
 * current output port while loading files.
 */

/*(c load-path :category variable)
 * (load-path)
 * 
 * A list of directories to search for scheme source code.
 * 
 * The list consists of elements from the environment variable
 * "SCHEME_LOAD_PATH" (or '("./" "/") if that environment is not
 * defined) followed by the built-in load path.
 */

/*(c scheme-suffixes :category variable)
 * (scheme-suffixes)
 * 
 * A list of filename extensions that designate Scheme
 * source code.  Default value:
 * 
 * 	 '(".scm" ".ss"))
 */


/*(c parse-path)
 * (parse-path path-string)
 * 
 * Convert a string of the form:
 * 
 * 		"elt1:elt2:elt3..."
 * 
 * to a list of the form:
 * 
 * 		("elt1" "elt2" "elt3" ...)
 */

/*(c try-load)
 * (try-load filename)
 * 
 * Try to load the named file from the list of directories
 * bound to `load-path'.   If `load-path' is not defined,
 * try to load the named file from the current directory or,
 * if `filename' is absolute, from the root.
 * 
 * Return `#f' if the file could not be found.
 */

/*(c try-load-with-path)
 * (try-load-with-path filename path)
 * 
 * Try to load the named file from the indicated list of
 * directories.   
 * 
 * Return `#f' if the file could not be found.
 */




SCM_PROCEDURE (proc_low_level_try_load, "low-level-try-load");



struct try_load_state
{
  SCM port;
  SCM filename;
  SCM case_insensative_p;
  SCM sharp;
};

static void
swap_try_load_file (int direction, void * vstate)
{
  struct try_load_state * state;
  SCM tmp;

  state = (struct try_load_state *)vstate;
  tmp = scm_cur_loadp;
  scm_cur_loadp = state->port;
  state->port = tmp;
}


SCM 
do_try_load (void * vstate)
{
  SCM_INTS_ENABLED;
  struct try_load_state * state;
  SCM form;
  int c;
  int errn;

  state = (struct try_load_state *)vstate;

  SCM_DEFER_INTS;
  c = scm_port_getc (&errn, scm_cur_loadp);
  SCM_ALLOW_INTS;
  if (c != '#')
    {
      if (c >= 0)
	{
	  SCM_DEFER_INTS;
	  c = scm_port_ungetc (&errn, scm_cur_loadp, c);
	  SCM_ALLOW_INTS;
	  if (c < 0)
	    return scm_makerrno (errn);
	}
    }
  else
    while ((c != -1) && (c != '\n'))
      {
	SCM_DEFER_INTS;
	c = scm_port_getc (&errn, scm_cur_loadp);
	SCM_ALLOW_INTS;
      }

  while (1)
    {
      form = scm_read (scm_cur_loadp, state->case_insensative_p, state->sharp, SCM_EOL);
      if (SCM_EOF_VAL == form)
	break;
#if 0
      scm_write (form, SCM_UNDEFINED, SCM_UNDEFINED);
      scm_newline (scm_cur_outp);
      SCM_DEFER_INTS;
      scm_port_flush (&errn, scm_cur_outp);
      SCM_ALLOW_INTS;
#endif
      scm_eval_x (form);
    }
  return SCM_BOOL_T;
}

/************************************************************************
 *(h1 "Low Level File Loading")
 * 
 * 
 * 
 */


/*(c low-level-try-load)
 * (load-level-try-load filename 
 *			[:optional ignore-case?
 *				   sharp-reader])
 * 
 * Attempt to open `filename', read expressions from that file, and evaluate
 * those expressions.
 *
 * `ignore-case?' means to convert all symbol names to lower case as they 
 * are read.
 * 
 * `sharp-reader', if provided and not #f, is a procedure to call to read
 * expressions that begin `#' where the `#' is not followed a recognized 
 * character.  (See `read').
 *
 * If the file can not be opened this procedure silently returns `#f'.
 * 
 * If any other error occurs, an exception is generated.
 * 
 * If this procedure succeeds, `#t' is returned.
 */
SCM_PROC(s_low_level_try_load, "low-level-try-load", 1, 2, 0, scm_low_level_try_load);
SCM 
scm_low_level_try_load (SCM filename,
			SCM case_insensative_p,
			SCM sharp)
{
  SCM_INTS_ENABLED;
  struct try_load_state state;
  SCM answer;
  int errn;

  SCM_ASSERT (scm_is_ro_string (filename),
	      filename, scm_arg1, s_low_level_try_load);
  
  state.port = scm_sys_open (filename, SCM_INUM0, SCM_INUM0);
  state.filename = filename;
  state.case_insensative_p = case_insensative_p;
  state.sharp = sharp;
  if (!(!SCM_IS_IMMEDIATE (state.port) && SCM_FDP (state.port)))
    return SCM_BOOL_F;
  else
    {
      int fd;
      int ok;
      
      SCM_DEFER_INTS;
      fd = SCM_FD (state.port);
      ok = (0 <= vfdbuf_buffer_fd (&errn, fd, 0, 0, 1));
      SCM_ALLOW_INTS;
      if (!ok)
	{
	  scm_sys_close (state.port);
	  scm_throw (scm_makerrno (errn), scm_listify (proc_low_level_try_load, filename, case_insensative_p, sharp, SCM_UNDEFINED));
	}
    }

  answer = scm_c_dynamic_wind (swap_try_load_file, do_try_load, (void *)&state);
  scm_close_port (state.port);
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_low_level_try_load, filename, case_insensative_p, sharp, SCM_UNDEFINED));
  return answer;
}




void
scm_init_load (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/load.x"
}

