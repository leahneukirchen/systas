/* async.c - handling asynchronous events
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <signal.h>
#include <unistd.h>
#include <unistd.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/async.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/read-print.h"


/****************************************************************
 *(h0 "Asynchronous Events")
 *
 * A process may be called upon to respond to events which occur
 * asynchronously with respect to all other activities of the process.
 * Unix signals are an example: a signal may be delivered to a process
 * at any time.
 * 
 * Systas Scheme is made of up several subsystems such as the
 * interpreter, the built-in functions, and the garbage collector that
 * can not interrupt one another arbitrarily.  For example, during
 * portions of garbage collection, primitive functions can not be
 * called.  During some portions of the built-ins, neither the garbage
 * collector or the evaluator can be called.  These restrictions raise
 * the question of just how asynchronous events are handled.  If an
 * asynchronous event arrives while a built-in function is busy, then
 * it is not safe to call the interpreter to handle the asynchronous
 * event.  Scheme programs may define additional conditions, such as
 * when making updates to a complex data structure, during which
 * asynchronous event handling should be deferred.
 * 
 * A data-type called an "async object" helps to reconcile this
 * situation.  An async object represents a particular kind of
 * asynchronous event.  Every async object corresponds to a procedure,
 * the "async handler", that will be called whenever that asynchronous
 * event has occured.  When the event occurs, the async object is
 * specially marked.  The mark instructs the system to call the async
 * later, when it is safe.  In other words, async objects provide
 * programmers with a way to schedule a call to a handler procedure
 * without necessarily making that call immediately.
 * 
 * Calls to async handlers are never nested.  While one is running, no
 * other handler will be called.  If an async is already marked,
 * marking it again has no further effect.
 * 
 * There are two flavors of async objects, "system asyncs" and "user
 * asyncs".  Handlers for system asyncs are called by the interpreter
 * as soon as it is safe to do so and take precedence over ordinary
 * evaluation Handlers for user asyncs are called only when explicitly
 * requested by a running program.
 *
 * To prevent all system asyncs from running, use the procedure
 * `mask-interrupts'.  To permit system asyncs to run after masking
 * them, use `unmask-interrupts'.
 */
/*(menu)
 */

/****************************************************************
 *(h1 "Signal Handlers")
 *
 * There is a system async for every kind of unix signal known to the
 * interpreter.  When a signal arrives, the corresponding async is
 * marked.
 *
 * By default, the async functions for signals throw an exception of
 * type `signal': For example:
 *
 *	(throw 'signal 'SIGINT)	// This happens when SIGINT arrives
 *
 * You can override the default behavior by defining a function named
 * after the signal (with a lowercase name).  For example:
 *
 *	(define (sigint) (display "sigint happens\n"))
 *
 * There are two "pseudo-signals" -- these work like unix signals but
 * are generated internally and do not correspond to any actual unix
 * signals.  These are:
 *
 *	SIGGC		;; Signalled when garbage collection finishes
 *	SIGTICK		;; Signalled when the tick counter expires.
 *
 * The "tick counter" is a count-down timer that is decremented
 * "frequently" by the interpreter.  To set the tick counter, use the
 * function `set-tick-rate'.
 */

/****************************************************************
 * Async Control Variables
 * 
 */


/* scm_async_clock
 *
 * Countdown at each SCM_ASYNC_TICK.  When 0 is reached, call
 * `scm_async_click'.
 *
 * Use: SCM_ASYNC_TICK
 *  or: SCM_(RE)ALLOW_INTS
 *
 * Set automatically.
 *
 * Set to 1 when a system async is marked for execution.
 *
 * Otherwise, set to the # of async ticks until the next tick signal.
 */
unsigned int scm_async_clock = 1 << 16;


/* scm_mask_ints
 *
 * Not safe to run new asyncs (one may be running).
 *
 * Therefore: no unexpected non-local exits will occur.  But, protect
 * against non-local exits due to errors.
 *
 * Use: scm_(un)mask_interrupts
 */
unsigned int scm_mask_ints = 0;


/* scm_saved_async_clock
 * 
 * When an asynchronous signal arrives, its async object is marked
 * and `scm_async_clock' is set to 1 to cause the handler to be 
 * called at the next async tick.
 *
 * The old value of `scm_async_clock' is saved here, and restored
 * after `scm_async_click'.
 */
static unsigned int scm_saved_async_clock = 0;


/* scm_tick_clock
 * 
 * When `scm_async_clock' has been decremented this many times,
 * generate an `scm_tick_signal'.
 */
static unsigned int scm_tick_clock = 0;


/* scm_ints_disabled
 *
 * Not safe to GC.
 * Not safe to exit non-locally.
 * 
 * Therefore: not safe to eval (in general).
 * 
 * Therefore: not safe to handle signals.
 *
 * Use: SCM_(RE){ALLOW,DISALLOW}_INTS(_ONLY)
 */
unsigned int scm_ints_disabled = 1;

/* signal_handlers_installed
 * 
 * When not-0, Scheme handlers are in place for unix signals.
 * The Scheme handlers mark the corresponding async object,
 * copy `scm_async_clock' to `scm_saved_async_clock', and set
 * `scm_async_clock' to 1.
 */
static int signal_handlers_installed = 0;

/* system_signal_asyncs
 * 
 * An array, indexed by `enum scm_signals', mapping signals
 * to async objects. 
 */
static SCM system_signal_asyncs[scm_num_sigs];


/****************************************************************
 * Signals Variables
 */ 

#ifndef SIGHUP
#define SIGHUP 0
#endif
#ifndef SIGCHLD
#define SIGCHLD 0
#endif
#ifndef SIGINT
#define SIGINT 0
#endif
#ifndef SIGFPE
#define SIGFPE 0
#endif
#ifndef SIGBUS
#define SIGBUS 0
#endif
#ifndef SIGSEGV
#define SIGSEGV 0
#endif
#ifndef SIGALRM
#define SIGALRM 0
#endif
#ifndef SIGIO
#define SIGIO 0
#endif

/* These must be in the opposite order of 
 * delivery priortity. 
 */
#define UNIX_SIGNALS \
	UNIX_SIGNAL (fpe, SIGFPE, SIG_IGN) TERM \
	UNIX_SIGNAL (int, SIGINT, SIG_IGN) TERM \
	UNIX_SIGNAL (chld, SIGCHLD, SIG_DFL) TERM \
	UNIX_SIGNAL (io, SIGIO, SIG_DFL) TERM \
	UNIX_SIGNAL (alrm, SIGALRM, SIG_DFL) TERM \
	UNIX_SIGNAL (hup, SIGHUP, SIG_DFL) TERM \
	UNIX_SIGNAL (segv, SIGSEGV, SIG_DFL) TERM \
	UNIX_SIGNAL (bus, SIGBUS, SIG_DFL) TERM


#undef UNIX_SIGNAL
#undef TERM
#define TERM ;
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	SCM_GLOBAL (scm_sig ## STUB ## _variable, "sig" #STUB)
UNIX_SIGNALS

SCM_GLOBAL (scm_siggc, "siggc");	/* pseudo-signal (GC finished) */
SCM_GLOBAL (scm_sigtick, "sigtick");	/* pseudo-signal (TICK timer expired) */

SCM_SYMBOL (s_signal, "signal"); 	/* Exception type of an unhandled signal */


/****************************************************************
 * Async Small Object Functions
 */

struct scm_async
{
  int got_it;			/* needs to be delivered? */
  SCM thunk;
};

/* Access to the "struct scm_async" of X.
 */
#define SCM_ASYNC(X) 	((struct scm_async *)SCM_CDR (X))


static int
print_async (SCM exp, SCM port, int writing __attribute__ ((unused)))
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<async ");
  scm_intprint (exp, 16, port);
  scm_port_putc (&errn, port, '>');
  return 1;
}


static SCM
mark_async (SCM obj)
{
  SCM_INTS_UNKNOWN;
  struct scm_async * it;

  if (SCM_GC8MARKP (obj))
    return SCM_BOOL_F;
  SCM_SETGC8MARK (obj);
  it = SCM_ASYNC (obj);
  return it->thunk;
}


static size_t
free_async (SCM obj)
{
  SCM_INTS_DISABLED;
  struct scm_async * it;

  it = SCM_ASYNC (obj);
  scm_must_free ((char *)it);
  return (sizeof (*it));
}

static scm_small_object_functions  async_smob =
{
  mark_async,
  free_async,
  print_async,
  0
};

static long scm_tc16_async;



/****************************************************************
 * Async Scheduling Functions
 */


/* asyncs_pending
 * 
 * Return 1 if there are any system asyncs pending, 0 otherwise.
 */
static int
asyncs_pending (void)
{
  SCM_INTS_DISABLED;
  SCM pos;

  pos = scm_asyncs;
  while (pos != SCM_EOL)
    {
      SCM a;
      struct scm_async * it;
      a = SCM_CAR (pos);
      it = SCM_ASYNC (a);
      if (it->got_it)
	return 1;
      pos = SCM_CDR (pos);
    }
  return 0;
}


/* async_mark
 * 
 * Mark an async ready for execution.
 */
static SCM
async_mark (SCM a)
{
  SCM_INTS_INDIFFERENT;
  struct scm_async * it;

  it = SCM_ASYNC (a);
  it->got_it = 1;
  return SCM_UNSPECIFIED;
}


/* system_async_mark
 * 
 * Mark an async ready for execution.
 * If scm_saved_async_clock is 0, copy scm_async_clock
 * to scm_saved_async_clock.  Set scm_async_clock to 1.
 */
static SCM
system_async_mark (SCM a)
{
  SCM_INTS_NESTED;
  struct scm_async * it;

  it = SCM_ASYNC (a);
  SCM_REDEFER_INTS;
  it->got_it = 1;
  if (!scm_saved_async_clock)
    scm_saved_async_clock = scm_async_clock;
  scm_async_clock = 1;
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


/* scm_async_click
 * 
 * This function is called when scm_async_clock is 1 and
 * should be decremented.   See the comment "System Async 
 * Internals".
 */
void
scm_async_click (void)
{
  SCM_INTS_ENABLED;
  int false_alarm;

  if (scm_mask_ints)
    return;

  false_alarm = 0;

  SCM_DEFER_INTS;
  if (scm_async_clock != 1)
    false_alarm = 1;
  else
    {
      if (scm_tick_clock && !scm_saved_async_clock)
	async_mark (system_signal_asyncs[scm_tick_signal]);
      
      if (scm_saved_async_clock && (scm_saved_async_clock < scm_tick_clock))
	{
	  scm_async_clock = scm_saved_async_clock;
	  scm_saved_async_clock = 0;
	}
      else if (scm_tick_clock)
	scm_async_clock = scm_tick_clock;
      else
	scm_async_clock = 1 << 16;
    }
  SCM_ALLOW_INTS_ONLY;

  if (false_alarm)
    return;
  {
    int x;
    do
      {
	scm_run_asyncs (scm_asyncs);
	SCM_DEFER_INTS;
	x = asyncs_pending ();
	SCM_ALLOW_INTS_ONLY;
      } while (x);
  }
}


/****************************************************************
 * Async Procedures
 */


/*(c async)
 * (async thunk)
 * 
 * Construct a new async object that calls `thunk' when executed.
 */
SCM_PROC(s_async, "async", 1, 0, 0, scm_async);
SCM
scm_async (SCM thunk)
{
  SCM it;
  struct scm_async * async;

  SCM_NEWCELL (it);
  SCM_DEFER_INTS;
  SCM_CDR (it) = SCM_EOL;
  async = (struct scm_async *)scm_must_malloc (sizeof (*async));
  async->got_it = 0;
  async->thunk = thunk;
  SCM_CDR (it) = (SCM)async;
  SCM_CAR (it) = (SCM)scm_tc16_async;
  SCM_ALLOW_INTS;
  return it;
}


/*(c system-async)
 * (system-async thunk)
 * 
 * Construct a new system async that calls `thunk' when ready to be
 * executed.  System asyncs are automatically invoked as soon after
 * being marked as possible, if they are marked using
 * `system-async-mark'.
 */
SCM_PROC(s_system_async, "system-async", 1, 0, 0, scm_system_async);
SCM 
scm_system_async (SCM thunk)
{
  SCM_INTS_ENABLED;
  SCM it;
  SCM list;

  it = scm_async (thunk);
  SCM_NEWCELL (list);
  SCM_DEFER_INTS;
  SCM_CAR (list) = it;
  SCM_CDR (list) = scm_asyncs;
  scm_asyncs = list;
  SCM_ALLOW_INTS;
  return it;
}


/*(c async-mark)
 * (async-mark async)
 * 
 * Mark `async' ready for execution.  
 */
SCM_PROC(s_async_mark, "async-mark", 1, 0, 0, scm_async_mark);
SCM
scm_async_mark (SCM a)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (scm_is_async (a), a, scm_arg1, s_async_mark);
  SCM_DEFER_INTS;
  answer = async_mark (a);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c system-async-mark)
 * (system-async-mark async)
 * 
 * Mark an `async' ready for execution and execute all pending system
 * asyncs as quickly as possible.
 */
SCM_PROC(s_system_async_mark, "system-async-mark", 1, 0, 0, scm_system_async_mark);
SCM
scm_system_async_mark (SCM a)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_async (a), a, scm_arg1, s_async_mark);
  return system_async_mark (a);
}


/*(c set-tick-rate)
 * (set-tick-rate n)
 * 
 * The `tick' timer is decremented frequently during the evaluation of
 * expressions.  When it reaches 0, a "tick signal" is generated which
 * causes the procedure bound to "TICK_SIGNAL" to be invoked.
 *
 * This procedure sets the tick timer to `n' -- `n' ticks must occur
 * before a tick signal is generated.  If `n' is 0, no tick signals
 * are generated.
 *
 * The previous value of the tick timer is returned.
 */
SCM_PROC(s_set_tick_timer, "set-tick-rate", 1, 0, 0, scm_set_tick_timer);
SCM
scm_set_tick_timer (SCM n)
{
  SCM_INTS_NESTED;
  unsigned int old_n;

  SCM_ASSERT (SCM_INUMP (n), n, scm_arg1, s_set_tick_timer);
  SCM_REDEFER_INTS;
  old_n = scm_tick_clock;
  scm_tick_clock = SCM_INUM (n);
  if (scm_tick_clock)
    scm_async_clock = scm_tick_clock;
  SCM_REALLOW_INTS;
  return SCM_MAKINUM (old_n);
}


/*(c run-asyncs)
 * (run-asyncs list-of-asyncs)
 * 
 * Execute the thunks associated with any marked asyncs in
 * `list-of-asyncs'.  Clear the marks of those asyncs.  If interrupts
 * are currently masked, return without doing anything.
 */
SCM_PROC(s_run_asyncs, "run-asyncs", 1, 0, 0, scm_run_asyncs);
SCM
scm_run_asyncs (SCM list_of_a)
{
  SCM_INTS_ENABLED;
  SCM pos;

  if (scm_mask_ints)
    return SCM_BOOL_F;
  pos = list_of_a;
  while (pos != SCM_EOL)
    {
      SCM a;
      struct scm_async * it;
      if (!(!SCM_IS_IMMEDIATE (pos) && SCM_CONSP (pos)))
	break;
      a = SCM_CAR (pos);
      if (scm_is_async (a))
	{
	  SCM_CATCH_LOCALS;
	  
	  it = SCM_ASYNC (a);

	  SCM_MASK_INTS;
	  SCM_CATCH_INIT (SCM_BOOL_T);
	  if (SCM_CATCH_SETJMP())
	    {
	      SCM_UNWIND;
	      SCM_UNMASK_INTS;
	      scm_throw (SCM_THROW_TAG, SCM_THROW_ARGS);
	    }
	  else
	    {
	      SCM_CATCH_BODY_BEGIN;
	      if (it->got_it)
		{
		  it->got_it = 0;
		  scm_apply3 (it->thunk, SCM_EOL, SCM_EOL, 0);
		}
	      SCM_CATCH_BODY_END;
	      SCM_UNMASK_INTS;
	    }
	}
      pos = SCM_CDR (pos);
    }
  return SCM_BOOL_T;
}


/*(c mask-interrupts)
 * (mask-interrupts)
 * 
 * Block the execution of asyncs by incrementing the interrupt mask
 * counter.  See `unmask-interrupts'.
 *
 * Return the old value of the interrupt mask counter (an integer).
 */
SCM_PROC(s_mask_interrupts, "mask-interrupts", 0, 0, 0, scm_mask_interrupts);
SCM
scm_mask_interrupts (void)
{
  SCM_INTS_INDIFFERENT;
  int was;

  SCM_DEFER_INTS;
  was = scm_mask_ints;
  SCM_MASK_INTS;
  SCM_ALLOW_INTS;
  return scm_long2num ((long)was);
}


/*(c unmask-interrupts)
 * (unmask-interrupts)
 * 
 * Decrement the interrupt mask variable if it is greater than 0.
 * When the interrupt mask variable is 0, interrupts are permitted.
 *
 * Return the old value of the interrupt mask counter (an integer).
 */
SCM_PROC(s_unmask_interrupts, "unmask-interrupts", 0, 0, 0, scm_unmask_interrupts);
SCM
scm_unmask_interrupts (void)
{
  SCM_INTS_INDIFFERENT;
  int was;

  SCM_DEFER_INTS;
  was = scm_mask_ints;
  SCM_UNMASK_INTS;
  SCM_ALLOW_INTS;
  return scm_long2num (was);
}

/*(c without-interrupts)
 * (without-interrupts thunk)
 * 
 * Simply:
 * 
 *  (dynamic-wind mask-interrupts
 * 		  thunk
 * 		  unmask-interrupts))
 */

/*(c with-interrupts)
 * (with-interrupts thunk)
 * 
 * Conceptually, this is approximately:
 * 
 *  (dynamic-wind unmask-interrupts
 * 		  thunk
 * 		  mask-interrupts))
 * 
 * but in fact, things are not that simple:
 * 
 *       ;; We can't write:
 *       ;;
 *       ;; 	(dynamic-wind unmask-interrupts thunk mask-interrupts)
 *       ;;
 *       ;; because `unmask-interrupts' can be exited by a exception 
 *       ;; thrown from an interrupt handler.  That would cause
 *       ;; `with-interrupts' to exit with interrupts unmasked.
 *       ;; 
 *       (dynamic-wind (lambda () #f)
 * 		       (lambda ()
 * 		         (dynamic-wind unmask-interrupts
 * 				       thunk
 * 				       (lambda () #f)))
 * 		       mask-interrupts)
 * 
 */



/*(c interrupts-masked?)
 * (interrupts-masked?)
 * 
 * Return `#t' if interrupts are currently blocked, `#f' otherwise.
 */
SCM_PROC (s_interrupts_masked_p, "interrupts-masked?", 0, 0, 0, scm_interrupts_masked_p);
SCM
scm_interrupts_masked_p (void)
{
  SCM_INTS_INDIFFERENT;

  return (scm_mask_ints ? SCM_BOOL_T : SCM_BOOL_F);
}



/****************************************************************
 * Asyncs and Signals
 */


/* scm_deliver_signal
 * 
 * Given a variable bound to a procedure invoke that procedure.
 *
 * If the variable is unbound, throw an exception of type 'signal.
 */
static void
scm_deliver_signal (SCM var)
{
  SCM_INTS_ENABLED;
  SCM handler;

  handler = SCM_CDR (var);
  if (!SCM_UNBNDP (handler) && (SCM_BOOL_F != handler))
    scm_apply3 (handler, SCM_EOL, SCM_EOL, 0);
  else
    scm_handle (s_signal, scm_listify (SCM_CAR (var), SCM_UNDEFINED));
}


/* scm_take_signal
 * 
 * This is a unix-style signal handler for signal `n'.
 * It marks the corresponding system async and returns.
 */
SCM
scm_take_signal (int n)
{
  SCM_INTS_INDIFFERENT;
  SCM ignored;

  if (!scm_ints_disabled)
    {
      SCM_NEWCELL (ignored);		/* In case we interrupted SCM_NEWCELL,
					 * throw out the possibly already allocated
					 * free cell.
					 */
    }
  system_async_mark (system_signal_asyncs[n]);
  if (!scm_ints_disabled)
    SCM_ASYNC_TICK;
  return SCM_BOOL_F;
}


/* scm_defer_exception
 * 
 * This is used to generate an exception during a Scheme critical
 * section (between SCM_DEFER_INTS and SCM_ALLOW_INTS).  It
 * records the exception tag and parameters and marks the system
 * async scm_exception_signal.
 */
void
scm_defer_exception (SCM tag, SCM args)
{
  SCM_INTS_DISABLED;

  scm_deferred_throw_tag = tag;
  scm_deferred_throw_args = args;
  system_async_mark (system_signal_asyncs[scm_exception_signal]);
}


/* scm_sys_exception_async_thunk
 * 
 * This is the async procedure used by the deferred-exception system
 * async.
 */
static SCM
scm_sys_exception_async_thunk (void)
{
  SCM_INTS_ENABLED;
  return scm_throw (scm_deferred_throw_tag, scm_deferred_throw_args);
}


/****************************************************************
 * Signal Thunks, Functions and Procedures.
 *
 * The functions `scm_sys_ ## STUB ## _async_thunk' are the async 
 * procedures of the asyncs associated with unix signals and 
 * pseudo-signals.
 *
 * The functions `scm_ ## STUB ## _signal_fn' are unix
 * signal handlers.
 *
 * The variables `scm_ ## STUB ## _signal_fn_override' are used to
 * prevent a particular handler from being installed.
 *
 * The variables `old ## STUB' hold the signal handlers that
 * were installed before the `scm_ ## STUB ## _signal_fn' functions.
 *
 */

#undef UNIX_SIGNAL
#undef TERM
#define TERM
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	int scm_ ## STUB ## _signal_fn_override = 0; \
	static void (* old ## STUB) (int); \
	static void \
	scm_ ## STUB ## _signal_fn (int sig __attribute__ ((unused))) \
	{ \
	  SCM_INTS_UNKNOWN; \
	  signal (SIGNAL, scm_ ## STUB ## _signal_fn); \
	  scm_take_signal (scm_ ## STUB ## _signal); \
	} \
	\
	\
	static SCM \
	scm_sys_ ## STUB ## _async_thunk (void) \
	{ \
	  SCM_INTS_ENABLED; \
	\
	  scm_deliver_signal (scm_sig ## STUB ## _variable); \
	  return SCM_BOOL_F; \
	}

UNIX_SIGNALS


/****************************************************************
 * Thunks for pseudo-signals:
 */

static SCM
scm_sys_gc_async_thunk (void)
{
  SCM_INTS_ENABLED;

  /* scm_deliver_signal (scm_siggc); */
  return SCM_BOOL_F;
}

static SCM
scm_sys_tick_async_thunk (void)
{
  SCM_INTS_ENABLED;

  scm_deliver_signal (scm_sigtick);
  return SCM_BOOL_F;
}


/****************************************************************
 * Installing and Removing Unix Signal Handlers
 */

#undef UNIX_SIGNAL
#undef TERM
#define TERM ;
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	old ## STUB = signal (SIGNAL, scm_ ## STUB ## _signal_fn)
void 
scm_init_signals (void)
{
  SCM_INTS_UNKNOWN;

  if (signal_handlers_installed)
    return;

  UNIX_SIGNALS;
  signal_handlers_installed = 1;
}


#undef UNIX_SIGNAL
#undef TERM
#define TERM ;
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	signal (SIGNAL, old ## STUB)
void
scm_restore_signals (void)
{
  SCM_INTS_UNKNOWN;

  if (!signal_handlers_installed)
    return;
  UNIX_SIGNALS;
  signal_handlers_installed = 0;
}


#undef UNIX_SIGNAL
#undef TERM
#define TERM ;
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	signal (SIGNAL, DEFAULT)
/*(c ignore-signals)
 * (ignore-signals)
 * 
 * Restore the default handlers for all unix signals handled by the
 * Scheme.
 *
 * This is intended for use after `fork()' and before `exec()'.
 */
SCM_PROC (s_ignore_signals, "ignore-signals", 0, 0, 0, scm_ignore_signals);
SCM
scm_ignore_signals (void)
{
  SCM_INTS_UNKNOWN;

  UNIX_SIGNALS;
  signal_handlers_installed = 0;
  return SCM_UNSPECIFIED;
}


/*(c unignore-signals)
 * (unignore-signals)
 * 
 * Restore the Scheme handlers for unix signals.
 *
 * This undoes the effect of `ignore-signals'.
 */
SCM_PROC (s_unignore_signals, "unignore-signals", 0, 0, 0, scm_unignore_signals);
SCM
scm_unignore_signals (void)
{
  SCM_INTS_UNKNOWN;
  scm_init_signals ();
  return SCM_UNSPECIFIED;
}



int
scm_is_async (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (scm_tc16_async == SCM_TYP16 (obj));
}


/****************************************************************
 * Initialization
 */
#undef UNIX_SIGNAL
#undef TERM
#define TERM ;
#define UNIX_SIGNAL(STUB, SIGNAL, DEFAULT) \
	a_thunk = scm_make_gsubr (#STUB "-thunk", 0, 0, 0, \
				  (SCM (*)())scm_sys_ ## STUB ## _async_thunk, 1); \
	system_signal_asyncs[scm_ ## STUB ## _signal] = scm_system_async (a_thunk)

void
scm_init_async (void)
{
  SCM_INTS_DISABLED;
  SCM a_thunk;

  scm_tc16_async = scm_newsmob (&async_smob);

  scm_asyncs = SCM_EOL;
  UNIX_SIGNALS;

  /* pseudo-signals */
  UNIX_SIGNAL (gc, 0, 0);
  UNIX_SIGNAL (tick, 0, 0);
  UNIX_SIGNAL (exception, 0, 0);
#include "systas/libsystas/async.x"
}




/************************************************************************
 * Asyncs Internals
 *
 * Asyncs are represented by a non-immediate type:
 *
 *  ..................scm_tc16_async .......struct scm_async *.......
 *
 * A "struct scm_async" is simply a procedure and a flag that says
 * whether or not the async has been marked.
 *
 * For every unix signal type, an async object is created.  These
 * are stored in the array "system_signal_asyncs" and GC protected
 * on the list "scm_asyncs".
 *
 * The list "scm_asyncs" contains all "system asyncs" -- all asyncs
 * that are automatically invoked.
 *
 * Several variables control the timing of the execution of system
 * asyncs:
 *
 *	scm_async_clock -- This is "frequently" decremented by the
 *			   the interpreter.  After the transition from
 *			   1 to 0, the list of system asyncs is
 *			   scanned and any that are marked are run.
 *			   The macro SCM_ASYNC_TICK attempts to decrement
 *			   this counter.
 *
 *	scm_mask_ints	-- While this is not 0, no system asyncs
 *			   are invoked and scm_async_clock does
 *			   not change value.
 *
 *	scm_saved_async_clock -- When a system async is becomes marked,
 *			   if this variable is 0, scm_async_clock is
 *			   copied to scm_saved_async_clock and scm_async_clock
 *			   is set to 1.  That causes a "nearly immediate"
 *			   scan of the system asyncs (on the transition of 
 *			   scm_async_clock from 1 to 0).  After that scan
 *			   is initated, scm_saved_async_clock is copied
 *			   back to scm_async_clock and scm_saved_async_clock
 *			   is reset to 0.
 *
 *	scm_tick_clock --  This is either 0, or the value to which to
 *			   reset the scm_async_clock when it reaches 0.
 *			   If it is 0, scm_async_clock is reset to a 
 *			   large number.
 *
 * Closely related is the variable:
 *
 *	scm_ints_disabled -- While this is not 0, the garbage collector
 *			   will not run and it is not safe to call eval
 *			   or cause a procedure to exit non-locally.
 *			   When this variable makes a transition to 0,
 *			   the macro SCM_TICK_CLOCK is invoked which
 *			   attempts to decrement scm_async_clock.
 *			   This variable is manipulated by the macros:
 *				SCM_DEFER_INTS
 *				SCM_REDEFER_INTS
 *				SCM_ALLOW_INTS
 *				SCM_REALLOW_INTS
 *				SCM_ALLOW_INTS_ONLY
 *			   See "scm.h".
 *
 *
 * For every type of unix signal or internally generated pseudo-signal
 * there is a system async.  That async invokes a handler procedure
 * that is bound to a variable named after the signal.
 */
/****************************************************************
 *st: Signal Handler Internals
 *
 * To propogate a unix signal to the interpreter, use the
 * function `scm_take_signal', passing it one of the signal
 * numbers defined in "async.h".  (See "enum scm_signals".)
 *
 * During interpreter initialization, scm_init_signals installs the
 * interpreter's handlers using `signal(2)'.  To restore the handlers
 * that were in place prior to that, use `scm_restore_signals'.  After
 * being restored, the interpreter's handlers can be re-installed
 * using `scm_init_signals'.
 *
 * To prevent the `scm_init_signals' from installing a particular
 * handler, set the corresponding override variable before `scm_init_signals'
 * is called (or after `scm_restore_signals' but before the next
 * call to `scm_init_signals'.  Override variables are named
 * after the scm handler functions:
 *
 *
 *	scm_restore_signals ();
 *	scm_int_signal_fn_override = 1;	// SIGINT no longer reaches Scheme
 *	scm_init_signals ();
 */



/************************************************************************
 *(h1 "Rationale -- Asynchronous Events")
 * 
 * Systas need asyncs for Posix signals, but their is a natural abstraction
 * to Posix signals, which I hope I've captured with this interface.
 * 
 */
