/* dynwind.c - scheme dynamic-wind
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
#include <setjmp.h>
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/numbers.h"


/************************************************************************
 *(h0 "Dynamic Wind")
 *
 * Dynamic wind is provided to help you write robust programs.
 * Sometimes it is desirable to write a program in three parts:
 *
 *	(let ((answer #f))
 *	   (do-this-going-in)
 *	   (set! answer (do-the-main-thing))
 *	   (do-this-on-the-way-out)
 *	   answer)
 *
 * A program written like that is not robust because if
 * `do-the-main-thing' encounters an error or exits non-locally,
 * `do-this-on-the-way-out' is never reached.  `dynamic-wind' is for
 * writing programs like that in a robust manner:
 *
 *
 *	(dynamic-wind do-this-going-in do-the-main-thing do-this-going-out) 
 * 
 * Using `dynamic-wind', if execution of `(do-this-going-in)'
 * completes, `(do-this-going-out)' will be invoked when
 * `(do-the-main-thing)' exits, whether it exits normally, or because
 * of an exception, or by calling a continuation captured outside of
 * `(do-the-main-thing)'. 
 */


/*(c dynamic-wind)
 * (dynamic-wind in-guard thunk out-guard)
 *
 * All three arguments must be 0-argument procedures.
 * 
 * `in-guard' is called, then `thunk', then `out-guard'.
 * 
 * If, at any time during the execution of `thunk', the continuation
 * of the `dynamic-wind' expression is escaped non-locally (because of
 * an exception or a call to a continuation), `out-guard' is called.
 * If that same continuation is re-entered, `in-guard' is called.
 * 
 * Thus it is possible that `in-guard' and `out-guard' may each be
 * called more than once.  If several calls to `dynamic-wind' are
 * nested, the out-guards for the inner-most call occurs first, and
 * out-guards are called in order with the outer-most guard called
 * last.  If the continuation of the dynamic-wind is re-entered,
 * `in-guard' is called.  In this case, the outermost in-guard is
 * called first, followed by successively nested in-guards.
 * 
 * Here is an example that illustrates the use of `dynamic-wind' to
 * implement a kind of dynamic binding.  This example also shows how
 * `dynamic-wind' and `call-with-current-continuation' interact.
 * `dynamic-wind' interacts similarly with `throw', `handle', and
 * `error'.
 * 
 * 	(define x 'normal-binding)
 * 	=> x
 * 	
 * 	(define a-cont #f)
 * 	
 * 	(set! a-cont
 * 	  (call-with-current-continuation
 * 	   (lambda (escape)
 * 	     (let ((old-x x))
 * 	       (dynamic-wind
 * 	        ;; in-guard:
 * 	        ;;
 * 	        (lambda () (set! x 'special-binding))
 * 	
 * 	        ;; thunk
 * 	        ;;
 * 	        (lambda ()
 * 	           (display x) (newline)
 * 	           (let ((ret-val
 * 	                    (call-with-current-continuation escape)))
 * 	             (display x) (newline)
 * 	             ret-val))
 * 	
 * 	        ;; out-guard:
 * 	        ;;
 * 	        (lambda () (set! x old-x)))))))
 * 	
 * 	;; Prints:
 * 	;;
 * 	special-binding
 * 	
 * 	;; Evaluates to:
 * 	;;
 * 	=> #<continuation ...>
 * 	
 * 	;; After evaluation, x is back to its original value:
 * 	;;
 * 	x
 * 	=> normal-binding
 * 	
 * 	;; We captured a continuation inside of the dynamic wind.
 * 	;; Calling that continuation will cause the in-guard
 * 	;; to be re-executed, and leaving it a second time will
 * 	;; cause the out-guard to be re-executed.
 * 	;;
 * 	(a-cont 'foo)
 * 	
 * 	;; Prints:
 * 	;;
 * 	special-binding
 * 	
 * 	;; Evaluates to:
 * 	;;
 * 	=> foo
 *
 * The guard procedures are called outside of the scope of the
 * `dynamic-wind'.  For example, if the `in-guard' exits non-locally,
 * this does not cause the `out-guard' to be called.
 * 
 * Dynamic wind guard procedures are called with interrupts masked.
 * That way they are protected from non-local exits from async
 * handlers.  Were this not the case, an ill-timed interrupt could
 * exit non-locally causing the in-guard to run without ever running
 * the out-guard, or causing the in-guard to run without running to
 * completion, or causing the out-guard to run twice without running
 * the in-guard in between.
 *
 */
SCM_PROC(s_dynamic_wind, "dynamic-wind", 3, 0, 0, scm_dynamic_wind);
SCM 
scm_dynamic_wind (SCM thunk1, SCM thunk2, SCM thunk3)
{
  SCM_INTS_ENABLED;
  SCM ans;
  
  {
    SCM_CATCH_LOCALS;
    /* An interrupt that exits non-locally could screw us in this
     * block -- we might wind up running only part of the in-guard; we
     * might wind up running the in-guard but not the out-guard
     * (because the out-guard never made it to the wind-chain. The
     * in-guard can't protect itself against the latter case: there
     * would still be a small window of opportunity between when the
     * in-guard finishes and the out-guard is added to the chain. 
     *
     * So we mask interrupts.
     *
     * Other kinds of non-local exit can also cause the in-guard
     * problems, but it can protect itself against those by not doing
     * anything that will cause a non-local exit.
     */
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
	scm_apply3 (thunk1, SCM_EOL, SCM_EOL, 0);
	SCM_CATCH_BODY_END;
      }
    /* We must add the guards to the wind chain after running the in-guard
     * so that, if the in-guard fails to complete, the guards are not on the 
     * wind chain.
     */
    scm_dynwinds = scm_acons (SCM_BOOL_F, scm_cons (thunk1, thunk3), scm_dynwinds);
    SCM_UNMASK_INTS;
  }
  ans = scm_apply3 (thunk2, SCM_EOL, SCM_EOL, 0);
  {
    SCM_CATCH_LOCALS;
    /* An interrupt that exits non-locally could screw us in this
     * block -- we might wind up running the out-guard more than once.
     * The out-guard can't protect itself against this; there would
     * still be small windows of opportunity between when the outguard
     * starts and when it masks interrupts and between when the
     * outguard unmasks interrupts and the outguard is removed from
     * the dynamic wind chain.  
     *
     * So we mask interrupts.
     *
     * Other kinds of non-local exit can also cause the out-guard
     * problems, but it can protect itself against those by not doing
     * anything that will cause a non-local exit.
     */
    SCM_MASK_INTS;
    /* We must remove the guards to the wind chain before running the out-guard
     * so that, if the out-guard exits non-locally, the guards are not on the 
     * wind chain.
     */
    scm_dynwinds = SCM_CDR (scm_dynwinds);
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
	scm_apply3 (thunk3, SCM_EOL, SCM_EOL, 0);
	SCM_CATCH_BODY_END;
      }
    SCM_UNMASK_INTS;
  }
  return ans;
}

/*(c symmetric-wind)
 * (symmetric-wind protect thunk)
 * 
 * This is simply:
 * 
 * 	(define (symmetric-wind protect thunk)
 *	  (dynamic-wind protect thunk protect))
 */




struct c_wind_desc
{
  void (*guard)(int direction, void *);
  void * closure;
};


/*c scm_c_dynamic_wind)
 * SCM scm_c_dynamic_wind (void (*guard)(int direction, void *),
 *                         SCM (*body)(void *),
 *                         void * closure);
 * 
 * Call "body(closure)"  between calls "guard (1,closure)" and 
 * "guard (0, closure)".   Store "guard" on the dynamic wind
 * chain in the manner of `dynamic-wind'.
 *
 * "guard" is called with interrupts masked.  See `scm_dynamic_wind'.
 * 
 */
SCM
scm_c_dynamic_wind (void (*guard)(int direction, void *),
		    SCM (*body)(void *),
		    void * closure)
{
  SCM guard_string;
  struct c_wind_desc * desc;
  SCM answer;

  guard_string = scm_makstr (sizeof (*desc));
  SCM_DEFER_INTS;
  desc = (struct c_wind_desc *)SCM_STRING_UCHARS (guard_string);
  desc->guard = guard;
  desc->closure = closure;
  SCM_ALLOW_INTS;
  {
    SCM_CATCH_LOCALS;
    /* An interrupt that exits non-locally could screw us in this
     * block -- we might wind up running only part of the in-guard; we
     * might wind up running the in-guard but not the out-guard
     * (because the out-guard never made it to the wind-chain. The
     * in-guard can't protect itself against the latter case: there
     * would still be a small window of opportunity between when the
     * in-guard finishes and the out-guard is added to the chain. 
     *
     * So we mask interrupts.
     *
     * Other kinds of non-local exit can also cause the in-guard
     * problems, but it can protect itself against those by not doing
     * anything that will cause a non-local exit.
     */
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
	guard (1, closure);
	SCM_CATCH_BODY_END;
      }
    /* We must add the guards to the wind chain after running the
     * in-guard so that, if the in-guard fails to complete, the guards
     * are not on the wind chain.
     */
    scm_dynwinds = scm_acons (SCM_BOOL_F, guard_string, scm_dynwinds);
    SCM_UNMASK_INTS;
  }
  answer = body (closure);
  {
    SCM_CATCH_LOCALS;
    /* An interrupt that exits non-locally could screw us in this
     * block -- we might wind up running the out-guard more than once.
     * The out-guard can't protect itself against this; there would
     * still be small windows of opportunity between when the outguard
     * starts and when it masks interrupts and between when the
     * outguard unmasks interrupts and the outguard is removed from
     * the dynamic wind chain.  
     *
     * So we mask interrupts.
     *
     * Other kinds of non-local exit can also cause the out-guard
     * problems, but it can protect itself against those by not doing
     * anything that will cause a non-local exit.
     */
    SCM_MASK_INTS;
    /* We must remove the guards to the wind chain before running the
     * out-guard so that, if the out-guard exits non-locally, the
     * guards are not on the wind chain.
     */
    scm_dynwinds = SCM_CDR (scm_dynwinds);
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
	guard (0, closure);
	SCM_CATCH_BODY_END;
      }
    SCM_UNMASK_INTS;
  }
  return answer;
}


/* scm_dowinds
 * 
 * This function is called when control is about to be passed
 * to a context in which the dynamic wind chain is `to'.  
 *
 * `delta' is equal to `(- (length dynwinds) (length to))'
 *
 * This procedure invokes the exit procedures for all dynamic
 * wind frames on the chain up to the common tail of `dynwinds'
 * and `to'.  Then it invokes the entry procedures for all
 * dynamic wind procedures on the portion of `to' that is 
 * not shared with `dynwinds'.
 */
void 
scm_dowinds (SCM to, long delta)
{
  SCM_INTS_ENABLED;

  while (scm_dynwinds != to)
    {
      if (0 > delta)
	{
	  SCM wind_elt;
	  SCM wind_key;
	  SCM wind_val;

	  /* If `to' is longer than dynwinds, unwind to CDR(to) and
	   * then, if CAR(to) is a dynamic-wind frame, invoke its
	   * entry procedure.  Then we are done.
	   *
	   * Yes, this code does belong inside of the loop.  It is run
	   * at most once, but might be run after several iterations.
	   */
	  scm_dowinds (SCM_CDR (to), 1 + delta);
	  wind_elt = SCM_CAR (to);
	  wind_key = SCM_CAR (wind_elt);
	  wind_val = SCM_CDR (wind_elt);
	  {
	    /* An interrupt that exits non-locally could screw us in
	     * this block -- we might wind up running only part of
	     * the in-guard; we might wind up running the in-guard
	     * but not the out-guard (because the out-guard never
	     * made it to the wind-chain. The in-guard can't protect
	     * itself against the latter case: there would still be
	     * a small window of opportunity between when the
	     * in-guard finishes and the out-guard is added to the
	     * chain. 
	     *
	     * So we mask interrupts.
	     *
	     * Other kinds of non-local exit can also cause the
	     * in-guard problems, but it can protect itself against
	     * those by not doing anything that will cause a
	     * non-local exit.
	     */
	    SCM_MASK_INTS;
	    if ((SCM_BOOL_F == wind_key) && !SCM_IS_IMMEDIATE (wind_val))
	      {
		SCM_CATCH_LOCALS;
		SCM_CATCH_INIT (SCM_BOOL_T);
		if (SCM_CATCH_SETJMP())
		  {
		    SCM_UNWIND;
		    SCM_UNMASK_INTS;
		    scm_throw (SCM_THROW_TAG, SCM_THROW_ARGS);
		  }
		else if (SCM_CONSP (wind_val))
		  {
		    SCM_CATCH_BODY_BEGIN;
		    scm_apply3 (SCM_CAR (wind_val), SCM_EOL, SCM_EOL, 0);
		    SCM_CATCH_BODY_END;
		  }
		else if (scm_tc7_string == SCM_TYP7 (wind_val))
		  {
		    struct c_wind_desc * desc;
		    desc = (struct c_wind_desc *)SCM_STRING_UCHARS (wind_val);
		    SCM_CATCH_BODY_BEGIN;
		    desc->guard (1, desc->closure);
		    SCM_CATCH_BODY_END;
		  }
	      }
	    /* We must add the guards to the wind chain after running
	     * the in-guard so that, if the in-guard fails to
	     * complete, the guards are not on the wind chain.
	     */
	    SCM_UNMASK_INTS;
	    scm_dynwinds = to;
	  }
	}
      else
	{
	  SCM wind_elt;
	  SCM wind_key;
	  SCM wind_val;

	  /* If `to' is not longer than `dynwinds',
	   * then pop one frame off of `dynwinds'.  If
	   * it is a dynamic-wind frame, then call its
	   * exit procedure.  Then loop.
	   */
	  wind_elt = SCM_CAR (scm_dynwinds);
	  wind_key = SCM_CAR (wind_elt);
	  wind_val = SCM_CDR (wind_elt);
	  {
	    /* An interrupt that exits non-locally could screw us in
	     * this block -- we might wind up running the out-guard
	     * more than once.  The out-guard can't protect itself
	     * against this; there would still be small windows of
	     * opportunity between when the out-guard starts and when
	     * it masks interrupts and between when the out-guard
	     * unmasks interrupts and the out-guard is removed from
	     * the dynamic wind chain.  
	     *
	     * So we mask interrupts.
	     *
	     * Other kinds of non-local exit can also cause the
	     * out-guard problems, but it can protect itself against
	     * those by not doing anything that will cause a
	     * non-local exit.
	     */
	    SCM_MASK_INTS;
	    /* We must remove the guards to the wind chain before
	     * running the out-guard so that, if the out-guard exits
	     * non-locally, the guards are not on the wind chain.
	     */
	    scm_dynwinds = SCM_CDR (scm_dynwinds);
	    if ((SCM_BOOL_F == wind_key) && !SCM_IS_IMMEDIATE (wind_val))
	      {
		SCM_CATCH_LOCALS;
		SCM_CATCH_INIT (SCM_BOOL_T);
		if (SCM_CATCH_SETJMP())
		  {
		    SCM_UNWIND;
		    SCM_UNMASK_INTS;
		    scm_throw (SCM_THROW_TAG, SCM_THROW_ARGS);
		  }
		else if (SCM_CONSP (wind_val))
		  {
		    SCM_CATCH_BODY_BEGIN;
		    scm_apply3 (SCM_CDR (wind_val), SCM_EOL, SCM_EOL, 0);
		    SCM_CATCH_BODY_END;
		  }
		else if (scm_tc7_string == SCM_TYP7 (wind_val))
		  {
		    struct c_wind_desc * desc;
		    desc = (struct c_wind_desc *)SCM_STRING_UCHARS (wind_val);
		    SCM_CATCH_BODY_BEGIN;
		    desc->guard (0, desc->closure);
		    SCM_CATCH_BODY_END;
		  }
	      }
	    SCM_UNMASK_INTS;
	  }
	  --delta;
	}
    }
}



void
scm_init_dynwind (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/dynwind.x"
}



/************************************************************************
 * Dynamic Wind Internals
 *
 * At all times, the interpreter maintains an association list called
 * the wind chain in the variable `scm_dynwinds'.  The wind chain is
 * used for two purposes: dynamic-wind and exception handling. 
 * (See "throw.c")
 *
 * Ordinary dynamic-wind frames entries on the wind chain have
 * an association key #f and an association value which is a cons
 * pair.  The car of the value holds a dynamic wind entry procedure
 * and the cdr of the value holds a dynamic wind exit procedure.
 *
 * Entries on the wind chain which have an association key #f and an
 * association value which is a string (scm_tc7_string) are
 * c_dynamic_wind frames.
 *
 * Other entries on the wind chain are exception handlers (See "Catch
 * and Throw Internals" in throw.c)
 *
 * Dynamic wind guard procedures (`do-this-going-in' and
 * `do-this-going-out' in the example) are called with interrupts
 * masked.  That way they are protected from non-local exits from
 * async handlers.  Otherwise, an ill-timed interrupt could exit
 * non-locally causing the in-guard to run without ever running the
 * out-guard, or causing the in-guard to run without running to
 * completion, or causing the out-guard to run twice without running
 * the in-guard in between.  
 *
 * The interaction between non-local exits, signal handlers, and
 * dynamic-wind is subtle and for many years the implementation was
 * buggy -- an ill-timed SIGINT could thwart dynamic-wind, for
 * example.  Because the time windows in which an errant interrupt
 * could cause problems are so small, this bug would almost certainly
 * not be caught by almost any kind of empirical testing imaginable,
 * yet it is quite serious!  How it was found and fixed is a long
 * story but the moral of the story is the most important part:
 * verified proofs, in addition to testing, are an important part of
 * making programs "safe-for-space".  (This program is not yet
 * "safe-for-space".)
 */


/************************************************************************
 *(h1 "Rationale -- Dynamic Wind")
 * 
 * Systas follows R5RS for `dynamic-wind'.
 * 
 * There is some (justified) controversy over this feature and its
 * interaction with continuations in standard Scheme.
 * 
 * Very likely, this function will be modified in future releases.
 * 
 */
