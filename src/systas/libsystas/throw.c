/* throw.c - scheme exception handling
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
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/throw.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/read-print.h"


/****************************************************************
 *(h0 "Catch and Throw")
 *
 * Exceptions are triggered by unusual events such as encountering
 * an error.  They interrupt the normal flow of control in a program.
 *
 * Exceptions may be caused deliberately by a programmer as a way
 * to organize some aspects of a program.
 *
 * Exceptions are caused by being "thrown".  When an exception is
 * thrown, an exception "type" must be specified, and any number 
 * of exception "parameters" may be specified.  Usually, but not
 * necessarily, the "type" is represented by a symbol.
 *
 *	(throw 'no-solution)	; throw an exception of type 'no-solution
 *
 *	(throw 'found-one 69)	; throw an exception of type 'found-one
 *				; with one parameter.
 *
 * A call to `throw' does not return.  To throw an exception which
 * might (but does not necessarily return), use `handle':
 *
 *	(if (number? x)
 *	    (compute-the-usual-way x)
 *	    (handle 'not-a-number x))
 *
 * The key passed to `throw' or `handle' must not be `#f' for reasons that
 * will be explained below.
 *
 * The procedure `catch' is used to handle exceptions which will
 * not ever return to the point at which they are thrown.  A call 
 * to `catch':
 *
 *	(catch key thunk handler)
 *
 * calls `thunk'.  If an exception of type `key' occurs during
 * the call to `thunk', `handler' is immediately invoked and passed
 * the exception type and parameters as arguments.  When `handler'
 * returns, `catch' immediately returns the value returned from
 * `handler' -- further execution of `thunk' is skipped.
 *
 * To establish a handler that can return to the point at which
 * an exception was thrown with `handle', use `handles':
 *
 *	(handles key thunk handler)
 *
 * If an exception is thrown with `handle', and a handler was 
 * established with `handles', then the value of the handler is
 * returned from `handle'.
 *
 * When searching for handlers, exception types are compared using `eq?'.
 *
 * Finally, for every type of exception, a default handler
 * can be established using `set-default-handler'.
 *
 * This table describes the interactions between `catch', `handles', `throw',
 * `handle', and default handlers:
 *
 *    exception       handler         is this         return from handler
 *    thrown by:      posted by:      handler used?   causes a return from:
 *    =====================================================================
 *    throw           catch           yes             catch
 *    throw           handles         no              -
 *    throw           default         yes             outermost[*]
 *
 *    handle          catch           yes             catch
 *    handle          handles         yes             handle
 *    handle          default         yes             handle
 *
 *            [*] If a default handler returns from an exception
 *                caused by `throw', that causes a return from the
 *                outermost expression being evaluated.  That might
 *                mean a return to a top-level repl or an immediate exit
 *                from the interpreter.
 *
 * If the `key' specified for `catch' or `handles' is #t, then all
 * exceptions are caught, regardless of type.
 *
 * If the `key' specified for `catch' (but not `handles') is `#f', then
 * `thunk' is invoked with one argument: a "jump buffer".  If that
 * object is passed to `throw', as the exception type, then the
 * exception is caught by this call to `catch' and no other.
 * That is the reason that #f can not be a key to `throw' or 
 * `handle' -- because that key has a special meaning when passed
 * to `catch'.
 *
 * If nested calls to `catch' or `handle' use the same value for `key'
 * and that value is not #f, then exceptions are caught by the
 * innermost call.  (Except that the handler supplied to a call to `handles' 
 * is never used for an excpetion thrown by `throw'.)
 *
 * If there is no handler established for a given key, the procedure
 * `bad-throw' is invoked and passed the exception type and parameters.
 * If `bad-throw' returns and the exception was caused by `throw', 
 * control returns from the outermost expression being evaluated.
 * If `bad_throw' returns from an expression caused by `handle', its 
 * return value is returned  from `handle'.
 *
 * The default definition of `bad-throw' operates as follows:
 * 
 * If the exception type has a default handler, that handler is invoked
 * and its value (if it returns) is returned from `bad-throw'.  The
 * procedure `set-default-exception-handler' is used to establish
 * a default handler.  `default-exception-handler' retrieves default
 * handlers.
 *
 * If the exception type has no default handler, then the exception is
 * converted to type 'error and re-thrown.  If not caught, the
 * exception type 'error does have a default handler which prints a
 * message to the current error port and throws an exception of type
 * 'abort.  Exceptions of type 'abort are caught, if nowhere else, by
 * the top-level repl.
 *
 * If no handler can be found for a given exception, for exaple, if `bad-throw'
 * is not defined as a procedure, that is a fatal condition that stops execution 
 * entirely.
 *
 */


SCM_SYMBOL (sym_throw, "throw");
SCM_STRING (s_bogus_catch, "throw to dynamically inactive catch");
SCM scm_bad_throw_vcell;
int scm_tc16_jmpbuffer;


/******************************************************
 * scm_small_object_functions for jump buffers.
 * 
 */

static int
printjb (SCM exp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<jmpbuffer ");
  scm_port_puts (&errn, port, SCM_JBACTIVE(exp) ? "(active) " : "(inactive) ");
  scm_intprint((SCM) SCM_JBJMPBUF(exp), 16, port);
  scm_port_putc (&errn, port, '>');
  return 1 ;
}

static scm_small_object_functions jbsmob = {scm_mark0, scm_free0, printjb, 0};



/* scm_make_jmpbuf
 * 
 * Create a new jump buffer object.
 */
SCM
scm_make_jmpbuf (struct scm_jmp_buf_and_retval * jmpbuf)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  {
    SCM_CAR(answer) = scm_tc16_jmpbuffer;
    SCM_CDR (answer) = (SCM)jmpbuf;
    SCM_DEACTIVATEJB(answer);
  }
  SCM_ALLOW_INTS;
  return answer;
}




/*(c catch)
 * (catch tag thunk handler)
 * 
 * Invoke `thunk' in the dynamic context of `handler' for excpetions
 * matching `key'.  If thunk throws to the exception `key', then `handler' is
 * invoked this way:
 * 
 *      (handler key args ...)
 * 
 * For most values of `key', `thunk' takes no arguments.  If
 * `thunk' returns normally, that is the return value of `catch'.
 * 
 * Handler is invoked outside the scope of its own `catch'.  If
 * `handler' again throws to the same key, a new handler from further
 * up the call chain is invoked.
 * 
 * If the key is #t, then a throw to *any* value will match this
 * call to `catch'.
 * 
 * Key may also be the value #f.  In that case, `thunk' takes one
 * argument which will be passed a "jump buffer object".  A jump
 * buffer object may be used as the key argument to `throw' to throw
 * to a specific `catch' without an intervening search for a key.
 * 
 */
SCM_PROC (s_at_catch, "@catch", 3, 0, 0, scm_at_catch);
SCM
scm_at_catch (SCM tag, SCM thunk, SCM handler)
{
  SCM_INTS_ENABLED;
  SCM_CATCH_LOCALS;
  SCM answer;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (handler) && (SCM_BOOL_F != scm_procedure_p (handler)),
	      handler, scm_arg3, s_at_catch);
  SCM_CATCH_INIT (tag);

  if (SCM_CATCH_SETJMP())
    {
      SCM_UNWIND;
      answer = scm_apply3 (handler, scm_cons (scm_throw_tag, scm_throw_args), SCM_EOL, 0);
    }
  else
    {
      SCM_CATCH_BODY_BEGIN;
      answer = scm_apply3 (thunk,
			  ((tag == SCM_BOOL_F) ? scm_cons (scm_catch_jmpbuf, SCM_EOL) : SCM_EOL),
			  SCM_EOL, 0);
      SCM_CATCH_BODY_END;
    }
  return answer;
}

/*(c handles)
 * (handles tag thunk handler)
 * 
 * Invoke `thunk' in the dynamic context of `handler' for excpetions
 * matching `key'.  If thunk invokes `handle' for the exception `key',
 * then `handler' is invoked this way:
 * 
 *      (handler key args ...)
 * 
 * For most values of `key', `thunk' takes no arguments.  If
 * `thunk' returns normally, that is the return value of `catch'.
 * 
 * Handler is invoked directly by `handle' and may return control
 * to the caller of `handle'.
 * 
 * If the key is #t, then `handle' for *any* value will match this
 * call to `handles'.
 * 
 */
SCM_PROC(s_handles, "handles", 3, 0, 0, scm_handles);
SCM
scm_handles (SCM tag, SCM thunk, SCM handler)
{
  SCM_INTS_ENABLED;
  SCM_HANDLES_LOCALS;
  SCM answer;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (handler) && (SCM_BOOL_F != scm_procedure_p (handler)),
	      handler, scm_arg3, s_handles);
  SCM_HANDLES_INIT (tag, handler);

  SCM_HANDLES_BODY_BEGIN;
  answer = scm_apply3 (thunk, SCM_EOL, SCM_EOL, 0);
  SCM_HANDLES_BODY_END;

  return answer;
}


/* scm_ithrow
 * 
 * Throw an exception of type key, with arguments `args'.
 *
 * If `noreturn' is 1, this is a `throw' and will not return.
 * If `noreturn' is 0, this is a `handle' and might return.
 */
SCM
scm_ithrow (SCM key, SCM args, int noreturn)
{
  SCM_INTS_ENABLED;

  SCM jmpbuf;
  SCM wind_goal;

  /* First, find the wind_goal and the jmpbuf.
   * The wind_goal is a pair in the dynwinds chain.
   * The jmpbuf is the SCM_CDAR of that pair, a jump buffer object.
   */

  if (noreturn && !SCM_IS_IMMEDIATE (key) && SCM_JMPBUFP (key))
    {
      /* We might have been passed the jump buffer.
       * It must be in an active state -- i.e., part of a 
       * dynamically active frame -- and noreturn must not be 0.
       * 
       * If that's the case, then all we have to do is find
       * the jump buffers position on the dynwinds chain and
       * unwind to there.
       */
      jmpbuf = key;
      SCM_ASSERT (SCM_JBACTIVE (jmpbuf), jmpbuf,
		  s_bogus_catch,
		  sym_throw);

      wind_goal = SCM_JBJMPBUF (jmpbuf)->wind_position;
    }
  else
    {
      SCM l;
      SCM dynpair;
      SCM hook;

      /* We might have been passed some object to be looked up
       * on the dynwinds chain.  We look for the first occurence
       * of either the object or #t.  The dynwinds chain is an assoc list.
       */

      l = scm_dynwinds;
      dynpair = SCM_BOOL_F;

      while (l != SCM_EOL)
	{
	  SCM dp;

	  dp = SCM_CAR (l);
	  if (   (   (SCM_CAR (dp) == SCM_BOOL_T)
		  || (SCM_CAR (dp) == key))
	      && (   !noreturn
		  || SCM_JMPBUFP (SCM_CDR (dp)))) 
	    {
	      wind_goal = l;
	      dynpair = dp;
	      jmpbuf = SCM_CDR (dynpair);
	      break;
	    }
	  l = SCM_CDR (l);
	}


      /* If no matching key was found, the bad-throw hook
       * gets a crack at it.  If the hook returns, then
       * we return its value to the caller (if that's permitted).
       *
       * If there is no bad-throw hook or noreturn is not 0, throw to 
       * the root continuation, thereby exiting this instance of Scheme
       * with an error.
       */
      if (dynpair == SCM_BOOL_F)
	{
	  hook = SCM_CDR (scm_bad_throw_vcell);
	  if (SCM_BOOL_T == scm_procedure_p (hook))
	    {
	      SCM value;
	      value = scm_apply3 (hook, scm_cons (key, args), SCM_EOL, 0);
	      if (!noreturn)
		return value;
	    }
	  scm_exitval = scm_cons (key, args);
	  scm_dowinds (SCM_EOL, scm_ilength (scm_dynwinds));
	  longjmp (SCM_REGS (scm_rootcont)->jmpbuf, 1);
	}
    }

  /* If we found the right jump buffer, unwind to that point
   * and do the longjmp or invoke the handler and return to
   * the caller -- whichever is appropriate:
   */
  if (!noreturn && !SCM_JMPBUFP (jmpbuf))
    return scm_apply3 (jmpbuf, scm_cons (key, args), SCM_EOL, 0);
  else
    {
      struct scm_jmp_buf_and_retval * jbr;
      jbr = (struct scm_jmp_buf_and_retval *)SCM_JBJMPBUF (jmpbuf);
      jbr->throw_tag = key;
      jbr->retval = args;
      scm_dowinds (wind_goal, scm_ilength (scm_dynwinds) - scm_ilength (wind_goal));
      longjmp (SCM_JBJMPBUF (jmpbuf)->buf, 1);
    }
}


/*(c throw)
 * (throw key . args)
 * 
 * Invoke the catch form matching `key', passing `args' to the 
 * handler.
 * 
 * key will match catches of the same value or
 * of #t.  If no catch matches but the key has a 
 * default exception handler, that handler is invoked:
 * 
 *      (handler key args ...)
 * 
 * If it returns, control returns from the outermost expression
 * being evaluated.
 * 
 * If there is no handler at all, `bad-throw' is used as the default
 * handler.
 * 
 */
SCM_PROC(s_throw, "throw", 1, 0, 1, scm_throw);
SCM
scm_throw (SCM key, SCM args)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (key != SCM_BOOL_F, key, scm_arg1, s_throw);
  return scm_ithrow (key, args, 1);
}

/*(c handle)
 * (handle key . args)
 * 
 * Invoke the catch form matching `key', passing `args' to the 
 * handler.
 * 
 * key will match catches of the same value or
 * of #t.  If no catch matches but the key has a 
 * default exception handler, that handler is invoked:
 * 
 *      (handler key args ...)
 * 
 * If the handler returns, its value is returned from this function.
 * 
 * If there is no handler at all, `bad-handle' is used as the default
 * handler.
 * 
 */
SCM_PROC(s_handle, "handle", 1, 0, 1, scm_handle);
SCM
scm_handle (SCM key, SCM args)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (key != SCM_BOOL_F, key, scm_arg1, s_handle);
  return scm_ithrow (key, args, 0);
}




void
scm_init_throw (void)
{
  SCM_INTS_DISABLED;

  scm_tc16_jmpbuffer = scm_newsmob (&jbsmob);
  scm_bad_throw_vcell = scm_intern_symhash ("bad-throw", SCM_BOOL_F);
#include "systas/libsystas/throw.x"
}



/****************************************************************
 *(h1 "Rationale -- Catch and Throw")
 *
 * It is well known that functions like `catch', `throw', `handle', and
 * `handles' can be implemented using the single primitive 
 * `call-with-current-continuation'.
 *
 * Actually using `call-with-current-continuation' for that purpose
 * places a heavy burden on the implementation.  `Catch' and `throw'
 * should be relatively fast and that precludes a stack-copying
 * implementation of `call-with-current-continuation'.
 *
 * Precluding a stack-copying `call-with-current-continuation'
 * places a heavy burden on the garbage collector and activation
 * record allocation strategies.  For example, it makes it 
 * impossible for Scheme to share flow of control in any reasonable
 * way with programs written in a stack-oriented language like C
 * since Scheme activation records must be allocated from the
 * heap -- not a stack.  This restriction would, in turn, complicate
 * the implementation of primitives in C -- the calling conventions
 * would be far more complicated than those we actually use.
 *
 * We wanted to mix fluently with C and therefore like the idea
 * of a stack-copying `call-with-current-continuation'.  Therefore,
 * we find ourselves wanting `catch', `throw', `handle' and `handles' to be
 * language primitives -- not synthesized from continuations.
 * We want exception handling to have an implementation like C's 
 * `setjmp/longjmp'. 
 *
 * We chose our particular calling convention and semantics for
 * these functions because they seem to capture the essense of other
 * systems of exception handling while not adding much clutter to
 * the language.
 *
 * We could have gotten by without introducing the new type `jump buffer'
 * to the language.  The same effect (of a newly created catch tag) can be
 * achieved without that type:
 *
 *	(define (catch-1 procedure handler)
 *	  (let ((pseudo-jump-buffer (cons #f #f)))
 *	    (catch pseudo-jump-buffer
 *	      (lambda () (procedure pseudo-jump-buffer))
 *	      (handler))))
 *
 * Which is effectively the same as this example which uses jump
 * buffers:
 *
 *	(define (catch-1 procedure handler)
 *	  (catch #f procedure handler))
 *
 * Conversely, it isn't hard, given our functions, to define `catch' and
 * `throw' which do not treat `#f' specially.  So it makes little difference
 * either way.
 *
 * It is mostly accidental that we have jump buffer objects, though
 * they may be faster than other kinds of exceptions when thrown in
 * situations where the wind chain is quite long.
 */


/************************************************************************
 *h1 "Catch and Throw Internals")
 *
 * Catch and throw are implemented using `setjmp/longjmp'.
 *
 * The system maintains a data structure called the "wind chain".
 * The chain is an association list whose keys are `catch' keys,
 * and whose values are jump buffer objects (if established by `catch')
 * or handler procedures (if established by `handles').  There is an
 * exception to this -- an element whose key is #f and whose CDR is a 
 * cons pair is an entry created by `dynamic-wind' (see dynwind.c).
 *
 * A jump buffer is a non-immediate value:
 *
 *  ..flags:16....scm_tc16_jmpbuffer  struct scm_jump_buf_and_retval *
 *
 * "throw.h" contains macros that are used by C functions wanting
 * to either catch or post handlers for exceptions.  In this
 * file, the functions scm_catch and scm_handles provide examples
 * of how those macros are used.
 *
 * Flags is 0 if the jump buffer is inactive, 1 if it is active.
 * A jump buffer is active if it is on the wind chain and safe to
 * to longjmp to.
 *
 * See "throw.h"
 */
