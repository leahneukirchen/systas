/* promise.c - delayed evaluation
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998 Free Software Foundation,Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/promise.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/read-print.h"


/************************************************************************
 *(h0 "Promises")
 * 
 * A "promise" represents a lazily-computed value that is cached once
 * it has been computed.
 */

/****************************************************************
 * Small Object Functions for Promises
 *
 */

static int 
prinprom (SCM exp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<promise ");
  scm_iprin1 (SCM_CDR (exp), port, writing);
  scm_port_putc (&errn, port, '>');
  return !0;
}

static long scm_tc16_promise;
static scm_small_object_functions promsmob = {scm_markcdr, scm_free0, prinprom};



/*(c promise?)
 * promise? obj
 * 
 * Return `#t' if `obj' is a promise, `#f' otherwise.
 */
SCM_PROC (s_promise_p, "promise?", 1, 0, 0, scm_promise_p);
SCM
scm_promise_p (SCM x)
{
  SCM_INTS_UNKNOWN;

  return ((!SCM_IS_IMMEDIATE (x) && (SCM_TYP16 (x) == scm_tc16_promise))
	   ? SCM_BOOL_T
	   : SCM_BOOL_F);
}


/*(c make-promise)
 * (make-promise thunk)
 * 
 * Return a new promise whose forced value is computed by `(thunk)'.
 * See also *xref*:"delay".
 */
SCM_PROC (s_make_promise, "make-promise", 1, 0, 0, scm_make_promise);
SCM
scm_make_promise (SCM thunk)
{
  SCM_INTS_ENABLED;
  SCM z;

  SCM_ASSERT ((SCM_BOOL_F != scm_procedure_p (thunk)), thunk, scm_arg1, s_make_promise);
  SCM_NEWCELL (z);
  SCM_CDR (z) = thunk;
  SCM_CAR (z) = scm_tc16_promise;
  return z;
}


/*(force)
 * (force promise)
 * 
 * Return the value of the expression of `promise'.
 * 
 * The first time the value of a promise is forced, the delayed value
 * is computed and remembered.  Subsequently, the remembered value is
 * returned.
 * 
 *	(define a-promise (let loop ((x 0))  (delay (cons x (loop (+ x 1))))))
 * 
 *	(force a-promise) => (0 . #<promise...>)
 *	(force (cdr (force a-promise))) => (1 . #<promise...>)
 *	(force (cdr (force (cdr (force a-promise))))) => (2 . #<promise...>)
 *
 * If `promise' is not a promise, it is returned unmodified.
 */
SCM_PROC(s_force, "force", 1, 0, 0, scm_force);
SCM 
scm_force (SCM x)
{
  SCM_INTS_ENABLED;

  if (!(!SCM_IS_IMMEDIATE (x) && (SCM_TYP16 (x) == scm_tc16_promise)))
    return x;

  if (!((1L << 16) & SCM_CAR (x)))
    {
      SCM ans;

      ans = scm_apply3 (SCM_CDR (x), SCM_EOL, SCM_EOL, 0);
      if (!((1L << 16) & SCM_CAR (x)))
	{
	  SCM_DEFER_INTS;
	  SCM_CDR (x) = ans;
	  SCM_CAR (x) |= (1L << 16);
	  SCM_ALLOW_INTS;
	}
    }
  return SCM_CDR (x);
}




void
scm_init_promise (void)
{
  SCM_INTS_DISABLED;

  scm_tc16_promise = scm_newsmob (&promsmob);
#include "systas/libsystas/promise.x"
}


/************************************************************************
 * Promises Internals
 *
 * Promises are represented as non-immediate values:
 *
 *  .......forced?..scm_tc16_promise   .......closure.or.value.........
 *
 * forced? is a one-bit flag (in bit 16) that is 1 if the promise
 * has been forced, 0 otherwise.
 *
 * If the promise has not been forced, the cdr holds a closure.
 * If the promise has been forced, the cdr holds a value.
 */


/************************************************************************
 *(h0 "Rationale -- Promises")
 * 
 * 
 * Systas simply follows the Scheme standard here.
 */
