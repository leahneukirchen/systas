/* eq.c - scheme tests for equality
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
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/async.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/variable.h"


/************************************************************************
 *(h0 "Equality")
 *
 * There are three built-in ways to compare objects for equality.
 *
 * `eq?' tests whether its arguments are one-in-the-same object.  If
 * it return `#t', the arguments can not be distinguished.  The `eq?'
 * test for equality is the fastest.
 *
 * `eqv?' tests whether its arguments are indistinguishable except
 * possibly by procedures that rely on `eq?' equivalence.  For
 * example, two inexact numbers may have exactly the same bit-wise
 * representation, yet not be `eq?'.  Those two numbers are `eqv?'.
 *
 * `eq?' values are always `eqv?'.
 *
 * `equal?' tests whether its arguments are structurally similar.  If
 * it returns `#t', then the arguments are lists of equal elements, or
 * vectors of equal elements, or strings which are `string=?', etc.
 * The `equal?' test for equality is the slowest.
 *
 * `eqv?' values are always `equal?'.
 * 
 * `eq?' is useful to compare objects of types with relevant
 * uniqueness properties (such as symbols), and to compare objects
 * when the the underlying question is whether or not data-flow
 * through a program has brought together two references to the very
 * same object.
 * 
 * `eqv?' is useful to compare objects whose equality under `eq?' is 
 * not guaranteed by uniqueness properties.
 * 
 * `eq?' and `eqv?' share the property that if two objects are equal,
 * mutations to one are mutations to both (in fact, if two mutable
 * objects are `eqv?', then they are `eq?').
 * 
 */


/*(c eq?)
 * (eq? . args)
 * 
 * Return `#t' if all of the arguments are one-in-the-same object,
 * otherwise return `#f'.
 * 
 */
SCM_PROC1 (s_eq_p, "eq?", scm_tc7_rpsubr, scm_eq_p);
SCM
scm_eq_p (SCM x, SCM y)
{
  SCM_INTS_INDIFFERENT;

  return ((x==y)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c eqv?)
 * (eqv? . args)
 * 
 * Return `#t' if all of the arguments are indistinguishable except
 * by procedures that rely on `eq?' equivalence, otherwise return 
 * `#f'.
 *
 * `eq?' values are always `eqv?'.
 * 
 */
SCM_PROC1 (s_eqv_p, "eqv?", scm_tc7_rpsubr, scm_eqv_p);
SCM
scm_eqv_p (SCM x, SCM y)
{
  SCM_INTS_INDIFFERENT;

  if (x==y)
    return SCM_BOOL_T;

  if (SCM_IS_IMMEDIATE(x))
    return SCM_BOOL_F;

  if (SCM_IS_IMMEDIATE(y))
    return SCM_BOOL_F;

  /* this ensures that types and scm_length are the same. */
  if (SCM_CAR(x) != SCM_CAR(y))
    return SCM_BOOL_F;

  if (SCM_VARIABLEP (x))
    {
      if (   (SCM_VARIABLE_VALUE_LOC (x) == SCM_VARIABLE_VALUE_LOC (y))
	  && (SCM_VARIABLE_NAME (x) == SCM_VARIABLE_NAME (y))
	  && (SCM_VARIABLE_TYPE (x) == SCM_VARIABLE_TYPE (y)))
	return SCM_BOOL_T;
      else
	return SCM_BOOL_F;
    }

  if (SCM_NUMP (x))
    {
#ifdef SCM_BIGDIG
      if (SCM_BIGP (x))
	return ((0 == scm_bigcomp (x, y))
		? SCM_BOOL_T
		: SCM_BOOL_F);
#endif

#ifdef SCM_FLOATS
      if (SCM_REALPART(x) != SCM_REALPART(y))
	return SCM_BOOL_F;

      if (SCM_CPLXP(x) && (SCM_IMAG(x) != SCM_IMAG(y)))
	return SCM_BOOL_F;
#endif

      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


/*(c equal?)
 * (equal? . args)
 * 
 * Return `#t' if all of the arguments are structurally identical.
 *
 * "Structurally identical" is recursively defined.  For example,
 * two lists are `equal?' if they are the same length and the
 * corresponding elements are `equal?'.
 *
 * `eqv?' values are always `equal?'.
 * 
 */
SCM_PROC1 (s_equal_p, "equal?", scm_tc7_rpsubr, scm_equal_p);
SCM
scm_equal_p (SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_CHECK_STACK;
 tailrecurse:
  SCM_ASYNC_TICK;
  if (x==y)
    return SCM_BOOL_T;

  if (scm_is_char (x) && scm_is_char (y) && (scm_char_to_int (x) == scm_char_to_int (y)))
    return SCM_BOOL_T;

  if (SCM_IS_IMMEDIATE(x))
    return SCM_BOOL_F;

  if (SCM_IS_IMMEDIATE(y))
    return SCM_BOOL_F;

  if (SCM_CONSP(x) && SCM_CONSP(y))
    {
      if (SCM_BOOL_F == scm_equal_p (SCM_CAR (x), SCM_CAR (y)))
	return SCM_BOOL_F;
      x = SCM_CDR(x);
      y = SCM_CDR(y);
      goto tailrecurse;
    }

  if (   (SCM_TYP7SU (x) == scm_tc7_string)
      && (SCM_TYP7SU (y) == scm_tc7_string))
    return scm_string_equal_p (x, y);
  
  /* This ensures that types and scm_length are the same,
   * except for strings.   Some different types of strings
   * may yet be equal.
   */

  if (SCM_CAR(x) != SCM_CAR(y))
    return SCM_BOOL_F;

  switch (SCM_TYP7(x))
    {
    default:
      return SCM_BOOL_F;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return scm_vector_equal_p(x, y, SCM_BOOL_F);

    case scm_tc7_smob:
      {
	int i = SCM_SMOBNUM(x);
	if (!(i < scm_numsmob))
	  return SCM_BOOL_F;
	if (scm_smobs[i].equalp)
	  return (scm_smobs[i].equalp)(x, y);
	else
	  return SCM_BOOL_F;
      }
    }
  return SCM_BOOL_F;
}

/************************************************************************
 *h1 "The C Interface for Generalized Equality Tests"
 * 
 * 
 * 
 */

/*c scm_generalized_equal_p
 * SCM scm_generalized_equal_p (SCM pred, SCM a, SCM b);
 * 
 * If `pred' is `SCM_UNDEFINED' or `SCM_BOOL_F', return `scm_equal_p
 * (a, b)'.
 * 
 * Otherwise, `pred' should be a procedure which accepts two arguments.
 * Return:
 * 
 *	(pred a b)
 * 
 */
SCM
scm_generalized_equal_p (SCM pred, SCM a, SCM b)
{
  if ((pred == SCM_BOOL_F) || (pred == SCM_UNDEFINED))
    return scm_equal_p (a, b);
  else
    return scm_apply3 (pred, scm_listify (a, b, SCM_UNDEFINED), SCM_EOL, 0);
}






void
scm_init_eq (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/eq.x"
}


/*(h1 "q, v, and generalized")
 * 
 * In standard scheme, some procedures which test values for equality
 * are grouped in sets of three, corresponding to the three standard
 * ways of testing for equality.  These procedures follow a regular
 * naming convention of names ending with "q" (for procedures that use
 * `eq?'), "v" (`eqv?'), and names without a special suffix
 * (`equal?').
 * 
 * In Systas scheme, the convention is extended.  The procedure that 
 * uses `equal?' usually accepts an optional argument which is used
 * instead of `equal?' to test for equality.  For example, 
 * 
 * 	(assoc hostname domain-rules same-domain?)
 * 
 * will use `same-domain?' to search for matches of `hostname' in 
 * the association list `domain-rules'.
 * 
 * The added generality adds very little complexity to the
 * implementation, and saves programmers from having to write their
 * own generalizations.
 */

/************************************************************************
 *(h1 "Rationale -- Equality")
 * 
 * The Systas Scheme equality tests simply follow standard Scheme.
 * 
 * 
 */
