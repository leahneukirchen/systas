/* pairs.c - cons pairs
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
#include "systas/libsystas/gc.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/symbols.h"


/************************************************************************
 *(h0 "Cons Pairs")
 *
 * A "cons pair" is storage for two Scheme values called the "car" and
 * "cdr" of the pair.  Cons pairs are written this way:
 * 
 * 	(a . b)
 * 
 * where `a' is stored in the car of the pair, and `b' is stored in the
 * the cdr.
 * 
 * As a special cases, if `b' is nil, the pair is normally written:
 * 
 * 	(a)
 * 
 * and if b is itself a pair, instead of:
 * 
 * 	(a . (c . (d . (e))))
 * 
 * it is customary to write:
 * 
 * 	(a c d e)
 * 
 * The notations may be mixed, so:
 * 
 * 	(a . (b . c))
 * 
 * is normally written:
 * 
 * 	(a b . c)
 */

/*(c pair?)
 * (pair? obj)
 * 
 * Return `#t' if `obj' is a cons pair, `#f' otherwise.
 */
SCM_PROC(s_pair_p, "pair?", 1, 0, 0, scm_pair_p);
SCM
scm_pair_p(SCM x)
{
  SCM_INTS_UNKNOWN;

  if (SCM_IS_IMMEDIATE(x))
    return SCM_BOOL_F;

  return (SCM_CONSP(x) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*(menu)
 */

/************************************************************************
 *(h1 "Constructing Cons Pairs")
 * 
 */


/*(c cons)
 * (cons a b)
 * 
 * Return a new cons pair with `a' in the car, and `b' in the cdr.
 */
SCM_PROC(s_cons, "cons", 2, 0, 0, scm_cons);
SCM 
scm_cons (SCM x, SCM y)
{
  SCM_INTS_UNKNOWN;
  SCM z;

  SCM_NEWCELL (z);
  SCM_CAR (z) = x;
  SCM_CDR (z) = y;

  return z;
}


/*(c cons*)
 * (cons* elt1 elt2 ... eltn)
 * 
 * With on arguments, return `()':
 * 
 * 	(cons*) => ()
 * 
 * With one argument, return that argument:
 * 
 * 	(cons* OBJ) => OBJ
 *
 * With two or more arguments:
 * 
 * 	(cons* A B ... Z)
 *	=> (cons A (cons* B ... Z))
 * 
 */
SCM_PROC (s_cons_star, "cons*", 0, 0, 1, scm_cons_star);
SCM
scm_cons_star (SCM objs)
{
  if (SCM_EOL == objs)
    return SCM_EOL;
  else if (SCM_EOL == SCM_CDR (objs))
    return SCM_CAR (objs);
  else
    {
      SCM answer;
      SCM * pos;
      answer = SCM_EOL;
      pos = &answer;
      while (SCM_EOL != SCM_CDR (objs))
	{
	  *pos = scm_cons (SCM_CAR (objs), SCM_EOL);
	  pos = &SCM_CDR (*pos);
	  objs = SCM_CDR (objs);
	}
      *pos = SCM_CAR (objs);
      return answer;
    }
}


/************************************************************************
 *(h1 "Modifying Cons Pairs")
 * 
 */

/*(c set-car! obj value)
 * (set-car! obj value)
 * 
 * Set the car of cons-pair `obj' to `value'.
 * 
 * Return `obj'.
 */
SCM_PROC(s_set_car_x, "set-car!", 2, 0, 0, scm_set_car_x);
SCM
scm_set_car_x(SCM pair, SCM value)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(pair) && SCM_CONSP(pair), pair, scm_arg1, s_set_car_x);
  SCM_CAR(pair) = value;
  return pair;
}

/*(c set-cdr!)
 * (set-cdr! obj value)
 * 
 * Set the cdr of cons-pair `obj' to `value'.
 * 
 * Return `obj'.
 */
SCM_PROC(s_set_cdr_x, "set-cdr!", 2, 0, 0, scm_set_cdr_x);
SCM
scm_set_cdr_x(SCM pair, SCM value)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(pair) && SCM_CONSP(pair), pair, scm_arg1, s_set_cdr_x);
  SCM_CDR(pair) = value;
  return pair;
}



/****************************************************************
 *(h1 "CxR Procedures")
 *
 * The procedures in this sectoin are all compositions of `car' and
 * `cdr'.  The name of each of these functions is a description of
 * what it does.  For example, `(cadr x)' is the same as
 * 
 *	(car (cdr x))
 * 
 * In C, these procedures are provided as macros.  For example, `cadr'
 * is the macro `SCM_CADR'.  The macros may be used as l-values, for
 * example:
 * 
 * 	SCM_CAR (x) = value;
 * 
 */

struct scm_iproc
{
  char *scm_string;
  SCM (*cproc)();
};

/*(c car)
 * (car obj)
 *
 * Return the car of cons-pair `obj'.
 */
/*(c cdr)
 * (cdr obj)
 * 
 * Return the cdr of cons-pair `obj'.
 */
/*(c caar)
 * (caar obj)
 * 
 * Return the car of `(car obj)'.
 */
/*(c cadr)
 * (cadr obj)
 * 
 * Return the car of `(cdr obj)'.
 */
/*(c cdar)
 * (cdar obj)
 * 
 * Return the cdr of `(car obj)'.
 */
/*(c cddr)
 * (cddr obj)
 * 
 * Return the cdr of `(cdr obj)'.
 */
/*(c caaar)
 * (caaar obj)
 * 
 * Return the car of `(caar obj)'.
 */
/*(c caadr)
 * (caadr obj)
 * 
 * Return the car of `(cadr obj)'.
 */
/*(c cadar)
 * (cadar obj)
 * 
 * Return the car of `(cdar obj)'.
 */
/*(c caddr)
 * (caddr obj)
 * 
 * Return the car of `(cddr obj)'.
 */
/*(c cdaar)
 * (cdaar obj)
 * 
 * Return the cdr of `(caar obj)'.
 */
/*(c cdadr)
 * (cdadr obj)
 * 
 * Return the cdr of `(cadr obj)'.
 */
/*(c cddar)
 * (cddar obj)
 * 
 * Return the cdr of `(cdar obj)'.
 */
/*(c cdddr)
 * (cdddr obj)
 * 
 * Return the cdr of `(cddr obj)'.
 */
/*(c caaaar)
 * (caaaar obj)
 * 
 * Return the car of `(caaar obj)'.
 */
/*(c caaadr)
 * (caaadr obj)
 * 
 * Return the car of `(caadr obj)'.
 */
/*(c caadar)
 * (caadar obj)
 * 
 * Return the car of `(cadar obj)'.
 */
/*(c caaddr)
 * (caaddr obj)
 * 
 * Return the car of `(caddr obj)'.
 */
/*(c cadaar)
 * (cadaar obj)
 * 
 * Return the car of `(cdaar obj)'.
 */
/*(c cadadr)
 * (cadadr obj)
 * 
 * Return the car of `(cdadr obj)'.
 */
/*(c caddar)
 * (caddar obj)
 * 
 * Return the car of `(cddar obj)'.
 */
/*(c cadddr)
 * (cadddr obj)
 * 
 * Return the car of `(cdddr obj)'.
 */
/*(c cdaaar)
 * (cdaaar obj)
 * 
 * Return the cdr of `(caaar obj)'.
 */
/*(c cdaadr)
 * (cdaadr obj)
 * 
 * Return the cdr of `(caadr obj)'.
 */
/*(c cdadar)
 * (cdadar obj)
 * 
 * Return the cdr of `(cadar obj)'.
 */
/*(c cdaddr)
 * (cdaddr obj)
 * 
 * Return the cdr of `(caddr obj)'.
 */
/*(c cddaar)
 * (cddaar obj)
 * 
 * Return the cdr of `(cdaar obj)'.
 */
/*(c cddadr)
 * (cddadr obj)
 * 
 * Return the cdr of `(cdadr obj)'.
 */
/*(c cdddar)
 * (cdddar obj)
 * 
 * Return the cdr of `(cddar obj)'.
 */
/*(c cddddr)
 * (cddddr obj)
 * 
 * Return the cdr of `(cdddr obj)'.
 */

static struct scm_iproc cxrs[] = 
{
  {"car", 0},
  {"cdr", 0},
  {"caar", 0},
  {"cadr", 0},
  {"cdar", 0},
  {"cddr", 0},
  {"caaar", 0},
  {"caadr", 0},
  {"cadar", 0},
  {"caddr", 0},
  {"cdaar", 0},
  {"cdadr", 0},
  {"cddar", 0},
  {"cdddr", 0},
  {"caaaar", 0},
  {"caaadr", 0},
  {"caadar", 0},
  {"caaddr", 0},
  {"cadaar", 0},
  {"cadadr", 0},
  {"caddar", 0},
  {"cadddr", 0},
  {"cdaaar", 0},
  {"cdaadr", 0},
  {"cdadar", 0},
  {"cdaddr", 0},
  {"cddaar", 0},
  {"cddadr", 0},
  {"cdddar", 0},
  {"cddddr", 0},
  {0, 0}
};


/************************************************************************
 *h1 "The C Interface to Cons pairs")
 * 
 */

/*(c scm_cons2)
 * SCM scm_cons2 (SCM w, SCM x, SCM y);
 * 
 * Return `(cons w (cons x y))'.
 * 
 */
SCM 
scm_cons2 (SCM w, SCM x, SCM y)
{
  SCM_INTS_UNKNOWN;
  SCM a;
  SCM b;

  SCM_NEWCELL (a);
  SCM_CAR (a) = x;
  SCM_CDR (a) = y;

  SCM_NEWCELL (b);
  SCM_CAR (b) = w;
  SCM_CDR (b) = a;

  return b;
}



static void
scm_init_iprocs(struct scm_iproc *subra, int type)
{
  SCM_INTS_UNKNOWN;

  for(;subra->scm_string; subra++)
    scm_make_subr (subra->scm_string,
		   type,
		   subra->cproc,
		   1);
}


void
scm_init_pairs (void)
{
  SCM_INTS_UNKNOWN;

  scm_init_iprocs(cxrs, scm_tc7_cxr);
#include "systas/libsystas/pairs.x"
}


/************************************************************************
 *h1 "Cons Pair Internals")
 *
 * A cons pair is a non-immediate oject:
 *
 *  .............car...............0  ..............cdr...............
 *
 * The tag system is designed so that a 0 in bit 0 of the car of a pair
 * indicates that the pair is a cons pair.  All other pairs (with a 1
 * in bit 0 of the car) are handles for some other type of object.
 */


/************************************************************************
 *(h1 "Rationale -- cons pairs")
 * 
 * Systas Scheme simply follows standard Scheme here.
 * 
 */
