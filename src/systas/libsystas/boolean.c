/* boolean.c - scheme boolean values
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
#include "systas/libsystas/boolean.h"


/************************************************************************
 *(h0 "Booleans")
 * 
 * There are two values which are called booleans, written:
 * 
 * 	`#t' 	;; used to represent "true"
 * 	`()'	;; used to represent "false"
 * 	`#f'	;; another way to write "false"
 * 
 * Only one value in the system, `()', represents "false".  All other
 * values, including `#t', are interpreted as "true".  `#f' is just
 * another way to write `()'.
 * 
 * In the text of a program, `()' typically represents ``nil'' or
 * ``nothing'', ``emptiness'', ``void''.  `()' is commonly used to
 * represent ``the empty list'', though it has other uses as well.
 * 
 * There are two important ways that Systas Scheme diverges from
 * standard Scheme, and this is one of them.  (The other is that
 * symbols and identifiers in Systas are guaranteed to be case
 * sensative.)
 * 
 */


/*(c not)
 * (not x)
 * 
 * If `x' is `#f', return `#t', otherwise return `#f'.
 */
SCM_PROC(s_not, "not", 1, 0, 0, scm_not);
SCM
scm_not(SCM x)
{
  SCM_INTS_INDIFFERENT;

 return ((SCM_BOOL_F == x)
	 ? SCM_BOOL_T
	 : SCM_BOOL_F);
}


/*(c boolean?)
 * (boolean? obj)
 * 
 * Return `#t' if `obj' is either `#t' or `#f'.
 */
SCM_PROC(s_boolean_p, "boolean?", 1, 0, 0, scm_boolean_p);
SCM
scm_boolean_p(SCM obj)
{
  SCM_INTS_INDIFFERENT;

  return (((SCM_BOOL_T==obj) || (SCM_BOOL_F==obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c ->bool)
 * (->bool obj)
 * 
 * Return `#f' if `obj' is `#f', otherwise return `#t'.
 */
SCM_PROC (s_to_bool, "->bool", 1, 0, 0, scm_to_bool);
SCM
scm_to_bool (SCM obj)
{
  if (SCM_BOOL_F == obj)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}




/*c scm_int_to_bool
 * SCM scm_int_to_bool (int x);
 * 
 * If `x' is 0, return `#f', return `#t', otherwise, return `#f'.
 */
SCM
scm_int_to_bool (int x)
{
  if (x)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}



void
scm_init_boolean (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/boolean.x"
}


/************************************************************************
 *(h1 "Rationale -- Booleans")
 * 
 * Standard Scheme demands that nil (`()') and false (`#f') be
 * distinct values, and that nil be a true value.  This is one of the
 * ways Systas Scheme is non-standard: nil and false are the same
 * value; nil is false.
 * 
 * We have a handy mnemonic for remembering the situation with `()' and `#f'
 * in Systas Scheme:
 * 
 * 		In Systas Scheme, nothing is false.
 * 
 * Systas deviates from the standard that way for many reasons, 
 * including:
 * 
 * \0. Compact coding idioms./
 * 
 * For example, I really like being able to write:
 * 
 * 		(and x (cdr x)) => <list>
 * 
 * \1. Aesthetic consistency with the design of various control operators
 * (`and' and `or' especially)./
 * 
 * For example, `and', applied to homogenously typed arguments, returns
 * a value of the same type iff that type includes boolean false.
 * That encourages the use of boolean false as a member of
 * non-primitive types to represent the default distinguished case,
 * when there is just one distinguished case.
 * 
 * (So, applying that aesthetic: `#f' is good as end-of-list, because
 * EOL is the natural and unique distinguished case of lists.  But `#f'
 * is not good as the number 0, because while 0 is *sometimes* the
 * natural distinguished case for a particular use of numbers, often
 * the distinguished case is 1 or some other number.)
 * 
 * \2. Better type isomorphism of a subset of Scheme types and a subset of
 * Common Lisp types.  A step in the direction of a universal standard
 * for basic s-expressions./
 * 
 * This would simplify both the exchange of data between CL and
 * Scheme, and the possibility of a single implementation supporting
 * both languages.  Similarly for Scheme and Emacs Lisp.
 * 
 * It would be a step in the direction of making basic s-expression
 * types a fundamental unit of exchange between programs in all
 * languages.
 * 
 * I find it irksome, for example, the way XML has evolved.  A very
 * simple idea -- recursively structured data over simple primitive
 * types -- is mixed up with some arbitrary restrictions (e.g. each
 * node has a keyword-parameterized header followed by a list of
 * sub-nodes).  If the Scheme designers had thought of the basic types
 * as something that could be usefully standardized across languages,
 * we might have general s-expressions instead of XML, and Scheme or
 * another simple Lisp as the language in which to express and
 * exchange (operational) standards for various kinds of type
 * checking, transformations, etc.  
 * 
 * Consider XML Schema.  A type system of superior power and greater
 * simplicity could have been specified precisely and operationally 
 * as a few pages of Scheme code (start with something like Wright's
 * pattern matcher, for example).  Instead, we have a huge and fairly
 * complex specification, rarely implemented or implemented correctly
 * in the public code base, and offering further evidence for the old
 * adage about sufficiently complex programs and common lisp.
 * 
 * Using s-expression types as the data exchange format would lead
 * naturally to using Emacs, Emacs-compatible, and Emacs-like programs
 * as extensible front-ends.  Instead, we have absurdly complex web
 * browsers fronting for programs exchanging XML data.  Maybe we need
 * a new adage: any sufficiently complex web browser is condemned to
 * re-implement most of Emacs -- poorly and with 10 times as much
 * code.
 * 
 * 
 * \3. Accumulated experience and programming idioms from the larger lisp
 * world./
 * 
 * We should give a lot of weight to the accumulated history and
 * experience represented by CL.  We should avoid diverging from it
 * except where there are very clear reasons for doing so.
 * 
 */
