/* alist.c - scheme association lists
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
#include "systas/libsystas/eq.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/alist.h"


/************************************************************************
 *(h0 "Association Lists")
 *
 * Lists of the form:
 *
 *	((k1 . v1) (k2 . v2) (k3 . v3) ...)
 *
 * are considered "association lists".  They represent a mapping from the
 * values `k1 k2 k3...' to the values `v1 v2 v3...'.
 * 
 * In an association, `(k1 . v1)' is called the "value cell" for `k1'.
 * `v1' is called the "binding" of `k1'.  `k1' is a "key" and `v1' a
 * "value".
 * 
 * An association list may contain more than one binding for a
 * particular key, but generally, only the first occuring binding
 * matters.
 * 
 */


/****************************************************************
 *(h1 "Constructing Association Lists")
 * 
 */

/*(c acons)
 * (acons key value alist)
 * 
 * Construct an association list whose car is `(key . value)' and
 * whose cdr is `alist'.  In other words, return:
 * 
 * 	(cons (cons key value) alist)
 */
SCM_PROC(s_acons, "acons", 3, 0, 0, scm_acons);
SCM 
scm_acons (SCM key, SCM value, SCM alist)
{
  SCM_INTS_INDIFFERENT;
  SCM value_cell;
  SCM new_alist;

  SCM_NEWCELL (value_cell);
  SCM_CAR (value_cell) = key;
  SCM_CDR (value_cell) = value;


  SCM_NEWCELL (new_alist);
  SCM_CAR (new_alist) = value_cell;
  SCM_CDR (new_alist) = alist;

  return new_alist;
}


/****************************************************************
 *(h1 "Finding Value Cells in Association Lists: assq assv assoc")
 */

/*(c assq)
 * (assq key alist)
 * 
 * Return the value cell for `key' from the association list `alist'.
 * Keys are compared using `eq?'. If there is no such cell, return
 * `#f'.
 *
 * `alist' is searched from beginning to end.  If a matching key is
 * found, that value cell is immediately returned.  If no matching key
 * is found, #f is returned.
 * 
 * If `alist' is found to be an improper list before a matching key is
 * found, `#f' is returned.
 * 
 * If `alist' contains an element which is not a pair (not a proper
 * value cell), that element is ignored.  
 * 
 * If `alist' is a circular list, `assq' is not guaranteed to
 * terminate.
 */
SCM_PROC (s_assq, "assq", 2, 0, 0, scm_assq);
SCM
scm_assq(SCM key, SCM alist)
{
  SCM_INTS_INDIFFERENT;
  SCM tmp;

  for( ; !SCM_IS_IMMEDIATE(alist) && SCM_CONSP (alist); alist = SCM_CDR(alist))
    {
      tmp = SCM_CAR(alist);
      if (!SCM_IS_IMMEDIATE (tmp) && SCM_CONSP (tmp) && (SCM_CAR (tmp) == key))
	return tmp;
    }
  return SCM_BOOL_F;
}


/*(c assv)
 * (assv key alist)
 * 
 * Return the value cell for `key' from the association list `alist'.
 * Keys are compared using `eqv?'. If there is no such cell, return
 * `#f'.
 *
 * `alist' is searched from beginning to end.  If a matching key is
 * found, that value cell is immediately returned.  If not matching
 * key is found, #f is returned.
 * 
 * If `alist' is found to be an improper list before a matching key is
 * found, `#f' is returned. 
 * 
 * If `alist' contains an element which is not a pair (not a proper
 * value cell), that element is ignored.  
 * 
 * If `alist' is a circular list, `assq' is not guaranteed to
 * terminate.
 */
SCM_PROC (s_assv, "assv", 2, 0, 0, scm_assv);
SCM
scm_assv(SCM x, SCM alist)
{
  SCM_INTS_INDIFFERENT;
  SCM tmp;

  for( ; !SCM_IS_IMMEDIATE (alist) && SCM_CONSP (alist); alist = SCM_CDR(alist))
    {
      tmp = SCM_CAR(alist);
      if (   !SCM_IS_IMMEDIATE (tmp)
	  && SCM_CONSP (tmp)
	  && (SCM_BOOL_F != scm_eqv_p (SCM_CAR (tmp), x)))
	return tmp;
    }
  return SCM_BOOL_F;
}


/*(c assoc)
 * (assoc key alist :optional compare)
 * 
 * Return the value cell for `key' from the association list `alist'.
 * Keys are compared using `equal?' unless `compare' is specified. If
 * there is no such cell, return `#f'.
 * 
 * If `compare' is specified, it is invoked:
 * 
 * 	(compare key key-from-alist)
 * 
 * and should return true if the `key-from-alist' should be considered
 * a match for `key'.
 *
 * `alist' is searched from beginning to end.  If a matching key is
 * found, that value cell is immediately returned.  If not matching
 * key is found, #f is returned.
 * 
 * If `alist' is found to be an improper list before a matching key is
 * found, `#f' is returned.
 * 
 * If `alist' contains an element which is not a pair (not a proper
 * value cell), that element is ignored.  
 * 
 * If `alist' is a circular list, `assq' is not guaranteed to
 * terminate.
 */
SCM_PROC (s_assoc, "assoc", 2, 1, 0, scm_assoc);
SCM
scm_assoc (SCM x, SCM alist, SCM compare)
{
  SCM_INTS_ENABLED;
  SCM tmp;

  for( ; !SCM_IS_IMMEDIATE (alist) && SCM_CONSP (alist); alist = SCM_CDR(alist))
    {
      tmp = SCM_CAR(alist);
      if (   !SCM_IS_IMMEDIATE (tmp)
	  && SCM_CONSP (tmp)
	  && (SCM_BOOL_F != scm_generalized_equal_p (compare, x, SCM_CAR (tmp))))
	return tmp;
    }
  return SCM_BOOL_F;
}


/****************************************************************
 *(h1 "Finding Values in Association Lists: assq-ref assv-ref assoc-ref")
 */

/*(c assq-ref)
 * (assq-ref alist key)
 * 
 * Equivalent to:
 * 
 * 	(and=> (assq key alist) cdr)
 *
 */
SCM_PROC (s_assq_ref, "assq-ref", 2, 0, 0, scm_assq_ref);
SCM
scm_assq_ref (SCM alist, SCM key)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assq (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}


/*(c assv-ref)
 * (assv-ref alist key)
 * 
 * Equivalent to:
 * 
 * 	(and=> (assv key alist) cdr)
 *
 */
SCM_PROC (s_assv_ref, "assv-ref", 2, 0, 0, scm_assv_ref);
SCM
scm_assv_ref (SCM alist, SCM key)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assv (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}


/*(c assoc-ref)
 * (assoc-ref alist key :optional compare)
 * 
 * Equivalent to:
 * 
 * 	(and=> (assoc key alist compare) cdr)
 *
 */
SCM_PROC (s_assoc_ref, "assoc-ref", 2, 1, 0, scm_assoc_ref);
SCM
scm_assoc_ref (SCM alist, SCM key, SCM compare)
{
  SCM_INTS_ENABLED;
  SCM handle;

  handle = scm_assoc (key, alist, compare);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}


/****************************************************************
 *(h1 "Changing Values in Association Lists: assq-set! assv-set! assoc-set!")
 */


/*(c assq-set!)
 * (assq-set! alist key value)
 *
 * Equivalent to:
 *
 *	(let ((value-cell (assq key alist)))
 * 	  (if (not value-cell)
 *	      (acons key value alist)
 *	      (begin
 *		 (set-cdr! value-cell value)
 *		 alist)))
 *
 */
SCM_PROC (s_assq_set_x, "assq-set!", 3, 0, 0, scm_assq_set_x);
SCM
scm_assq_set_x (SCM alist, SCM key, SCM val)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assq (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      SCM_CDR (handle) = val;
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}


/*(c assv-set!)
 * (assv-set! alist key value)
 *
 * Equivalent to:
 *
 *	(let ((value-cell (assv key alist)))
 * 	  (if (not value-cell)
 *	      (acons key value alist)
 *	      (begin
 *		 (set-cdr! value-cell value)
 *		 alist)))
 *
 */
SCM_PROC (s_assv_set_x, "assv-set!", 3, 0, 0, scm_assv_set_x);
SCM
scm_assv_set_x (SCM alist, SCM key, SCM val)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assv (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      SCM_CDR (handle) = val;
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}


/*(c assoc-set!)
 * (assoc-set! alist key value :optional compare)
 *
 * Equivalent to:
 *
 *	(let ((value-cell (assoc key alist compare)))
 * 	  (if (not value-cell)
 *	      (acons key value alist)
 *	      (begin
 *		 (set-cdr! value-cell value)
 *		 alist)))
 *
 */
SCM_PROC (s_assoc_set_x, "assoc-set!", 3, 1, 0, scm_assoc_set_x);
SCM
scm_assoc_set_x (SCM alist, SCM key, SCM val, SCM compare)
{
  SCM_INTS_ENABLED;
  SCM handle;

  handle = scm_assoc (key, alist, compare);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    {
      SCM_CDR (handle) = val;
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}


/****************************************************************
 *(h1 "Removing Values from Association Lists: assq-remove! assv-remove! assoc-remove!")
 */

/*(c assq-remove!)
 * (assq-remove! alist key)
 * 
 * Equivalent to:
 * 
 *	(let ((value-cell (assq key alist)))
 * 	  (if (not value-cell)
 *	      alist
 *	      (delq! value-cell alist)))
 * 
 */
SCM_PROC (s_assq_remove_x, "assq-remove!", 2, 0, 0, scm_assq_remove_x);
SCM
scm_assq_remove_x (SCM alist, SCM key)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assq (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    return scm_delq_x (handle, alist);
  else
    return alist;
}


/*(c assv-remove!)
 * (assv-remove! alist key)
 * 
 * Equivalent to:
 * 
 *	(let ((value-cell (assq key alist)))
 * 	  (if (not value-cell)
 *	      alist
 *	      (delq! value-cell alist)))
 * 
 */
SCM_PROC (s_assv_remove_x, "assv-remove!", 2, 0, 0, scm_assv_remove_x);
SCM
scm_assv_remove_x (SCM alist, SCM key)
{
  SCM_INTS_INDIFFERENT;
  SCM handle;

  handle = scm_assv (key, alist);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    return scm_delq_x (handle, alist);
  else
    return alist;
}


/*(c assoc-remove!) 
 * (assoc-remove! alist key :optional compare)
 * 
 * Equivalent to:
 * 
 *	(let ((value-cell (assoc key alist compare)))
 * 	  (if (not value-cell)
 *	      alist
 *	      (delq! value-cell alist)))
 * 
 */
SCM_PROC (s_assoc_remove_x, "assoc-remove!", 2, 1, 0, scm_assoc_remove_x);
SCM
scm_assoc_remove_x (SCM alist, SCM key, SCM compare)
{
  SCM_INTS_ENABLED;
  SCM handle;

  handle = scm_assoc (key, alist, compare);
  if (!SCM_IS_IMMEDIATE (handle) && SCM_CONSP (handle))
    return scm_delq_x (handle, alist);
  else
    return alist;
}





void
scm_init_alist (void)
{
  SCM_INTS_INDIFFERENT;

#include "systas/libsystas/alist.x"
}




/****************************************************************
 *(h1 "Rationale -- Association Lists"
 *    :category design)
 *
 * Mostly, Systas follows the Scheme standard.
 * 
 * See xref:"q, v, and generalized".
 * 
 * See xref:"Rationale -- Dictionaries in General"
 * 
 * In many procedures that operate on association lists we accept
 * lists that contain elements which are not pairs and simply ignore
 * those elements.
 * 
 * This keeps the implementation slightly smaller and simpler.
 * 
 * This makes the procedures useful on a wider range of input values,
 * though this increased range doesn't seem to be used often in
 * practice.
 * 
 * This increases the chance of undetected errors, but errors of this
 * sort do not seem to be common in practice.  We believe that in
 * programs where such errors are unacceptable, static analysis, not
 * run-time detection, is the proper technique for catching them.
 */

