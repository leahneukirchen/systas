/* hashtab.c - scheme hash tables
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
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/hash.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/vectors.h"


struct scm_ihashx_closure
{
  SCM hash;
  SCM assoc;
  SCM del;
};



/* __STDC__ prototypes for static functions */
static unsigned int scm_ihashx (SCM obj, unsigned int n, struct scm_ihashx_closure * closure);
static SCM scm_assx (SCM obj, SCM alist, struct scm_ihashx_closure * closure);
static SCM scm_delx_x (SCM obj, SCM alist, struct scm_ihashx_closure * closure);
static SCM scm_hash_fn_get_handle (SCM table, SCM obj,
				   unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
				   SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
				   void * closure);
static SCM scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init,
					unsigned int (*hash_fn)(SCM obj,
								unsigned int n,
								void * closure),
					SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
					void * closure);
static SCM scm_hash_fn_ref (SCM table, SCM obj, SCM dflt,
			    unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
			    SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
			    void * closure);
static SCM scm_hash_fn_set_x (SCM table, SCM obj, SCM val,
			      unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
			      SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
			      void * closure);
static SCM scm_hash_fn_remove_x (SCM table,
				 SCM obj,
				 unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
				 SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
				 SCM (*delete_fn)(SCM obj, SCM alist, void * closure),
				 void * closure);


/************************************************************************
 *(h0 "Hash Tables")
 *
 * A hash table is a vector whose elements are association lists (see
 * xref:"Association Lists" and xref:"Vectors").  If the vector has
 * `N' elements, each `key' is stored on the association list at
 * `(hash-fn key N)'.
 * 
 * There are five kinds of hash function:
 * 
 *	(get-handle hash-table key) => (key . value) | #f
 *	(create-handle hash-table key) => (key . value)
 * 	(ref hash-table key :optional default) => value | default
 * 	(set! hash-table key value) => hash-table
 * 	(remove! hash-table key value) => hash-table
 * 
 * For each of those five kinds of function, four implementations are
 * provided:
 *
 *	hashq-	; hash function `hashq' and association function `assq'
 *	hashv-	; hash function `hashv' and association function `assv'
 *	hash-	; hash function `hash' and association function `assoc'
 *	hashx-	; hash function and association function are parameters
 * 
 * For example, there are:
 * 
 * 	(hashq-ref hash-table key)
 * 	(hashv-ref hash-table key :optional default)
 * 	(hash-ref hash-table key)
 * 	(hashx-ref hash-fn assoc-fn hash-table key)
 * 	
 */


SCM_SYMBOL (s_hash_fn_get_handle, "scm_hash_fn_get_handle");
SCM_SYMBOL (s_hash_fn_create_handle_x, "scm_hash_fn_create_handle_x");
SCM_SYMBOL (s_hash_fn_remove_x, "scm_hash_fn_remove_x");


/*(c make-hash-table)
 * (make-hash-table :optional n)
 * 
 * Make a hash table (vector) with `n' slots (default: 63).
 * 
 */


/*(c hashq-get-handle)
 * (hashq-get-handle hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table'. Return `#f'
 * if no such pair exists.
 *
 * Keys are compared using `eq?'.
 */
SCM_PROC (s_hashq_get_handle, "hashq-get-handle", 2, 0, 0, scm_hashq_get_handle);
SCM
scm_hashq_get_handle (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_get_handle (table, obj,
				 (unsigned int (*)(SCM, unsigned int, void *))scm_ihashq,
				 (SCM (*)(SCM, SCM, void *))scm_assq,
				 0);
}


/*(c hashq-create-handle)
 * (hashq-create-handle hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table', creating it if
 * necessary.
 *
 * Keys are compared using `eq?'.
 */
SCM_PROC (s_hashq_create_handle_x, "hashq-create-handle!", 3, 0, 0, scm_hashq_create_handle_x);
SCM
scm_hashq_create_handle_x (SCM table, SCM obj, SCM init)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_create_handle_x (table, obj, init,
				      (unsigned int (*)(SCM, unsigned int, void *))scm_ihashq,
				      (SCM (*)(SCM, SCM, void *))scm_assq,
				      0);
}


/*(c hashq-ref)
 * (hashq-ref hash-table key (:optional default))
 * 
 * Return the value associated with `key' in `hash-table'.  If there
 * is no such value, return `default' or `#f'.
 *
 * Keys are compared using `eq?'.
 */
SCM_PROC (s_hashq_ref, "hashq-ref", 2, 1, 0, scm_hashq_ref);
SCM 
scm_hashq_ref (SCM table, SCM obj, SCM dflt)
{
  SCM_INTS_ENABLED;

  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt,
			  (unsigned int (*)(SCM, unsigned int, void *))scm_ihashq,
			  (SCM (*)(SCM, SCM, void *))scm_assq,
			  0);
}


/*(c hashq-set!)
 * (hashq-set! hash-table key value)
 * 
 * Set the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `eq?'.
 */
SCM_PROC (s_hashq_set_x, "hashq-set!", 3, 0, 0, scm_hashq_set_x);
SCM 
scm_hashq_set_x (SCM table, SCM obj, SCM val)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_set_x (table, obj, val,
			    (unsigned int (*)(SCM, unsigned int, void *))scm_ihashq,
			    (SCM (*)(SCM, SCM, void *))scm_assq,
			    0);
}


/*(c hashq-remove!)
 * (hashq-remove! hash-table key)
 * 
 * Remove the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `eq?'.
 */
SCM_PROC (s_hashq_remove_x, "hashq-remove!", 2, 0, 0, scm_hashq_remove_x);
SCM
scm_hashq_remove_x (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_remove_x (table, obj,
			       (unsigned int (*)(SCM, unsigned int, void *))scm_ihashq,
			       (SCM (*)(SCM, SCM, void *))scm_assq,
			       (SCM (*)(SCM, SCM, void *))scm_delq_x,
			       0);
}




/*(c hashv-get-handle)
 * (hashv-get-handle hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table'. Return `#f'
 * if no such pair exists.
 *
 * Keys are compared using `eqv?'.
 */
SCM_PROC (s_hashv_get_handle, "hashv-get-handle", 2, 0, 0, scm_hashv_get_handle);
SCM
scm_hashv_get_handle (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_get_handle (table, obj,
				 (unsigned int (*)(SCM, unsigned int, void *))scm_ihashv,
				 (SCM (*)(SCM, SCM, void *))scm_assv,
				 0);
}


/*(c hashv-create-handle)
 * (hashv-create-handle hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table' creating it if
 * necessary.
 *
 * Keys are compared using `eqv?'.
 */
SCM_PROC (s_hashv_create_handle_x, "hashv-create-handle!", 3, 0, 0, scm_hashv_create_handle_x);
SCM
scm_hashv_create_handle_x (SCM table, SCM obj, SCM init)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_create_handle_x (table, obj, init,
				      (unsigned int (*)(SCM, unsigned int, void *))scm_ihashv,
				      (SCM (*)(SCM, SCM, void *))scm_assv,
				      0);
}


/*(c hashv-ref)
 * (hashv-ref hash-table key (:optional default))
 * 
 * Return the value associated with `key' in `hash-table'.
 * If there is no such value, return `default' or `#f'.
 *
 * Keys are compared using `eqv?'.
 */
SCM_PROC (s_hashv_ref, "hashv-ref", 2, 1, 0, scm_hashv_ref);
SCM 
scm_hashv_ref (SCM table, SCM obj, SCM dflt)
{
  SCM_INTS_ENABLED;

  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt,
			  (unsigned int (*)(SCM, unsigned int, void *))scm_ihashv,
			  (SCM (*)(SCM, SCM, void *))scm_assv,
			  0);
}


/*(c hashv-set!)
 * (hashv-set! hash-table key value)
 * 
 * Set the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `eqv?'.
 */
SCM_PROC (s_hashv_set_x, "hashv-set!", 3, 0, 0, scm_hashv_set_x);
SCM 
scm_hashv_set_x (SCM table, SCM obj, SCM val)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_set_x (table, obj, val,
			    (unsigned int (*)(SCM, unsigned int, void *))scm_ihashv,
			    (SCM (*)(SCM, SCM, void *))scm_assv,
			    0);
}


/*(c hashv-remove!)
 * (hashv-remove! hash-table key)
 * 
 * Remove the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `eqv?'.
 */
SCM_PROC (s_hashv_remove_x, "hashv-remove!", 2, 0, 0, scm_hashv_remove_x);
SCM
scm_hashv_remove_x (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_remove_x (table, obj,
			       (unsigned int (*)(SCM, unsigned int, void *))scm_ihashv,
			       (SCM (*)(SCM, SCM, void *))scm_assv,
			       (SCM (*)(SCM, SCM, void *))scm_delv_x,
			       0);
}



/*(c hash-get-handle)
 * (hash-get-handle hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table'. Return `#f'
 * if no such pair exists.
 *
 * Keys are compared using `equal?'.
 */
SCM_PROC (s_hash_get_handle, "hash-get-handle", 2, 0, 0, scm_hash_get_handle);
SCM
scm_hash_get_handle (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_get_handle (table, obj,
				 (unsigned int (*)(SCM, unsigned int, void *))scm_ihash,
				 (SCM (*)(SCM, SCM, void *))scm_assoc,
				 (void *)SCM_BOOL_F);
}


/*(c hash-create-handle)
 * (hash-create-handle hash-table obj)
 * 
 * Return (perhaps create) a key-value pair for `obj' in `hash-table'.
 *
 * Keys are compared using `equal?'.
 */
SCM_PROC (s_hash_create_handle_x, "hash-create-handle!", 3, 0, 0, scm_hash_create_handle_x);
SCM
scm_hash_create_handle_x (SCM table, SCM obj, SCM init)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_create_handle_x (table, obj, init,
				      (unsigned int (*)(SCM, unsigned int, void *))scm_ihash,
				      (SCM (*)(SCM, SCM, void *))scm_assoc,
				      (void *)SCM_BOOL_F);
}


/*(c hash-ref)
 * (hash-ref hash-table key (:optional default))
 * 
 * Return the value associated with `key' in `hash-table'.  If there
 * is no such value, return `default' or `#f'.
 *
 * Keys are compared using `equal?'.
 */
SCM_PROC (s_hash_ref, "hash-ref", 2, 1, 0, scm_hash_ref);
SCM 
scm_hash_ref (SCM table, SCM obj, SCM dflt)
{
  SCM_INTS_ENABLED;

  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt,
			  (unsigned int (*)(SCM, unsigned int, void *))scm_ihash,
			  (SCM (*)(SCM, SCM, void *))scm_assoc,
			  (void *)SCM_BOOL_F);
}


/*(c hash-set!)
 * (hash-set! hash-table key value)
 * 
 * Set the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `equal?'.
 */
SCM_PROC (s_hash_set_x, "hash-set!", 3, 0, 0, scm_hash_set_x);
SCM 
scm_hash_set_x (SCM table, SCM obj, SCM val)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_set_x (table, obj, val,
			    (unsigned int (*)(SCM, unsigned int, void *))scm_ihash,
			    (SCM (*)(SCM, SCM, void *))scm_assoc,
			    (void *)SCM_BOOL_F);
}


/*(c hash-remove!)
 * (hash-remove! hash-table key)
 * 
 * Remove the value associated with `key' in `hash-table'.
 *
 * Keys are compared using `equal?'.
 */
SCM_PROC (s_hash_remove_x, "hash-remove!", 2, 0, 0, scm_hash_remove_x);
SCM
scm_hash_remove_x (SCM table, SCM obj)
{
  SCM_INTS_ENABLED;

  return scm_hash_fn_remove_x (table, obj,
			       (unsigned int (*)(SCM, unsigned int, void *))scm_ihash,
			       (SCM (*)(SCM, SCM, void *))scm_assoc,
			       (SCM (*)(SCM, SCM, void *))scm_delete_x,
			       (void *)SCM_BOOL_F);
}



/* scm_ihashx
 * 
 * A hash function for use with the scm_hash_fn_ functions.
 */
static unsigned int
scm_ihashx (SCM obj, unsigned int n, struct scm_ihashx_closure * closure)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = scm_apply3 (closure->hash,
		      scm_listify (obj, scm_ulong2num ((unsigned long)n), SCM_UNDEFINED),
		      SCM_EOL, 0);
  return SCM_INUM (answer);
}


/* scm_assx
 * 
 * An association function for use with the scm_hash_fn_ functions.
 */
static SCM
scm_assx (SCM obj, SCM alist, struct scm_ihashx_closure * closure)
{
  SCM_INTS_DISABLED;
  SCM answer;

  answer = scm_apply3 (closure->assoc,
		      scm_listify (obj, alist, SCM_UNDEFINED),
		      SCM_EOL, 0);
  return answer;
}


/* scm_delx
 * 
 * An delete function for use with the scm_hash_fn_ functions.
 */
static SCM
scm_delx_x (SCM obj, SCM alist, struct scm_ihashx_closure * closure)
{
  SCM_INTS_DISABLED;
  SCM answer;

  answer = scm_apply3 (closure->del,
		       scm_listify (obj, alist, SCM_UNDEFINED),
		       SCM_EOL, 0);
  return answer;
}



/*(c hashx-get-handle)
 * (hashx-get-handle hash-fn assoc-fn hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table'. Return `#f'
 * if no such pair exists.
 *
 * Hash values are computed using `hash-fn'.   Buckets are searched 
 * using `assoc-fn'.
 */
SCM_PROC (s_hashx_get_handle, "hashx-get-handle", 4, 0, 0, scm_hashx_get_handle);
SCM
scm_hashx_get_handle (SCM hash, SCM assoc, SCM table, SCM obj)
{
  SCM_INTS_ENABLED;
  struct scm_ihashx_closure closure;

  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_get_handle (table, obj,
				 (unsigned int (*)(SCM, unsigned int, void *))scm_ihashx,
				 (SCM (*)(SCM, SCM, void *))scm_assx,
				 (void *)&closure);
}


/*(c hashx-create-handle)
 * (hashx-create-handle hash-fn assoc-fn hash-table obj)
 * 
 * Return a key-value pair for `obj' in `hash-table', creating it if
 * necessary.
 *
 * Hash values are computed using `hash-fn'.   Buckets are searched 
 * using `assoc-fn'.
 */
SCM_PROC (s_hashx_create_handle_x, "hashx-create-handle!", 5, 0, 0, scm_hashx_create_handle_x);
SCM
scm_hashx_create_handle_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM init)
{
  SCM_INTS_ENABLED;
  struct scm_ihashx_closure closure;

  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_create_handle_x (table, obj, init,
				      (unsigned int (*)(SCM, unsigned int, void *))scm_ihashx,
				      (SCM (*)(SCM, SCM, void *))scm_assx,
				      (void *)&closure);
}


/*(c hashx-ref)
 * (hashx-ref hash-fn assoc-fn hash-table key (:optional default))
 * 
 * Return the value associated with `key' in `hash-table'. Return
 * `default' or `#f' if no such value exists.
 *
 * Hash values are computed using `hash-fn'.   Buckets are searched 
 * using `assoc-fn'.
 */
SCM_PROC (s_hashx_ref, "hashx-ref", 4, 1, 0, scm_hashx_ref);
SCM 
scm_hashx_ref (SCM hash, SCM assoc, SCM table, SCM obj, SCM dflt)
{
  SCM_INTS_ENABLED;
  struct scm_ihashx_closure closure;

  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_ref (table, obj, dflt,
			  (unsigned int (*)(SCM, unsigned int, void *))scm_ihashx,
			  (SCM (*)(SCM, SCM, void *))scm_assx,
			  (void *)&closure);
}


/*(c hashx-set!)
 * (hashx-set! hash-fn assoc-fn hash-table key value)
 * 
 * Set the value associated with `key' in `hash-table'.
 *
 * Hash values are computed using `hash-fn'.   Buckets are searched 
 * using `assoc-fn'.
 */
SCM_PROC (s_hashx_set_x, "hashx-set!", 5, 0, 0, scm_hashx_set_x);
SCM 
scm_hashx_set_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM val)
{
  SCM_INTS_ENABLED;
  struct scm_ihashx_closure closure;

  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_set_x (table, obj, val,
			    (unsigned int (*)(SCM, unsigned int, void *))scm_ihashx,
			    (SCM (*)(SCM, SCM, void *))scm_assx,
			    (void *)&closure);
}


/*(c hashx-remove)
 * (hashx-remove hash-fn assoc-fn hash-table key)
 * 
 * Remove the value associated with `key' in `hash-table'.
 *
 * Hash values are computed using `hash-fn'.   Buckets are searched 
 * using `assoc-fn' and the key-value pair removed using `del'.
 */
SCM
scm_hashx_remove_x (SCM hash, SCM assoc, SCM del, SCM table, SCM obj)
{
  SCM_INTS_ENABLED;
  struct scm_ihashx_closure closure;

  closure.hash = hash;
  closure.assoc = assoc;
  closure.del = del;
  return scm_hash_fn_remove_x (table, obj,
			       (unsigned int (*)(SCM, unsigned int, void *))scm_ihashx,
			       (SCM (*)(SCM, SCM, void *))scm_assx,
			       (SCM (*)(SCM, SCM, void *))scm_delx_x,
			       (void *)&closure);
}



static SCM
scm_hash_fn_get_handle (SCM table, SCM obj,
			unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
			SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
			void * closure)
{
  SCM_INTS_ENABLED;
  int k;
  SCM h;

  SCM_ASSERT (scm_is_vector (table), table, scm_arg1, s_hash_fn_get_handle);
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      SCM_MAKINUM (k),
	      scm_outofrange,
	      s_hash_fn_get_handle);
  h = assoc_fn (obj, SCM_VECTOR_ELTS (table)[k], closure);
  return h;
}


static SCM
scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init,
			     unsigned int (*hash_fn)(SCM obj,
						     unsigned int n,
						     void * closure),
			     SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
			     void * closure)
{
  SCM_INTS_ENABLED;
  int k;
  SCM it;
  SCM new_bucket;
  SCM old_bucket;
  SCM answer;

  SCM_ASSERT (scm_is_vector (table), table, scm_arg1, s_hash_fn_create_handle_x);
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      SCM_MAKINUM (k),
	      scm_outofrange,
	      s_hash_fn_create_handle_x);
  it = assoc_fn (obj, SCM_VECTOR_ELTS (table)[k], closure);
  if (!SCM_IS_IMMEDIATE (it))
    answer = it;
  else
    {
      old_bucket = SCM_VECTOR_ELTS (table)[k];
      new_bucket = scm_acons (obj, init, old_bucket);
      SCM_VECTOR_ELTS(table)[k] = new_bucket;
      answer = SCM_CAR (new_bucket);
    }
  return answer;
}



static SCM 
scm_hash_fn_ref (SCM table, SCM obj, SCM dflt,
		 unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
		 SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
		 void * closure)
{
  SCM_INTS_ENABLED;

  SCM it;

  it = scm_hash_fn_get_handle (table, obj, hash_fn, assoc_fn, closure);
  if (SCM_IS_IMMEDIATE (it))
    return dflt;
  else
    return SCM_CDR (it);
}



static SCM 
scm_hash_fn_set_x (SCM table, SCM obj, SCM val,
		   unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
		   SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
		   void * closure)
{
  SCM_INTS_ENABLED;
  SCM it;

  it = scm_hash_fn_create_handle_x (table, obj, SCM_BOOL_F, hash_fn, assoc_fn, closure);
  SCM_CDR (it) = val;
  return table;
}


static SCM 
scm_hash_fn_remove_x (SCM table,
		      SCM obj,
		      unsigned int (*hash_fn)(SCM obj, unsigned int n, void * closure),
		      SCM (*assoc_fn)(SCM obj, SCM alist, void * closure),
		      SCM (*delete_fn)(SCM obj, SCM alist, void * closure),
		      void * closure)
{
  SCM_INTS_ENABLED;
  int k;
  SCM h;

  SCM_ASSERT (scm_is_vector (table), table, scm_arg1, s_hash_fn_remove_x);
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      SCM_MAKINUM (k),
	      scm_outofrange,
	      s_hash_fn_remove_x);
  h = assoc_fn (obj, SCM_VECTOR_ELTS (table)[k], closure);
  SCM_VECTOR_ELTS(table)[k] = delete_fn (h, SCM_VECTOR_ELTS(table)[k], closure);
  return h;
}




void
scm_init_hashtree (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/hashtab.x"
}


/****************************************************************
 * Hash Table Internals
 *
 * This is quite systematic.  The hash table operations are:
 *
 *	get_handle	- return the key-value pair (or `#f') for a given key
 *	create_handle	- return (perhaps create) a key-value pair for a given key
 *	ref		- return the value for a given key
 *	set_x		- set the value for a given key
 *	remove_x	- remove the key-value pair for a given key
 *
 * For each of those operations we provide several versions:
 *
 *	scm_hash_fn_	- the most general form, only callable from C
 *	scm_hashq_	- the eq? version
 *	scm_hashv_	- the eqv? version
 *	scm_hash_	- the equal? version
 *	scm_hashx_	- a Scheme procedure that accepts hash functions and
 *			  bucket association functions as parameters
 *
 */


/************************************************************************
 *(h1 "Rationale -- Hash Tables")
 * 
 * The hash table functions follow the general pattern of interfaces
 * for dictionaries (see xref:"Rationale -- Dictionaries in General").
 * 
 */
