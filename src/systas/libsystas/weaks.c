/* weaks.c - weak vectors and hash tables
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
#include "systas/libsystas/error.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/weaks.h"


/****************************************************************
 *(h0 "Weak Vectors and Weak Hash Tables")
 *
 * Normally, if object A references object B, then object B is
 * guaranteed to exist for as long as A does.  "Weak references" are
 * the exception to that rule.
 * 
 * An object A may weakly reference an object B, in which case B might
 * cease to exist before A does.  If that happens, A's reference to B
 * is removed.  The rule is that when all the references to an object
 * are weak references, the system is free (but not obligated) to
 * destroy the object and remove the references at any time.  If any
 * reference to an object is not a weak reference (including the
 * implicit reference of using the object as an anonymous intermediate
 * value of a complex expression), then the object is protected and
 * all references to the object, including weak references, will
 * continue to exist.
 * 
 * There are several types in Systas that hold weak references to
 * other objects.  These types are all vector-like objects - `vector?'
 * returns true if passed one of these objects.
 * 
 * A "weak-vector" is like a vector, except that the elements of the
 * vector are weakly referenced.  When a reference is removed from a
 * weak vector, that element is replaced by `()'.
 * 
 * A "weak-key-hash-table" is like a vector, except that if any elements
 * of the vector are assoc lists, then the *keys* (cars of key-value
 * cells) of those lists are weakly referenced.  When a key is removed
 * from such an assoc list because the object it refers to has been
 * destroyed, the key-value cell for that key is deleted from the
 * assoc list.
 * 
 * A "weak-value-hash-table" is like a vector, except that if any
 * elements of the vector are assoc lists, then the *values* (cdrs of
 * key-value cells) of those lists are weakly referenced.  When a value is
 * removed from such an assoc list because the object it refers to has
 * been destroyed, the key-value cell for that value is deleted
 * from the assoc list.
 * 
 * A "doubly-weak-hash-table" is like a vector, except that if any
 * elements of the vector are assoc lists, then both the *keys* and
 * *values* (cars and cdrs of key-value cells) of those lists are
 * weakly referenced.  When a key or value is removed from such an assoc
 * list because the object it refers to has been destroyed, the 
 * key-value cell for that value is deleted from the assoc list.
 *
 * The printed representations of these objects are:
 *
 *	#w( elts... )		; a weak hash table.
 *	#wv( elts... )		; a weak value hash table.
 *	#wk( elts... )		; a weak key hash table.
 *	#wkv( elts... )		; a doubly weak hash table.
 */



enum scm_weak_vector_kinds
{
  scm_weak_vector_tag = 0,
  scm_weak_key_hash_table_tag = 1,
  scm_weak_value_hash_table_tag = 2,
  scm_doubly_weak_hash_table_tag = 3,
};




/*(c weak-vector?)
 * (weak-vector? obj)
 * 
 * Return `#t' if `obj' is a weak vector, `#f' otherwise.
 */
SCM_PROC(s_weak_vector_p, "weak-vector?", 1, 0, 0, scm_weak_vector_p);
SCM
scm_weak_vector_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return ((scm_is_weak_vector (x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c weak-key-hash-table?)
 * (weak-key-hash-table? obj)
 *
 * Return `#t' if `obj' is a weak key hash table, `#f' otherwise.
 */
SCM_PROC (s_weak_key_hash_table_p, "weak-key-hash-table?",
	  1, 0, 0, scm_weak_key_hash_table_p);
SCM
scm_weak_key_hash_table_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return (scm_is_weak_key_hash_table (x)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c weak-value-hash-table?)
 * (weak-value-hash-table? obj)
 *
 * Return `#t' if `obj' is a weak value hash table, `#f' otherwise.
 */
SCM_PROC (s_weak_value_hash_table_p, "weak-value-hash-table?",
	  1, 0, 0, scm_weak_value_hash_table_p);
SCM
scm_weak_value_hash_table_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return (scm_is_weak_value_hash_table (x)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c doubly-weak-hash-table?)
 * (doubly-weak-hash-table? obj)
 *
 * Return `#t' if `obj' is a doubly weak hash table, `#f' otherwise.
 */
SCM_PROC (s_doubly_weak_hash_table_p, "doubly-weak-hash-table?",
	  1, 0, 0, scm_doubly_weak_hash_table_p);
SCM
scm_doubly_weak_hash_table_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return (scm_is_doubly_weak_hash_table (x)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}



/****************************************************************
 *(h1 "Constructing Weak Vectors")
 */

/*(c make-weak-vector)
 * (make-weak-vector n :optional fill multiple?)
 *
 * Create a new weak vector of `n' elements, initialized to `fill' or
 * `()' if `fill' is not specified.
 * 
 * If `multiple?' is supplied and not `#f', then `fill' is interpreted
 * as a list of initializers.  If there are too few elements in that
 * list, the remaining elements are initialized to `()'.
 */
SCM_PROC(s_make_weak_vector, "make-weak-vector", 1, 2, 0, scm_make_weak_vector);
SCM
scm_make_weak_vector (SCM k, SCM fill, SCM multi)
{
  SCM_INTS_ENABLED;
  SCM v;

  v = scm_make_vector (scm_sum (k, SCM_MAKINUM (1)), fill, multi);
  SCM_DEFER_INTS;
  SCM_SET_LENGTH(v, SCM_INUM (k), scm_tc7_wvect);
  SCM_VECTOR_ELTS(v)[0] = (SCM)scm_weak_vector_tag;
  SCM_VECTOR_ELTS(v) = SCM_VECTOR_ELTS(v) + 1;
  SCM_ALLOW_INTS;
  return v;
}


/*(c weak-vector)
 * (weak-vector . elts)
 * 
 * Construct a new weak vector containing `elts'.
 */
SCM_PROC(s_weak_vector, "weak-vector", 0, 0, 1, scm_weak_vector);
SCM
scm_weak_vector (SCM l)
{
  SCM_INTS_ENABLED;
  SCM res;
  SCM *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, scm_arg1, s_weak_vector);
  res = scm_make_weak_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED, SCM_BOOL_F);
  data = SCM_VECTOR_ELTS (res);
  for (;
       i && !SCM_IS_IMMEDIATE (l) && SCM_CONSP (l);
       --i, l = SCM_CDR (l))
    *data++ = SCM_CAR (l);
  return res;
}


/*(c list->weak-vector)
 * (list->weak-vector elts)
 * 
 * Construct a new weak vector containing the members of the list
 * `elts'.
 */
SCM_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);
SCM
scm_list_to_weak_vector (SCM elts)
{
  return scm_weak_vector (elts);
}


/****************************************************************
 *(h1 "Constructing Weak Hash Tables")
 */


/*(c make-weak-key-hash-table)
 * (make-weak-key-hash-table n)
 *
 * Create a weak key hash table with `n' buckets.
 */
SCM_PROC(s_make_weak_key_hash_table, "make-weak-key-hash-table",
	 1, 0, 0, scm_make_weak_key_hash_table);
SCM
scm_make_weak_key_hash_table (SCM k)
{
  SCM_INTS_ENABLED;
  SCM v;

  SCM_ASSERT (SCM_INUMP (k), k, scm_arg1, s_make_weak_key_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL, SCM_BOOL_F);
  SCM_VECTOR_ELTS (v)[-1] = scm_weak_key_hash_table_tag;
  return v;
}


/*(c make-weak-value-hash-table)
 * (make-weak-value-hash-table n)
 *
 * Create a weak value hash table with `n' buckets.
 */
SCM_PROC (s_make_weak_value_hash_table, "make-weak-value-hash-table",
	  1, 0, 0, scm_make_weak_value_hash_table);
SCM
scm_make_weak_value_hash_table (SCM k)
{
  SCM_INTS_ENABLED;
  SCM v;

  SCM_ASSERT (SCM_INUMP (k), k, scm_arg1, s_make_weak_value_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL, SCM_BOOL_F);
  SCM_VECTOR_ELTS (v)[-1] = scm_weak_value_hash_table_tag;
  return v;
}


/*(c make-doubly-weak-hash-table)
 * (make-doubly-weak-hash-table n)
 *
 * Create a doubly weak hash table with `n' buckets.
 */
SCM_PROC (s_make_doubly_weak_hash_table, "make-doubly-weak-hash-table",
	  1, 0, 0, scm_make_doubly_weak_hash_table);
SCM
scm_make_doubly_weak_hash_table (SCM k)
{
  SCM_INTS_ENABLED;
  SCM v;

  SCM_ASSERT (SCM_INUMP (k), k, scm_arg1, s_make_weak_value_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL, SCM_BOOL_F);
  SCM_VECTOR_ELTS (v)[-1] = scm_doubly_weak_hash_table_tag;
  return v;
}


/****************************************************************
 *h1 "The C Interface to Weak Vectors and Hash Tables")
 */

/*c scm_is_weak_vector)
 * int scm_is_weak_vector (SCM x);
 * 
 * Return 1 if `x' is a weak vector, 0 otherwise.
 */
int
scm_is_weak_vector (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7(x)==scm_tc7_wvect)  && (SCM_VECTOR_ELTS(x)[-1] == scm_weak_vector_tag);
}


/*c scm_is_weak_key_hash_table)
 * int scm_is_weak_key_hash_table (SCM x);
 * 
 * Return 1 if `x' is a weak key hash table, 0 otherwise.
 */
int
scm_is_weak_key_hash_table (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7(x)==scm_tc7_wvect) && (SCM_VECTOR_ELTS(x)[-1] == scm_weak_key_hash_table_tag);
}


/*c scm_is_weak_value_hash_table)
 * int scm_is_weak_value_hash_table (SCM x);
 * 
 * Return 1 if `x' is a weak value hash table, 0 otherwise.
 */
int
scm_is_weak_value_hash_table (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7(x)==scm_tc7_wvect) && (SCM_VECTOR_ELTS(x)[-1] == scm_weak_value_hash_table_tag);
}


/*c scm_is_doubly_weak_hash_table)
 * int scm_is_doubly_weak_hash_table (SCM x);
 * 
 * Return 1 if `x' is a doubly weak value hash table, 0 otherwise.
 */
int
scm_is_doubly_weak_hash_table (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7(x)==scm_tc7_wvect) && (SCM_VECTOR_ELTS(x)[-1] == scm_doubly_weak_hash_table_tag);
}


/*c scm_is_weak_hash_table)
 * int scm_is_weak_hash_table (SCM x);
 * 
 * Return 1 if `x' is any variety of weak hash table, 0 otherwise.
 */
int
scm_is_weak_hash_table (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7(x)==scm_tc7_wvect) && (SCM_VECTOR_ELTS(x)[-1] != scm_weak_vector_tag);
}



void
scm_init_weaks (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/weaks.x"
}



/****************************************************************
 *h1 "Weak References Internals")
 *
 * All weak vectors are represented by non-immediate objects:
 *	
 *  ...size (24 bits)..scm_tc7_wvect  ...........SCM * data...........
 *
 * `scm_tc7_wvect' and `scm_tc7_vector' differ only in the "R" option bit:
 *
 *	SCM_TYP7R(a_weak_vector) == scm_tc7_vector
 *	SCM_TYP7(a_weak_vector) == scm_tc7_wvect
 *
 * The `data' is an array of SCM values, except for one word to
 * the left of the data:
 *
 *	SCM * data = SCM_VECTOR_ELTS(a_weak_vector) - the vector elements
 *	SCM_VECTOR_ELTS(a_weak_vector)[-1]	    - the special value
 *
 * The special value to the left of the data contains an 
 * ancillary tag (defined by an enum in `"weaks.c"':
 *
 *	scm_is_weak_vector		- a weak vector
 *	scm_is_weak_key_hash_table	- a weak key hash table
 *	scm_is_weak_value_hash_table	- a weak value hash table
 *	scm_is_doubly_weak_hash_table	- a doubly weak hash table
 *
 * The number of elements (not counting the special value) is:
 *
 *	SCM_LENGTH (a_weak_vector)
 *
 * Garbage collection of weak vectors works like this:
 *
 * During ordinary marking, live weak vectors are recorded in an
 * array.  The non-weak keys of weak-value tables and the non-weak
 * values of weak-key tables are also marked at this time.
 * 
 * The top-level cons pairs and key-value cons pairs of the
 * association lists in weak hash tables are not marked when the hash
 * table itself is marked.  That's because those objects contain
 * references to values that are weakly held by the hash tables.  If
 * those same cons pairs are referenced by some other live object
 * besides the hash table, then those weakly held objects are actually
 * strongly held.  If we were to mark the pairs but not the objects
 * they reference while marking a hash table, that would prevent the
 * objects they reference from being properly marked in the case that
 * the cons pairs are referenced outside of the hash table.  To
 * prevent that problem, the cons pairs that make up association lists
 * in weak hash tables are marked after the ordinary mark phase.
 *
 * This scenario illustrates:
 * 
 *	A: a weak key hash table
 * 	B: an ordinary vcell (storage for a top-level variable)
 *	C: a cons pair which is accessible as (vector-ref A 0)
 *	D: a cons pair which is accessible (CAR C)
 * 	K: a key in the weak hash table which is accessible as (CAR D)
 * 	V: a value in the weak hash table which is accessible as (CDR D)
 * 
 * Suppose that the value of `B' (which is stored in `SCM_CDR(B)') is
 * `()'.  Then during marking we:
 * 
 *		1. Mark A
 * 		2.   Postpone marking C and D
 * 		3.   Avoid marking K since it is weakly held
 *		4.   Mark V
 *		5. Mark B
 *		6. 	Mark CAR(B)
 *		7.	stop because CDR(B) is nil
 *
 * And in a special mark phase immediately afterwards, we scan the
 * weak hash table, `A', to mark the spines of the assoc lists it
 * contains:
 * 
 * 		8. Mark C
 *		9.	Mark D
 *		10. 	Avoid marking K (a weakly held key)
 * 
 * During the sweep phase, dead objects are removed from the hash
 * table.  Since `K' was not marked, the pair `(K . V)' is removed
 * from the association list `C' and hence from weak hash table `A'.
 * `K' is now dead.
 *
 * Now consider the same scenario, exceptthat `CDR(B)' is `D':
 * 
 *		1. Mark A
 * 		2.   Postpone marking C and D
 * 		3.   Avoid marking K since it is weakly held
 *		4.   Mark V
 *		5. Mark B
 * 		6.	Mark CAR(B)
 *		7.	Mark D (which is CDR(B))
 * 		8.		Mark K (which is CAR(D))
 *		9.		V (which is CDR(D)) is already 
 *			        marked, so stop.
 * 
 * In the special mark phase for weak hash table association list spines:
 *
 *		10. C is already marked so stop.
 * 
 * This time, the reference to `K' via `B' has kept `K' alive even
 * though that same reference happens to be part of a spine of an
 * association list in the weak vector `A'.  If we had marked `D' in
 * step 2, then steps 7, 8, and 9 would not have occured.  After
 * sweeping, the value of `B' would still be `D', but the `CAR' of `D'
 * would be a dead object (formerly `K').
 * 
 * At this time, the garbage collector is not robust against circular 
 * association lists in weak hash tables (it should be made so).
 */


/****************************************************************
 *(h1 "Rationale -- Weak References")
 *
 * Weak references were included to support a style of programming
 * with memoization that is not possible without them.
 *
 * Memoization, weak-references, and garbage collection interact
 * nicely: a function can be memoized in a weak hash table without
 * leaking storage.  The memo retains only live values.  If only a few
 * values would otherwise live, programmers can augment a weak hash
 * table with a bounded-size queue of recently memoized values to
 * extend the lifetime of a limited number of memoized values.
 *
 * Without weak refrences, or something equivalent, a memo must either
 * grow without bound, or fail to promise `eq?' results for `eq?'
 * arguments.  Consider whether it is possible to implement the
 * following without weak references of some kind and without leaking
 * storage:
 *
 * 		(define memo (make-doubly-weak-hash-table 509))
 * 
 * 		;; This version of cons always returns 
 *		;; an eq? pair for eq? objects except that
 *		;; the pairs it creates may be garbage
 *		;; collected if they are not otherwise 
 *		;; referenced.
 *		;;
 *		(define (cached-cons a b)
 *		   (let ((prototype (cons a b)))
 *		      (or (hash-ref memo prototype)
 * 			  (hash-set! memo prototype))))
 *
 *
 * Other systems provide "weak pointers" which is conceptually a more
 * general facility (though there is no actual difference in
 * capability).
 * 
 * Weak vectors and hash tables were provided in Systas to "optimize
 * the common case" (our most favored uses for weak references) and
 * because they happened to be easy to implement.  Had we implemented
 * weak pointers and tried to build hash tables from those, the result
 * would probably have been much less efficient and less convenient.
 *
 * Weak pointers may be provided later, if a need for them is
 * demonstrated.
 */

