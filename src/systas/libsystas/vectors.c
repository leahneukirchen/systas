/* vectors.c - scheme vectors (arrays)
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
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/strings.h"


/****************************************************************
 *(h0 "Vectors")
 *
 * A vector is an array of objects, addressed by successive integers
 * starting with 0, in which elements can be referenced or set in a
 * single, efficient operation.
 *
 * Vectors are written this way:
 *
 *	#(a b c)        ; a three element vector
 *
 * Weak vectors and weak hash tables are special kind of vector
 * which interact with the garbage collector in particular ways.
 * See x-ref:"".
 */



SCM_SYMBOL (s_vector_set_length_x, "vector-set-length!");


/*(c vector?)
 * (vector? obj)
 * 
 * Return `#t' if `obj' is a vector, `#f' otherwise.
 *
 * `#t' is returned if `obj' is a weak vector or weak hash table.
 */
SCM_PROC(s_vector_p, "vector?", 1, 0, 0, scm_vector_p);
SCM
scm_vector_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  return scm_is_vector(x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c vector-length)
 * (vector-length vec)
 * 
 * Return the number of elements in the vector `vec'.
 */
SCM_PROC(s_vector_length, "vector-length", 1, 0, 0, scm_vector_length);
SCM
scm_vector_length(SCM v)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_vector(v), v, scm_arg1, s_vector_length);
  return SCM_MAKINUM(SCM_LENGTH(v));
}


/*(c vector-ref)
 * (vector-ref vec n)
 * 
 * Return the `n'th element of vector `vec'.
 */
SCM_PROC(s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref);
SCM
scm_vector_ref(SCM v, SCM k)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_vector(v), v, scm_arg1, s_vector_ref);
  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_vector_ref);
  SCM_ASSERT((SCM_INUM(k) < SCM_LENGTH(v)) && (SCM_INUM(k) >= 0), k, scm_outofrange, s_vector_ref);
  return SCM_VECTOR_ELTS(v)[((long) SCM_INUM(k))];
}


/*(c vector-set!)
 * (vector-set! vector n value)
 * 
 * Set the `n'th element of vector `vector' to `value'.
 * 
 * Return `vector'.
 */
SCM_PROC(s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x);
SCM
scm_vector_set_x(SCM v, SCM k, SCM obj)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_vector (v), v, scm_arg1, s_vector_set_x);
  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_vector_set_x);
  SCM_ASSERT((SCM_INUM(k) < SCM_LENGTH(v)) && (SCM_INUM(k) >= 0), k, scm_outofrange, s_vector_set_x);
  SCM_VECTOR_ELTS(v)[((long) SCM_INUM(k))] = obj;
  return v;
}


/*(c vector=?)
 * (vector=? a b :optional =)
 * 
 * Return #t if `a' and `b' are equal size vectors containing `equal?'
 * elements.  If `=' is specified, it is used to compare elements:
 * 
 * 	(= vector-a-elt vector-b-elt)
 */
SCM_PROC (s_vector_equal_p, "vector=?", 2, 1, 0, scm_vector_equal_p);
SCM
scm_vector_equal_p (SCM x, SCM y, SCM eq)
{
  SCM_INTS_ENABLED;
  long i;

  SCM_ASSERT(scm_is_vector (x), x, scm_arg1, s_vector_equal_p);
  SCM_ASSERT(scm_is_vector (y), y, scm_arg2, s_vector_equal_p);

  if (SCM_LENGTH (x) != SCM_LENGTH (y))
    return SCM_BOOL_F;

  for(i = SCM_LENGTH(x)-1; i >= 0; i--)
    {
      SCM cmp;

      cmp = scm_generalized_equal_p (eq, SCM_VECTOR_ELTS (x)[i], SCM_VECTOR_ELTS (y)[i]);

      if (SCM_BOOL_F == cmp)
	return SCM_BOOL_F;
    }

  return SCM_BOOL_T;
}


/****************************************************************
 *(h1 "Making Vectors")
 *
 */


/*(c make-vector)
 * (make-vector n fill multiple?)
 * 
 * Create a new vector with `n' elements, initialized with `fill'
 * or () if `fill' is not supplied.
 *
 * If `multiple?' is supplied and not `#f', then `fill' is interpreted
 * as a list of initializers.  If there are too few elements in that
 * list, the remaining elements of the vector are initialized to `()'.
 * 
 */
SCM_PROC(s_make_vector, "make-vector", 1, 2, 0, scm_make_vector);
SCM
scm_make_vector (SCM k, SCM fill, SCM multip)
{
  SCM_INTS_ENABLED;
  int i;
  int multi;

  SCM_ASSERT(SCM_INUMP(k) && (0 <= SCM_INUM (k)), k, scm_arg1, s_make_vector);

  i = SCM_INUM (k);

  multi = !(SCM_UNBNDP(multip) || (SCM_BOOL_F == multip));
  return scm_makvector (i, fill, multi, SCM_BOOL_F);
}

/*(c vector-copy)
 * (vector-copy v)
 * 
 * Return a copy of vector `v'.
 */


/*(c vector)
 * (vector . elts)
 * 
 * Construct a new vector containing `elts'.
 */
SCM_PROC(s_vector, "vector", 0, 0, 1, scm_vector);
SCM
scm_vector (SCM l)
{
  SCM_INTS_ENABLED;
  SCM res;
  SCM *data;
  long i;

  i = scm_ilength(l);
  SCM_ASSERT(i >= 0, l, scm_arg1, s_vector);
  res = scm_make_vector(SCM_MAKINUM(i), SCM_UNSPECIFIED, SCM_UNDEFINED);
  data = SCM_VECTOR_ELTS(res);
  for(;i && !SCM_IS_IMMEDIATE(l);--i, l = SCM_CDR(l))
    *data++ = SCM_CAR(l);
  return res;
}


/************************************************************************
 *(h1 "Modifying Vectors")
 * 
 */

/*(c vector-fill!)
 * (vector-fill! vector fill)
 * 
 * Replace all elements of `vector' with `fill'.
 * 
 * Return `vector'.
 */
SCM_PROC(s_vector_fill_x, "vector-fill!", 2, 0, 0, scm_vector_fill_x);
SCM
scm_vector_fill_x (SCM v, SCM fill_x)
{
  SCM_INTS_ENABLED;
  long i;
  SCM *data;

  SCM_ASSERT(scm_is_vector (v), v, scm_arg1, s_vector_fill_x);

  data = SCM_VECTOR_ELTS(v);
  for (i = SCM_LENGTH(v)-1; i >= 0; i--)
    data[i] = fill_x;
  return v;
}


/*(c vector-move-left!)
 * (vector-move-left! from-vector start end to-vector destination)
 * 
 * Copy elements from the `from-vector' to the `to-vector'.  Return
 * `to-vector'.
 *
 * The `start' element through the `(- end 1)' element replace
 * elements of the `to-vector' beginning at position `destination'.
 * The `start' element is copied first and elements are copied in
 * order.
 * 
 * It is an error if any of the subscripts are out of range.
 *
 * `from-vector' and `to-vector' may be the same.
 */
SCM_PROC (s_vector_move_left_x, "vector-move-left!", 5, 0, 0, scm_vector_move_left_x);
SCM
scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2)
{
  SCM_INTS_ENABLED;
  long i;
  long j;
  long e;
  
  SCM_ASSERT (scm_is_vector (vec1), vec1, scm_arg1, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, scm_arg2, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, scm_arg3, s_vector_move_left_x);
  SCM_ASSERT (scm_is_vector (vec2), vec2, scm_arg4, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, scm_arg5, s_vector_move_left_x);

  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);

  SCM_ASSERT (i <= SCM_LENGTH (vec1) && i >= 0, start1, scm_outofrange, s_vector_move_left_x);
  SCM_ASSERT (j <= SCM_LENGTH (vec2) && j >= 0, start2, scm_outofrange, s_vector_move_left_x);
  SCM_ASSERT (e <= SCM_LENGTH (vec1) && e >= 0, end1, scm_outofrange, s_vector_move_left_x);
  SCM_ASSERT (e-i+j <= SCM_LENGTH (vec2), start2, scm_outofrange, s_vector_move_left_x);

  while (i<e)
    SCM_VECTOR_ELTS (vec2)[j++] = SCM_VECTOR_ELTS (vec1)[i++];

  return vec2;
}


/*(c vector-move-right!)
 * (vector-move-right! from-vector start end to-vector destination)
 * 
 * Copy elements from the `from-vector' to the `to-vector'.  Return
 * `to-vector'.
 *
 * The `start' element through the `(- end 1)' element replace
 * elements of the `to-vector' beginning at position `destination'.
 * The `(- end 1)' element is copied first and elements are copied
 * in reverse order.
 *
 * `from-vector' and `to-vector' may be the same.
 */
SCM_PROC (s_vector_move_right_x, "vector-move-right!", 5, 0, 0, scm_vector_move_right_x);
SCM
scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2)
{
  SCM_INTS_ENABLED;
  long i;
  long j;
  long e;

  SCM_ASSERT (scm_is_vector (vec1), vec1, scm_arg1, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, scm_arg2, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, scm_arg3, s_vector_move_right_x);
  SCM_ASSERT (scm_is_vector (vec2), vec2, scm_arg4, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, scm_arg5, s_vector_move_right_x);

  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);

  SCM_ASSERT (i <= SCM_LENGTH (vec1) && i >= 0, start1, scm_outofrange, s_vector_move_right_x);
  SCM_ASSERT (j <= SCM_LENGTH (vec2) && j >= 0, start2, scm_outofrange, s_vector_move_right_x);
  SCM_ASSERT (e <= SCM_LENGTH (vec1) && e >= 0, end1, scm_outofrange, s_vector_move_right_x);
  SCM_ASSERT ((j = e-i+j) <= SCM_LENGTH (vec2), start2, scm_outofrange, s_vector_move_right_x);

  while (i<e)
    SCM_VECTOR_ELTS (vec2)[--j] = SCM_VECTOR_ELTS (vec1)[--e];

  return vec2;
}


/****************************************************************
 *(h1 "Vector <-> List Conversions")
 */

/*(c vector->list)
 * (vector->list vector)
 * 
 * Return a list containing the elements of `vector', in order.
 */
SCM_PROC(s_vector_to_list, "vector->list", 1, 0, 0, scm_vector_to_list);
SCM
scm_vector_to_list(SCM v)
{
  SCM_INTS_ENABLED;
  SCM res;
  long i;
  SCM *data;

  res = SCM_EOL;
  SCM_ASSERT(scm_is_vector (v), v, scm_arg1, s_vector_to_list);
  data = SCM_VECTOR_ELTS(v);
  for (i = SCM_LENGTH(v)-1; i >= 0; i--)
    res = scm_cons(data[i], res);
  return res;
}


/*(c list->vector)
 * (list->vector elts)
 * 
 * Construct a new vector containing the members of the list `elts'.
 */
SCM_PROC(s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
SCM
scm_list_to_vector (SCM elts)
{
  return scm_vector (elts);
}

/************************************************************************
 *(h1 "Vector Iterators")
 * 
 * These procedures iterate over vector elements.
 * 
 */

/*(c vector-map)
 * (vector-map proc vec)
 * 
 * Return a new vector with as many elements as `vec', with each
 * element at index `n' being the value of:
 * 
 *	(proc (vector-ref vec n))
 * 
 * Iteration is guaranteed to be in-order beginning at element 0.
 */

/*(c vector-for-each)
 * (vector-for-each proc vec)
 * 
 * For each element at index `n' of `vec', call:
 * 
 *	(proc (vector-ref vec n))
 * 
 * discarding its return value.
 * 
 * Iteration is guaranteed to be in-order beginning at element 0.
 */



/****************************************************************
 *h1 "The C Interface to Vectors")
 * 
 */


/*c scm_is_vector)
 * int scm_is_vector (SCM x);
 * 
 * Return 1 if `x' is a vector, 0 otherwise.
 * 
 * This function returns 1 for weak vectors and weak hash tables.
 */
int
scm_is_vector (SCM x)
{
  return !SCM_IS_IMMEDIATE (x) && (SCM_TYP7R(x)==scm_tc7_vector);
}


/*c scm_makvector)
 * SCM scm_makvector (int i, SCM fill, int multi, SCM head);
 * 
 * Create a new vector with `i' elements, initialized with `fill'.
 *
 * If `multi' is not 0, then `fill' is interpreted as a list of
 * initializers.  If there are too few elements in that list, the
 * remaining elements of the vector are initialized to `()'.
 *
 * If `fill' is SCM_UNDEFINED, elements are initialized to `()'.
 * 
 */
SCM
scm_makvector (int i, SCM fill, int multi, SCM head)
{
  SCM_INTS_NESTED;
  SCM v;
  long j;
  SCM *velts;

  if (SCM_UNBNDP(fill))
    fill = SCM_BOOL_F;

  if (head != SCM_BOOL_F)
    v = head;
  else
    SCM_NEWCELL(v);

  SCM_REDEFER_INTS;
  if (i)
    SCM_CDR (v) = (SCM)scm_must_malloc (i ? (long)(i*sizeof(SCM)) : 1L);
  else
    SCM_CDR (v) = 0;
  SCM_SET_LENGTH (v, i, scm_tc7_vector);
  velts = SCM_VECTOR_ELTS(v);
  j = 0;
  if (multi)
    {
      while ((fill != SCM_EOL) && (j < i))
	{
	  (velts)[j++] = SCM_CAR (fill);
	  fill = SCM_CDR (fill);
	}
      fill = SCM_BOOL_F;
    }
  while(--i >= j) (velts)[i] = fill;
  SCM_REALLOW_INTS;

  return v;
}




void
scm_init_vectors (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/vectors.x"
}


/************************************************************************
 *h1 "Vectors Internals")
 *
 * All vectors are represented by non-immediate objects:
 *
 *     ..size (24 bits)..scm_tc7_vector  ...........SCM * data...........
 *
 * The `data' is an array of `SCM' values.
 *
 * `data' is an array of SCM values:
 *
 *	SCM * data = SCM_VECTOR_ELTS(vector);
 *
 * The number of elements (not counting the special value) is returned
 * by:
 *
 *	SCM_LENGTH (vector)
 *
 */


/************************************************************************
 *(h1 "Rationale -- Vectors")
 * 
 * Systas simply follows standard Scheme here.
 * 
 */
