/* ar.c - variable size arrays
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/machine/alignment.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/arrays/ar.h"

/************************************************************************
 *(h1 "Variable Size Arrays"
 *	:include ("hackerlab/arrays/ar.h"))
 *
 * |array|
 * |variable size array|
 * |power of two size array|
 * |aparse array|
 *
 * A "variable size array" is a dynamically allocated block of memory,
 * similar to a block returned by `lim_malloc', except that a variable sized
 * array is tagged with its size, measured in the number of array elements.
 * 
 * A null pointer counts as an array of 0 elements.  For example, if
 * `ar_size', which returns the size of a variable sized arrary, is
 * passed `0', it returns 0.  That means there is no special function
 * to allocate a new variable sized array -- instead, array pointers
 * should be initialized to 0.  This example creates an array with ten
 * integers by using `ar_ref':
 *
 *	{
 *	  int * the_array;
 *	  int * tenth_element;
 * 
 *	  the_array = 0;
 *	  tenth_element = (int *)ar_ref (&the_array,
 *				         lim_use_must_malloc,
 *				         9,
 * 				         sizeof (int));
 *	}
 *
 * A variable size array can be used as a stack. (See `ar_push' and
 * `ar_pop'.)
 *
 * Array functions use the `lim_malloc' family of functions to allcoate
 * memory.  (See xref:"Allocation With Limitations".)
 */
/*(menu)
 */

/************************************************************************
 * (h2 "Variable Size Array Internals")
 *
 * A variable size array is represented by a malloced block of memory
 * approximately one word larger than the memory containing the
 * array's elements.  The extra word contains the number of elements
 * in the array:
 *
 *
 *
 *	__________________________________________________
 *	| padding | n-elements | elt0 | elt1 | elt2 | ...
 *	--------------------------------------------------
 *		               ^
 *		               | `base'
 *
 * Pointers to the array point to element 0 (labeled `base').  The
 * size of each element is determined by the `szof' parameter to
 * functions like `ar_setsize' and `ar_ref'.  It is not recorded as part
 * of the array.
 * 
 * Padding is added as necessary to ensure that the alignment of `elt0'
 * is as would be returned from malloc.
 *
 * The amount of storage allocated to the array may be larger than is
 * indicated by `n-elements'.
 */

/************************************************************************
 *(h2 "Basic Variable Size Array Functions")
 * 
 * 
 * 
 */

#define SIZEOF_HEADER		((sizeof (int) > MACHINE_ALIGNMENT) ? sizeof (int) : MACHINE_ALIGNMENT)
#define AR_TO_HEADER(B)		((int *)((char *)(B) - SIZEOF_HEADER))
#define HEADER_TO_AR(H)		((void *)((char *)(H) + SIZEOF_HEADER))

/*(c ar_size)
 * int ar_size (void * base,
 *	      alloc_limits limits,
 * 	      size_t elt_size);
 *
 * Return the number of elements in the array.  If `base == 0', return
 * 0.
 * 
 * `limits' is the allocation limits associated with this array.
 */
int
ar_size (void * base, alloc_limits limits, size_t elt_size)
{
  if (!base)
    return 0;
  else
    return *AR_TO_HEADER(base);
}


/*(c ar_ref)
 * void * ar_ref (void ** base,
 *                alloc_limits limits,
 *                int n,
 *                int szof);
 * 
 * Return the address of element `n' of an array, expanding the array
 * to `n+1' elements, if necessary.
 *
 * `base' is a pointer to a pointer to the array.
 *
 * `limits' is the allocation limits associated with this array.
 * 
 * `szof' is the size, in bytes, of one element of the array.
 *
 * If this function adds new elements to an array, those elements are
 * filled with 0 bytes.
 *
 * This function may resize and relocate the array.  If it does,
 * `*base' is updated to point to the new location of the array.
 */
void *
ar_ref (void ** base,
	alloc_limits limits,
	int n,
	int szof)
{
  int * size;
  void * m;
  char * b;

  if (!*base)
    {
      m = lim_malloc (limits, SIZEOF_HEADER + szof * (n + 1));
      if (!m)
	return 0;
      b = HEADER_TO_AR (m);
      size = AR_TO_HEADER (b);
      mem_set0 (b, szof * (n + 1));
      *size = n + 1;
      *base = b;
    }
  else
    {
      b = (char *)*base;
      size = AR_TO_HEADER (b);
      if (*size < (n + 1))
	{
	  size_t old_size;
	  old_size = *size;
	  m = lim_realloc (limits, size, (SIZEOF_HEADER + szof * (n + 1)));
	  if (!m)
	    return 0;
	  b = HEADER_TO_AR (m);
	  size = AR_TO_HEADER (b);
	  mem_set0 (b + old_size * szof, (n + 1 - old_size) * szof);
	  *size = n + 1;
	  *base = b;
	}
    }
  return b + szof * n;
}


/*(c ar_setsize)
 * void ar_setsize (void ** base,
 *                alloc_limits limits,
 *                int n,
 *                size_t szof);
 * 
 * Resize the array so that it contains exactly `n' elements.
 *
 * `base' is a pointer to a pointer to the array.
 * 
 * `limits' is the allocation limits associated with this array.
 *
 * `szof' is the size, in bytes, of one element.
 *
 * If this function adds new elements to an array, those elements are
 * filled with 0 bytes.
 *
 * This function can be used to make an array smaller, but doing so
 * does not reclaim any storage. (See `ar_compact'.)
 */
void
ar_setsize (void ** base,
	  alloc_limits limits,
	  int n,
	  size_t szof)
{
  if (!szof && !*base)
    return;

  if (n > 0)
    {
      ar_ref (base, limits, n - 1, szof);
    }

  if (*base)
    {
      *AR_TO_HEADER (*base) = n;
    }
}


/*(c ar_compact)
 * void ar_compact (void ** base,
 *                alloc_limits limits,
 *                size_t szof);
 * 
 * Resize an array so that it is only as large as it needs to be.
 *
 * `base' is a pointer to a pointer to the array.
 * 
 * `limits' is the allocation limits associated with this array.
 *
 * `szof' is the size, in bytes, of one element.
 *
 * This function may resize and relocate the array.  If it does,
 * `*base' is updated to point to the new location of the array.
 *
 * Functions like `ar_setsize' can be used to make an array smaller, but
 * doing so does not reclaim any storage used by the array and does
 * not move the array in memory.
 *
 * This function does attempt to reclaim storage (by using
 * `lim_realloc').  If the array occupies significantly more memory
 * than needed, this function will move it to a smaller block.
 * If `lim_realloc' returns 0, this function has no effect.
 */
void
ar_compact (void ** base,
	  alloc_limits limits,
	  size_t szof)
{
  size_t size;

  size = ar_size (*base, limits, szof);

  if (size == 0)
    {
      if (*base)
	ar_free (base, limits);
      *base = 0;
      return;
    }

  *base = HEADER_TO_AR (lim_realloc (limits, (void *)AR_TO_HEADER (*base), SIZEOF_HEADER + size * szof));
}


/*(c ar_free)
 * void ar_free (void ** base, alloc_limits limits);
 * 
 * Release storage associated with the array pointed to by `*base'.
 * Set `*base' to 0.
 * 
 * `limits' is the allocation limits associated with this array.
 */
void
ar_free (void ** base, alloc_limits limits)
{
  if (*base)
    lim_free (limits, (void *)AR_TO_HEADER (*base));
  *base = 0;
}



/************************************************************************
 *(h2 "Variable Sized Arrays as Stacks")
 * 
 * 
 * 
 */

/*(c ar_push)
 * void * ar_push (void ** base,
 *               alloc_limits limits,
 *               size_t szof);
 * 
 * Return the address of element `n' in an array previously containing
 * only `n-1' elements.
 *
 * `base' is a pointer to a pointer to the array.
 * 
 * `limits' is the allocation limits associated with this array.
 *
 * `szof' is the size, in bytes, of one element.
 *
 * The new array element is filled with 0 bytes.
 *
 * This function may resize and relocate the array.  If it does,
 * `*base' is updated to point to the new location of the array.
 */
void *
ar_push (void ** base,
       alloc_limits limits,
       size_t szof)
{
  return ar_ref (base, limits, ar_size (*base, limits, szof), szof);
}


/*(c ar_pop)
 * void * ar_pop (void ** base,
 *              alloc_limits limits,
 *              size_t szof);
 * 
 * Return the address of the `n'th element in an array previously
 * containing `n' elements.  Resize the array so that it contains
 * exactly `n-1' elements.
 *
 * `base' is a pointer to a pointer to the array.
 * 
 * `limits' is the allocation limits associated with this array.
 *
 * `szof' is the size, in bytes, of one element.
 *
 * This function may resize and relocate the array.  If it does,
 * `*base' is updated to point to the new location of the array.
 */
void *
ar_pop (void ** base,
      alloc_limits limits,
      size_t szof)
{
  int size;

  size = ar_size (*base, limits, szof);
  ar_setsize (base, limits, size - 1, szof);
  return (void *)((char *)*base + ((size - 1) * szof));
}


/*(c ar_copy)
 * void * ar_copy (void * base,
 *               alloc_limits limits,
 *               size_t szof);
 * 
 * Create a new array which is a copy of the array pointed to by
 * `base'.
 * 
 * `limits' is the allocation limits associated with this array.
 */
void *
ar_copy (void * base,
       alloc_limits limits,
       size_t szof)
{
  void * answer;

  answer = 0;
  ar_setsize (&answer, limits, ar_size (base, limits, szof), szof);
  mem_move (answer, base, szof * ar_size (base, limits, szof));
  return answer;
}
