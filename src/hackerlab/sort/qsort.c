/* qsort.c: 
 *
 ****************************************************************
 * Copyright (C) 1991, 1992, 1996, 1997, 1999 Free Software Foundation, Inc.
 * Modifications (C) 2002, Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 * 
 * This file is snarfed from the GNU C Library and modified for libhackerlab.
 * Originally Written by Douglas C. Schmidt (schmidt@ics.uci.edu).
 * 
 */


#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "hackerlab/sort/qsort.h"



/* This implementation incorporates four optimizations discussed in Sedgewick:
 * 
 *    1. Non-recursive, using an explicit stack of pointer that store the
 *       next array partition to sort.  To save time, this maximum amount
 *       of space required to store an array of SIZE_MAX is allocated on the
 *       stack.  Assuming a 32-bit (64 bit) integer for size_t, this needs
 *       only 32 * sizeof(stack_node) == 256 bytes (for 64 bit: 1024 bytes).
 *       Pretty cheap, actually.
 * 
 *    2. Chose the pivot element using a median-of-three decision tree.
 *       This reduces the probability of selecting a bad pivot value and
 *       eliminates certain extraneous comparisons.
 * 
 *    3. Only quicksorts TOTAL_ELEMS / MAX_THRESH partitions, leaving
 *       insertion sort to order the MAX_THRESH items within each partition.
 *       This is a big win, since insertion sort is faster for small, mostly
 *       sorted array segments.
 * 
 *    4. The larger of the two sub-partitions is always pushed onto the
 *       stack first, with the algorithm then concentrating on the
 *       smaller partition.  This *guarantees* no more than log (total_elems)
 *       stack size is needed (actually O(1) in this case)!  
 */



/* Discontinue quicksort algorithm when partition gets below this size.
 * This particular magic number was chosen to work best on a Sun 4/260.
 */
#define MAX_THRESH 4

/* Stack node declarations used to store unfulfilled partition obligations.
 */
struct stack_node
{
    char *lo;
    char *hi;
};

/* The next 4 #defines implement a very fast in-line stack abstraction.
 *
 * The stack needs log (total_elements) entries (we could even subtract
 * log(MAX_THRESH)).  Since total_elements has type size_t, we get as
 * upper bound for log (total_elements):
 * bits per byte (CHAR_BIT) * sizeof(size_t)
 */
#define STACK_SIZE		(8 * sizeof(size_t))
#define PUSH(low, high) 	((void) ((top->lo = (low)), (top->hi = (high)), ++top))
#define POP(low, high)        	((void) (--top, (low = top->lo), (high = top->hi)))
#define STACK_NOT_EMPTY        	(stack < top)

/* Byte-wise swap two items of size SIZE.
 */
#define SWAP(a, b, size)                                                      \
  do                                                                          \
    {                                                                         \
      size_t __size = (size);                                        	      \
      char * __a;							      \
      char * __b;							      \
									      \
      __size = (size);                                        	      	      \
      __a = (a);							      \
      __b = (b);                                   	      		      \
									      \
      do                                                                      \
        {                                                                     \
          char __tmp;                                                         \
          		                                                      \
          __tmp = *__a;                                                       \
          *__a++ = *__b;                                                      \
          *__b++ = __tmp;                                                     \
        }								      \
      while (--__size > 0);                                                   \
    }									      \
  while (0)


void
quicksort (void * base,
	   size_t n_elts,
	   size_t sizeof_elt,
	   quicksort_cmp cmp,
	   void * closure)
{
  char * base_ptr;
  size_t max_thresh;

  if (n_elts == 0)
    return;

  base_ptr = (char *)base;
  max_thresh = MAX_THRESH * sizeof_elt;

  if (n_elts > MAX_THRESH)
    {
      struct stack_node stack[STACK_SIZE];
      struct stack_node *top;
      char *lo;
      char *hi;

      top = stack + 1;
      lo = base_ptr;
      hi = &lo[sizeof_elt * (n_elts - 1)];

      while (STACK_NOT_EMPTY)
        {
          char *left_ptr;
          char *right_ptr;
          char *mid;

          /* Select median value from among LO, MID, and HI. Rearrange
           * LO and HI so the three values are sorted. This lowers the
           * probability of picking a pathological pivot value and
           * skips a comparison for both the LEFT_PTR and RIGHT_PTR in
           * the while loops.
	   */

          mid = lo + sizeof_elt * ((hi - lo) / sizeof_elt >> 1);

          if ((*cmp) ((void *) mid, (void *) lo, closure) < 0)
	    {
	      SWAP (mid, lo, sizeof_elt);
	    }

          if ((*cmp) ((void *) hi, (void *) mid, closure) < 0)
	    {
	      SWAP (mid, hi, sizeof_elt);
	    }
          else
            goto jump_over;

	  if ((*cmp) ((void *) mid, (void *) lo, closure) < 0)
	    SWAP (mid, lo, sizeof_elt);
	  
        jump_over:

          left_ptr = lo + sizeof_elt;
          right_ptr = hi - sizeof_elt;

          /* Here's the famous ``collapse the walls'' section of quicksort.
           * Gotta like those tight inner loops!  They are the main reason
           * that this algorithm runs much faster than others.
	   */
          do
            {
              while ((*cmp) ((void *) left_ptr, (void *) mid, closure) < 0)
                left_ptr += sizeof_elt;

              while ((*cmp) ((void *) mid, (void *) right_ptr, closure) < 0)
                right_ptr -= sizeof_elt;

              if (left_ptr < right_ptr)
                {
                  SWAP (left_ptr, right_ptr, sizeof_elt);

                  if (mid == left_ptr)
                    mid = right_ptr;
                  else if (mid == right_ptr)
                    mid = left_ptr;

                  left_ptr += sizeof_elt;
                  right_ptr -= sizeof_elt;
                }
              else if (left_ptr == right_ptr)
                {
                  left_ptr += sizeof_elt;
                  right_ptr -= sizeof_elt;
                  break;
                }
            }
          while (left_ptr <= right_ptr);

          /* Set up pointers for next iteration.  First determine whether
           * left and right partitions are below the threshold size.  If so,
           * ignore one or both.  Otherwise, push the larger partition's
           * bounds on the stack and continue sorting the smaller one.
	   */

          if ((size_t) (right_ptr - lo) <= max_thresh)
            {
              if ((size_t) (hi - left_ptr) <= max_thresh)
		{
		  /* Ignore both small partitions.
		   */
		  POP (lo, hi);
		}
              else
		{
		  /* Ignore small left partition.
		   */
		  lo = left_ptr;
		}
            }
          else if ((size_t) (hi - left_ptr) <= max_thresh)
	    {
	      /* Ignore small right partition.
	       */
	      hi = right_ptr;
	    }
          else if ((right_ptr - lo) > (hi - left_ptr))
            {
              /* Push larger left partition indices.
	       */
              PUSH (lo, right_ptr);
              lo = left_ptr;
            }
          else
            {
              /* Push larger right partition indices.
	       */
              PUSH (left_ptr, hi);
              hi = right_ptr;
            }
        }
    }

  /* Once the BASE_PTR array is partially sorted by quicksort the rest
   * is completely sorted using insertion sort, since this is efficient
   * for partitions below MAX_THRESH size. BASE_PTR points to the beginning
   * of the array to sort, and END_PTR points at the very last element in
   * the array (*not* one beyond it!).
   */

#define min(x, y) ((x) < (y) ? (x) : (y))

  {
    char * end_ptr;
    char * tmp_ptr;
    char * thresh;
    char * run_ptr;

    end_ptr = &base_ptr[sizeof_elt * (n_elts - 1)];
    tmp_ptr = base_ptr;
    thresh = min(end_ptr, base_ptr + max_thresh);

    /* Find smallest element in first threshold and place it at the
     * array's beginning.  This is the smallest array element,
     * and the operation speeds up insertion sort's inner loop.
     */

    for (run_ptr = tmp_ptr + sizeof_elt; run_ptr <= thresh; run_ptr += sizeof_elt)
      if ((*cmp) ((void *) run_ptr, (void *) tmp_ptr, closure) < 0)
        tmp_ptr = run_ptr;

    if (tmp_ptr != base_ptr)
      {
	SWAP (tmp_ptr, base_ptr, sizeof_elt);
      }

    /* Insertion sort, running from left-hand-side up to right-hand-side.
     */

    run_ptr = base_ptr + sizeof_elt;

    while ((run_ptr += sizeof_elt) <= end_ptr)
      {
        tmp_ptr = run_ptr - sizeof_elt;

        while ((*cmp) ((void *) run_ptr, (void *) tmp_ptr, closure) < 0)
          tmp_ptr -= sizeof_elt;

        tmp_ptr += sizeof_elt;

        if (tmp_ptr != run_ptr)
          {
            char *trav;

            trav = run_ptr + sizeof_elt;
            while (--trav >= run_ptr)
              {
                char c;
                char * hi;
		char * lo;

                c = *trav;

                for (hi = lo = trav; (lo -= sizeof_elt) >= tmp_ptr; hi = lo)
                  *hi = *lo;

                *hi = c;
              }
          }
      }
  }
}

/* tag: Tom Lord Fri Feb 22 06:34:02 2002 (qsort.c)
 */

