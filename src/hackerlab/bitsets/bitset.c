/* bitset.c - bitsets
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/bitsets/bitset.h"


/************************************************************************
 *(h1 "Flat Bitsets"
 *	:includes ("hackerlab/bitset/bitset.h"))
 *
 * |subset|
 * |flat bitset|
 * 
 * A "bitset" (or "flat bitset") is a packed array whose elements are
 * each 1 bit.  A bitset can be considered a finite, ordered set of
 * numbered entities.
 *
 * The type `bitset_subset' is a bitset small enough to fit in a
 * variable of type `void *'.  (See xref:"Portability Assumptions in
 * the Hackerlab C Library".
 *
 * The type `bitset' is pointer to `bitset_subset'.  Bitset functions
 * generally operate on a pair of values: the `size' (measured in
 * bits, an integer of type `bit_t') and the `set' (a pointer of type
 * `bitset_subset *').  
 * 
 * |ranges (in bitset functions)| Some functions operate on an
 * arbitrary subsequence of a bitset.  Their arguments include a
 * starting bit (`from'), and an ending bit (`to'); these have names
 * that end in the suffix `_range'.  When a function operates on a
 * range of bits, the range is specified as a half-open interval.  For
 * example,
 * 
 * 	bitset_clear_range (bits, from, to)
 * 
 * changes the bits numbered:
 *
 *	from ... to - 1
 *
 * and the bit numbered `to' is unaffected.  This permits bit addresses
 * to be used similarly to pointers:  a range of `n' bits beginning
 * at bit `pos' is:
 * 
 * 	pos, pos + n
 *
 * Bitsets are ordinarilly allocated in sizes rounded up to the
 * nearest `sizeof (bitset_subset)'.  Operations on bitsets, however,
 * are precise: they modify only the bits in a bitset and no others.
 * For that reason, it is safe to operate on a bitset with `N'
 * elements as if it had only `M' elements (with `M < N').  Bits
 * `M..N-1' will be unaffected.
 */
/*(menu)
 */

/*(include-documentation "bitset.h")
 */


/* bitset_access(B,N,OP)
 *
 * Apply OP to the subset of bitset `B' containing element `N' and to
 * a singleton subset containing a 1-bit in the position where element
 * `N' resides.
 */
#define bitset_access(B,N,OP) 	((B)[bitset_which_subset(N)] OP (1 << ((N) & bitset_subset_mask)))


/************************************************************************
 *(h2 "Allocating and Freeing Bitsets")
 * 
 */


/*(c bitset_alloc)
 * bitset bitset_alloc (alloc_limits limits, bit_t size);
 * 
 * Allocate an empty bitset large enough to hold `size' members.
 *
 * Allocation is performed by `lim_malloc' using `limits'.  See
 * xref:"Allocation With Limitations".
 */
bitset
bitset_alloc (alloc_limits limits, bit_t size)
{
  bitset b;
  b = (bitset) lim_malloc (limits, sizeof_bitset (size));
  bitset_clear (size, b);
  return b;
}


/*(c bitset_free)
 * void bitset_free (alloc_limits limits, bitset a);
 * 
 * Free the bitset `a'.
 * 
 * Deallocation is performed by `lim_free' using `limits'.  See
 * xref:"Allocation With Limitations".
 */
void
bitset_free (alloc_limits limits, bitset a)
{
  lim_free (limits, a);
}


/*(c bitset_dup)
 * bitset bitset_dup (alloc_limits limits, bit_t size, bitset a);
 * 
 * Allocate a copy of bitset `a' which has `size' members.
 *
 * Allocation is performed by `lim_malloc' using `limits'.  See
 * xref:"Allocation With Limitations".
 */
bitset
bitset_dup (alloc_limits limits, bit_t size, bitset a)
{
  bitset cs;

  cs = lim_malloc (limits, sizeof_bitset (size));
  bitset_assign (size, cs, a);
  return cs;
}



/************************************************************************
 *(h2 "Tests on Bitsets")
 * 
 * 
 * 
 */



/*(c bitset_is_member)
 * int bitset_is_member (bitset b, bit_t n);
 * 
 * Return bit `n' of bitset `b' (either 0 or 1).
 */
int
bitset_is_member (bitset b, bit_t n)
{
  return !!bitset_access((b), (n), &);
}


/*(c bitset_is_equal)
 * int bitset_is_equal (bit_t size, bitset a, bitset b);
 * 
 * Compare two bitsets for equality.  Both bitsets have `size'
 * members.  Return 1 if they are equal, 0 otherwise.
 */
int
bitset_is_equal (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;

  if (!size)
    return 1;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  if ((a[x] & last_bitset_subset_mask) != (b[x] & last_bitset_subset_mask))
    return 0;

  while (x--)
    {
      if (a[x] != b[x])
	return 0;
    }

  return 1;
}


/*(c bitset_is_subset)
 * int bitset_is_subset (bit_t size, bitset a, bitset b);
 * 
 * Return 1 if bitset `b' is a subset of bitset `a', 0 otherwise.
 */
int
bitset_is_subset (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  bitset_subset a_bits;
  bitset_subset b_bits;

  if (!size)
    return 1;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  a_bits = (a[x] & last_bitset_subset_mask);
  b_bits = (b[x] & last_bitset_subset_mask);
  if ((a_bits | b_bits) != a_bits)
    return 0;

  while (x-- && ((a[x] | b[x]) == a[x]))
    ;

  return x == -1;
}


/*(c bitset_is_empty)
 * int bitset_is_empty (bit_t size, bitset a);
 * 
 * Return 1 if bitset `a' has no members.
 */
int
bitset_is_empty (bit_t size, bitset a)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;

  if (!size)
    return 1;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  if (a[x] & last_bitset_subset_mask)
    return 0;

  while (x-- && !a[x])
    ;

  return x == -1;
}


/*(c bitset_is_empty_range)
 * int bitset_is_empty_range (bitset a, bit_t from, bit_t to);
 * 
 * Return 1 if bitset `a' has no members in the closed 
 * interval `[from ... to-1]'.
 */
int
bitset_is_empty_range (bitset a, bit_t from, bit_t to)
{
  bit_t first_subset;
  bitset_subset first_bitset_subset_mask;
  bit_t last_subset;
  bitset_subset last_bitset_subset_mask;
  bit_t x;

  if (to <= from)
    return 1;

  --to;
  
  first_subset = bitset_which_subset (from);
  last_subset = bitset_which_subset (to);
  
  first_bitset_subset_mask  = ~(bitset_subset)0 << bitset_which_bit (from);
  last_bitset_subset_mask  = ~(bitset_subset)0 >> (bits_per_subset - (1 + bitset_which_bit (to)));

  if (first_subset == last_subset)
    return !(a[first_subset] & first_bitset_subset_mask & last_bitset_subset_mask);

  if (a[first_subset] & first_bitset_subset_mask)
    return 0;

  if (a[last_subset] & last_bitset_subset_mask)
    return 0;

  for (x = first_subset + 1; x < last_subset; ++x)
    if (a[x])
      return 0;

  return 1;
}



/*(c bitset_is_full)
 * int bitset_is_full (bit_t size, bitset a);
 * 
 * Return 1 if bitset `a' is filled (all ones).
 */
int
bitset_is_full (bit_t size, bitset a)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;

  if (!size)
    return 1;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  if ((a[x] & last_bitset_subset_mask) != last_bitset_subset_mask)
    return 0;

  while (x-- && (a[x] == (bitset_subset)-1L))
    ;

  return x == -1;
}


/*(c bitset_is_full_range)
 * int bitset_is_full_range (bitset a, bit_t from, bit_t to);
 * 
 * Return 1 if bitset `a' is filled in the closed 
 * interval `[from .. to-1]'.
 */
int
bitset_is_full_range (bitset a, bit_t from, bit_t to)
{
  bit_t first_subset;
  bitset_subset first_bitset_subset_mask;
  bit_t last_subset;
  bitset_subset last_bitset_subset_mask;
  bit_t x;

  if (to <= from)
    return 1;

  first_subset = bitset_which_subset (from);
  last_subset = bitset_which_subset (to);
  first_bitset_subset_mask = ~((bitset_subset)(1 << (from & bitset_subset_mask)) - 1);
  last_bitset_subset_mask = ((bitset_subset)(1 << (to & bitset_subset_mask)) - 1);

  if (first_subset == last_subset)
    return (   (a[first_subset] & first_bitset_subset_mask & last_bitset_subset_mask)
	    == (first_bitset_subset_mask & last_bitset_subset_mask));

  if ((a[first_subset] & first_bitset_subset_mask) != first_bitset_subset_mask)
    return 0;

  if ((a[last_subset] & last_bitset_subset_mask) != last_bitset_subset_mask)
    return 0;

  for (x = first_subset + 1; x < last_subset - 1; ++x)
    if (a[x] != (bitset_subset)-1L)
      return 0;

  return 1;
}


/************************************************************************
 *(h2 "Set Operations")
 * 
 * 
 * 
 */


/*(c bitset_adjoin)
 * void bitset_adjoin (bitset b, bit_t n);
 * 
 * Set bit `n' of bitset `b' to 1.
 */
void
bitset_adjoin (bitset b, bit_t n)
{
  bitset_access((b), (n), |=);
}


/*(c bitset_remove)
 * void bitset_remove (bitset b, bit_t n);
 * 
 * Set bit `n' of bitset `b' to 0.
 */
void
bitset_remove (bitset b, bit_t n)
{
  bitset_access((b), (n), &= ~);
}


/*(c bitset_toggle)
 * void bitset_toggle (bitset b, bit_t n);
 * 
 * If bit `n' of bitset `b' is 1, set it to 0;  if 0,
 * set it to 1.
 */
void
bitset_toggle (bitset b, bit_t n)
{
  bitset_access((b), (n), ^= );
}


/*(c bitset_clear)
 * void bitset_clear (bit_t size, bitset b);
 * 
 * Clear all bits of bitset `b'.
 */
void
bitset_clear (bit_t size, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  b[x] &= ~last_bitset_subset_mask;
  mem_set ((char *)b, 0, x * sizeof (bitset_subset));
}



/*(c bitset_clear_range)
 * void bitset_clear_range (bitset b, bit_t from, bit_t to);
 * 
 * Clear the bits `from .. to - 1' of bitset `b'.
 */
void
bitset_clear_range (bitset b, bit_t from, bit_t to)
{
  bitset bp;

  while ((from < to) && (from % bits_per_subset))
    {
      bitset_remove (b, from);
      ++from;
    }

  bp = b + bitset_which_subset (from);
  while (from + bits_per_subset < to)
    {
      *bp = (bitset_subset)0;
      ++bp;
      from += bits_per_subset;
    }

  while (from < to)
    {
      bitset_remove (b, from);
      ++from;
    }
}


/*(c bitset_fill)
 * void bitset_fill (bit_t size, bitset b);
 * 
 * Set all bits of bitset `b'.
 */
void
bitset_fill (bit_t size, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  b[x] |= last_bitset_subset_mask;
  mem_set ((t_uchar *)b, 0xff, x * sizeof (bitset_subset));
}


/*(c bitset_fill_range)
 * void bitset_fill_range (bitset b, bit_t from, bit_t to);
 * 
 * Set the bits `from .. to - 1' of bitset `b'.
 */
void
bitset_fill_range (bitset b, bit_t from, bit_t to)
{
  bitset bp;

  while ((from < to) && (from % bits_per_subset))
    {
      bitset_adjoin (b, from);
      ++from;
    }

  bp = b + bitset_which_subset (from);
  while (from + bits_per_subset < to)
    {
      *bp = ~(bitset_subset)0;
      ++bp;
      from += bits_per_subset;
    }

  while (from < to)
    {
      bitset_adjoin (b, from);
      ++from;
    }
}


/*(c bitset_complement)
 * void bitset_complement (bit_t size, bitset b);
 * 
 * Toggle all bits in bitset `b'.
 */
void
bitset_complement (bit_t size, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  b[x] = ((b[x] & ~last_bitset_subset_mask) | (~b[x] & last_bitset_subset_mask));

  while (x--)
    {
      *b = ~*b;
      ++b;
    }
}


/*(c bitset_assign)
 * void bitset_assign (bit_t size, bitset a, bitset b);
 * 
 * Copy bitset `b' to bitset `a'.
 */
void
bitset_assign (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);

  a[x] = (a[x] & ~last_bitset_subset_mask) | (b[x] & last_bitset_subset_mask);

  while (x--)
    a[x] = b[x];
}


/*(c bitset_union)
 * void bitset_union (bit_t size, bitset a, bitset b);
 * 
 * Add all elements of bitset `b' to bitset `a'.
 */
void
bitset_union (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  a[x] |= (b[x] & last_bitset_subset_mask);

  while (x--)
    a[x] |= b[x];
}


/*(c bitset_intersection)
 * void bitset_intersection (bit_t size, bitset a, bitset b);
 * 
 * Remove from bitset `a' all alements that are not also in bitset
 * `b'.
 */
void
bitset_intersection (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  a[x] = (a[x] & ~last_bitset_subset_mask) | ((a[x] & b[x]) & last_bitset_subset_mask);

  while (x--)
    a[x] &= b[x];
}


/*(c bitset_difference)
 * void bitset_difference (bit_t size, bitset a, bitset b);
 * 
 * Remove from bitset `a' all alements that are in bitset `b'.
 */
void
bitset_difference (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  a[x] = (a[x] & ~last_bitset_subset_mask) | ((a[x] & ~b[x]) & last_bitset_subset_mask);

  while (x--)
    a[x] &= ~ b[x];
}


/*(c bitset_revdifference)
 * void bitset_revdifference (bit_t size, bitset a, bitset b);
 * 
 * Set all bits in `a' that are set in bitset `b' but not in `a';
 * clear all other bits of bitset `a'.
 *
 * This is similar to `bitset_difference (size, b, a)' except that
 * the result is assigned to bitset `a' instead of bitset `b'.
 */
void
bitset_revdifference (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  a[x] = (a[x] & ~last_bitset_subset_mask) | ((~a[x] & b[x]) & last_bitset_subset_mask);

  while (x--)
    a[x] = ~a[x] & b[x];
}


/*(c bitset_xor)
 * void bitset_xor (bit_t size, bitset a, bitset b);
 * 
 * Toggle all bits in bitset `a' that are set in bitset `b'.
 */
void
bitset_xor (bit_t size, bitset a, bitset b)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  
  if (!size)
    return;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  a[x] ^= (b[x] & last_bitset_subset_mask);

  while (x--)
    a[x] ^= b[x];
}


/************************************************************************
 *(h2 "Population Size")
 * 
 * 
 * 
 */


/* 
 * (define l (let loop ((x 0) (l '())) (if (eq? x 256) l (loop (+ x 1) (cons x l)))))
 * (define lb (map (lambda (n) (number->string n 2)) l))
 * (define lc (map string->list lb))
 * (define ln (map (lambda (l) (map (lambda (c) (if (eq? c #\1) 1 0)) l)) lc))
 * (define lt (map (lambda (l) (apply + l)) ln))
 */
static int bitset_byte_populations[256] = 
{
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};


/*(c bitset_population)
 * bit_t bitset_population (bit_t size, bitset a);
 * 
 * Return the number of bits set in bitset `a'.
 */
bit_t
bitset_population (bit_t size, bitset a)
{
  bit_t x;
  bit_t bits_in_last_subset;
  bitset_subset last_bitset_subset_mask;
  bitset_subset a_subset;
  bit_t total;
  
  if (!size)
    return 0;

  x = bitset_numb_subsets(size) - 1;
  bits_in_last_subset = (size - bits_per_subset * x);
  last_bitset_subset_mask = ~(bitset_subset)0 >> (bits_per_subset - bits_in_last_subset);
  total = 0;
  a_subset = a[x] & last_bitset_subset_mask;
  while (a_subset)
    {
      total += bitset_byte_populations[a_subset & 0xff];
      a_subset >>= 8;
    }
  while (x--)
    {
      a_subset = a[x];
      while (a_subset)
	{
	  total += bitset_byte_populations[a_subset & 0xff];
	  a_subset >>= 8;
	}
    }
  return total;
}     



/*(c bitset_population_range)
 * bit_t bitset_population (bitset a, bit_t from, bit_t to);
 * 
 * Return the number of bits set in bitset `a' couning only
 * members in the closed interval `[from .. to-1]'.
 */
bit_t
bitset_population_range (bitset a, bit_t from, bit_t to)
{
  bit_t first_subset;
  bit_t first_subset_offset;
  bit_t overcount;
  bit_t count_error;


  if (to <= from)
    return 0;

  first_subset = bitset_which_subset (from);
  first_subset_offset = first_subset * bits_per_subset;
  overcount = bitset_population (to - first_subset_offset, a + first_subset);
  count_error = bitset_population (from - first_subset_offset, a + first_subset);
  return overcount - count_error;
}


/************************************************************************
 *(h2 "First Bit Set -- First Bit Clear")
 * 
 * 
 * 
 */

static int bitset_byte_ffs[256] =
{
  -1, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
  4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0
};


/* static bit_t subset_ffs (bitset_subset a);
 * 
 * Return the address of the first bit set in a bitset_subset.
 * For bitset_subset 0, return -1.
 */
static bit_t
subset_ffs (bitset_subset a)
{
  bit_t offset;
  bit_t x;

  if (!a)
    return -1;

  offset = 0;
  x = sizeof (a);
  while (x--)
    {
      bit_t q;
      q = bitset_byte_ffs [a & 0xff];
      if (q >= 0)
	return q + offset;
      offset += 8;
      a >>= 8;
    }

  while (1)
    panic ("not reached");
}


/*(c bitset_ffs)
 * bit_t bitset_ffs (bit_t size, bitset b);
 * 
 * Return the (bit) address of the first bit set in bitset `b'.
 * Return -1 if no bit is set.
 */
bit_t
bitset_ffs (bit_t size, bitset b)
{
  bit_t offset;
  bit_t x;
  bit_t bound;
  
  offset = 0;
  x = 0;
  bound = bitset_numb_subsets(size);

  while (x < bound)
    {
      bit_t fs;

      fs = subset_ffs (b[x]);

      if (fs >= 0)
	{
	  bit_t answer;
	  answer = offset + fs;
	  return (answer < size) ? answer : -1;
	}

      offset += bits_per_subset;
      ++x;
    }
  return -1;
}


/*(c bitset_ffs_range)
 * bit_t bitset_ffs_range (bitset b, bit_t from, bit_t to);
 * 
 * Return the (bit) address of the first bit set in bitset `b'
 * in the range `from .. to-1'.  Return -1 if no bit is set.
 */
bit_t
bitset_ffs_range (bitset b, bit_t from, bit_t to)
{
  bit_t n_bits;
  bit_t first_subset_skip;
  bit_t f;
  bit_t answer;

  n_bits = to - from;
  first_subset_skip = (from % bits_per_subset);
  if (first_subset_skip)
    {
      bitset_subset s;
      bit_t first_in_s;

      s = b[bitset_which_subset (from)];
      s &= ~((1 << first_subset_skip) - 1);
      first_in_s = subset_ffs (s);
      if (first_in_s != -1)
	{
	  answer = first_in_s + from - first_subset_skip;
	  return (answer < (from + n_bits)) ? answer : -1;
	}
      from += bits_per_subset - first_subset_skip;
      n_bits -= bits_per_subset - first_subset_skip;
    }
  f = bitset_ffs (n_bits, b + bitset_which_subset (from));
  if (f < 0)
    return f;
  answer = from + f;
  return (answer < (from + n_bits)) ? answer : -1;
}




static int bitset_byte_ffc[256] =
{
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 6,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 7,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 6,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, -1
};


/* static bit_t subset_ffc (bitset_subset a);
 * 
 * Return the address of the first bit set in a subset.
 * For bitset_subset 0, return -1.
 */
static bit_t
subset_ffc (bitset_subset a)
{
  bit_t offset;
  bit_t x;

  if (a == (bitset_subset)-1L)
    return -1;

  offset = 0;
  x = sizeof (a);
  while (x--)
    {
      bit_t q;
      q = bitset_byte_ffc [a & 0xff];
      if (q >= 0)
	return q + offset;
      offset += 8;
      a >>= 8;
    }

  while (1)
    panic ("not reached");
}

/*(c bitset_ffc)
 * bit_t bitset_ffc (bit_t size, bitset b);
 *
 * Return the (bit) address of the first bit clear in bitset `b'.
 * Return -1 if no bit is clear.
 */
bit_t
bitset_ffc (bit_t size, bitset b)
{
  bit_t offset;
  bit_t x;
  bit_t bound;
  
  offset = 0;
  x = 0;
  bound = bitset_numb_subsets(size);

  while (x < bound)
    {
      bit_t fs;

      fs = subset_ffc (b[x]);

      if (fs >= 0)
	{
	  bit_t answer;
	  answer = offset + fs;
	  return (answer < size) ? answer : -1;
	}

      offset += bits_per_subset;
      ++x;
    }
  return -1;
}


/*(c bitset_ffc_range)
 * bit_t bitset_ffc_range (bitset b, bit_t from, bit_t to);
 * 
 * Return the (bit) address of the first bit clear in bitset `b'
 * in the range `from .. to-1'.  Return -1 if no bit is set.
 */
bit_t
bitset_ffc_range (bitset b, bit_t from, bit_t to)
{
  bit_t n_bits;
  bit_t first_subset_skip;
  bit_t f;
  bit_t answer;

  n_bits = to - from;
  first_subset_skip = (from % bits_per_subset);
  if (first_subset_skip)
    {
      bitset_subset s;
      bit_t first_in_s;

      s = b[bitset_which_subset (from)];
      s |= ((1 << first_subset_skip) - 1);
      first_in_s = subset_ffc (s);
      if (first_in_s != -1)
	{
	  answer = first_in_s + from - first_subset_skip;
	  return (answer < (from + n_bits)) ? answer : -1;
	}
      from += bits_per_subset - first_subset_skip;
      n_bits -= bits_per_subset - first_subset_skip;
    }
  f = bitset_ffc (n_bits, b + bitset_which_subset (from));
  if (f < 0)
    return f;
  answer = from + f;
  return (answer < (from + n_bits)) ? answer : -1;
}

