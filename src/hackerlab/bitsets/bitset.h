/* bitset.h - bitset decls
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/limits.h"
#include "hackerlab/mem/alloc-limits.h"


#ifndef INCLUDE__BITSETS__BITSET_H
#define INCLUDE__BITSETS__BITSET_H

/************************************************************************
 *(h2 "Bitset Types and Macros")
 * 
 */ 

/*(c bit_t :category type)
 * typedef long bit_t;
 * 
 * Values of type `bit_t' represent a bit address within
 * a bitset.  The address of the first bit is 0.
 * 
 * `bit_t' is a signed integer type.  Some functions which return a
 * bit address use the value -1 to indicate `no such bit'.
 * 
 */
typedef long bit_t;		/* A bit address. */

/*end-insert
 */

/*(c bitset_subset :category type)
 * typedef unsigned long bitset_subset;
 * 
 * A fragment of a bitset.  A bitset is an array of these.
 */
typedef unsigned long bitset_subset;

/*(c bitset :category type)
 * typedef bitset_subset * bitset;
 * 
 * A packed array of bits.
 */
typedef bitset_subset * bitset;


/*c bits_per_subset :category macro)
 * #define bits_per_subset	(8 * sizeof (bitset_subset))
 *
 * The number of bits in one value of type `subset'.  The
 * implementation presumes that `sizeof (bitset_subset)' is a power of two.
 */
#if ULONG_MAX == 0xffffffff
#define bits_per_subset		(32)
#elif ULONG_MAX == ((0xffffffff << 32) + 0xffffffff)
#define bits_per_subset		(64)
#else
#error "odd ULONG_MAX in bitset.h"
#endif

/*c bitset_subset_mask :category macro)
 * #define bitset_subset_mask		(bits_per_subset - 1)
 *
 * A bitmask useful for finding the bit position within a subset
 * of a bitset element.
 */
#define bitset_subset_mask		(bits_per_subset - 1)

/*(c bitset_which_subset :category macro)
 * #define bitset_which_subset(N)  ((N) / bits_per_subset)
 *
 * A macro useful for finding the subset index within a bitset of a
 * particular bitset element.  The subset containing bit `N' bit
 * bitset `B' is:
 * 
 * 	B[bitset_which_subset(N)]
 */
#define bitset_which_subset(N)  ((N) / bits_per_subset)


/*(c bitset_subset_offset :category macro)
 * #define bitset_subset_offset(N) \
 *	  (((N) / bits_per_subset) * bits_per_subset)
 *
 * A macro useful for finding a subset offset within a bitset of the
 * subset containing a bitset element.  Bit 0 (the low order bit) of
 * the subset containing bit `N' is the bit whose bit address is
 * `bitset_subset_offset(N)'.
 */
#define bitset_subset_offset(N)  (((N) / bits_per_subset) * bits_per_subset)

/*(c bitset_which_bit :category macro)
 * #define bitset_which_bit(N)  ((N) & bitset_subset_mask)
 *
 * The bit number of a bitset index within its subset.
 */
#define bitset_which_bit(N)  ((N) & bitset_subset_mask)


/*c bitset_bit_mask :category macro)
 * #define bitset_bit_mask(N) \
 *   ((~(bitset_subset)0L) >> (bits_per_subset - bitset_which_bit (N)))
 * 
 * A bitmask for all bits up to (but not including) element `N' of the
 * subset containing element `N'.
 */
#define bitset_bit_mask(N)   ((~(bitset_subset)0L) >> (bits_per_subset - bitset_which_bit (N)))


/*(c bitset_numb_subsets :category macro)
 * #define bitset_numb_subsets(N)
 *
 * As a function:
 *
 * 	bit_t bitset_numb_subsets (bit_t n);
 * 
 * Return the number of values of type `subset' necessary to represent
 * a bitset with `n' elements. Because this is a macro, it can be used
 * in a declaration:
 *
 *	{
 *	  // declare a bitset that can hold 12 elements:
 *	  //
 *	  bitset_subset options_set [bitset_numb_subsets(12)];
 *	}
 */
#define bitset_numb_subsets(n) (((n) + bits_per_subset - 1) / bits_per_subset)

/*(c sizeof_bitset :category macro)
 * #define sizeof_bitset(N)
 *
 * As a function:
 *
 * 	size_t sizeof_bitset (bit_t n);
 * 
 * Return the size, in bytes, of a bitset large enough to
 * hold `n' elements:
 * 
 *	// allocate a bitset that can hold 12 elements:
 *	//
 *	options_set = (bitset)must_malloc (sizeof_bitset (12));
 */
#define sizeof_bitset(n) (bitset_numb_subsets(n) * sizeof(bitset_subset))



/* automatically generated __STDC__ prototypes */
extern bitset bitset_alloc (alloc_limits limits, bit_t size);
extern void bitset_free (alloc_limits limits, bitset a);
extern bitset bitset_dup (alloc_limits limits, bit_t size, bitset a);
extern int bitset_is_member (bitset b, bit_t n);
extern int bitset_is_equal (bit_t size, bitset a, bitset b);
extern int bitset_is_subset (bit_t size, bitset a, bitset b);
extern int bitset_is_empty (bit_t size, bitset a);
extern int bitset_is_empty_range (bitset a, bit_t from, bit_t to);
extern int bitset_is_full (bit_t size, bitset a);
extern int bitset_is_full_range (bitset a, bit_t from, bit_t to);
extern void bitset_adjoin (bitset b, bit_t n);
extern void bitset_remove (bitset b, bit_t n);
extern void bitset_toggle (bitset b, bit_t n);
extern void bitset_clear (bit_t size, bitset b);
extern void bitset_clear_range (bitset b, bit_t from, bit_t to);
extern void bitset_fill (bit_t size, bitset b);
extern void bitset_fill_range (bitset b, bit_t from, bit_t to);
extern void bitset_complement (bit_t size, bitset b);
extern void bitset_assign (bit_t size, bitset a, bitset b);
extern void bitset_union (bit_t size, bitset a, bitset b);
extern void bitset_intersection (bit_t size, bitset a, bitset b);
extern void bitset_difference (bit_t size, bitset a, bitset b);
extern void bitset_revdifference (bit_t size, bitset a, bitset b);
extern void bitset_xor (bit_t size, bitset a, bitset b);
extern bit_t bitset_population (bit_t size, bitset a);
extern bit_t bitset_population_range (bitset a, bit_t from, bit_t to);
extern bit_t bitset_ffs (bit_t size, bitset b);
extern bit_t bitset_ffs_range (bitset b, bit_t from, bit_t to);
extern bit_t bitset_ffc (bit_t size, bitset b);
extern bit_t bitset_ffc_range (bitset b, bit_t from, bit_t to);
#endif  /* INCLUDE__BITSETS__BITSET_H */
