/* bits.h - generic bitsets
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__BITSETS__BITS_H
#define INCLUDE__BITSETS__BITS_H



#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/bitsets/bitset-tree.h"

struct bits_tree_shared
{
  /* update bits-print.c when changing fields here */
  int refs;
  bits_tree tree;
};

struct bits
{
  /* update bits-print.c when changing fields here */
  alloc_limits lim;
  struct bits_tree_rule * rule;
  struct bits_tree_shared * stree;
};

typedef struct bits * bits;


/* automatically generated __STDC__ prototypes */
extern bits bits_alloc (alloc_limits lim, struct bits_tree_rule * rule);
extern void bits_free (bits b);
extern bits bits_dup (bits a);
extern int bits_get_subset (bitset_subset * answer, bits b, int n);
extern void bits_compact (bits a);
extern int bits_is_member (bits b, int n);
extern int bits_is_equal (bits a, bits b);
extern int bits_is_subset (bits a, bits b);
extern int bits_is_empty (bits a);
extern int bits_is_full (bits a);
extern int bits_is_empty_range (bits a, int from, int to);
extern int bits_is_full_range (bits a, int from, int to);
extern int bits_adjoin (bits b, int n);
extern int bits_remove (bits b, int n);
extern int bits_toggle (bits b, int n);
extern int bits_clear (bits b);
extern int bits_fill (bits b);
extern int bits_clear_range (bits b, int from, int to);
extern int bits_fill_range (bits b, int from, int to);
extern int bits_complement (bits b);
extern int bits_assign (bits a, bits b);
extern int bits_union (bits a, bits b);
extern int bits_intersection (bits a, bits b);
extern int bits_difference (bits a, bits b);
extern int bits_revdifference (bits a, bits b);
extern int bits_xor (bits a, bits b);
extern int bits_population (bits a);
extern int bits_population_range (bits a, int from, int to);
extern int bits_ffs (bits b);
extern int bits_ffc (bits b);
extern int bits_ffs_range (bits b, int from, int to);
extern int bits_ffc_range (bits b, int from, int to);
#endif  /* INCLUDE__BITSETS__BITS_H */
