/* tag: Tom Lord Tue Dec  4 14:41:34 2001 (bits.c)
 */
/* bits.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bitsets/bits.h"


/************************************************************************
 *(h1 "Shared Bitset Trees"
 *    :includes ("hackerlab/bitsets/bits.h"))
 * 
 * "Shared bitset trees" are ordinary bitset trees with two differences:
 * 
 * \1./ The allocation limits that apply to a shared bitset tree are
 * recorded when the tree is created and do not need to be passed as
 * parameters to every bitset operation.  (For more information about
 * allocation limits, see xref:"Allocation With Limitations".
 * 
 * \2./ When a shared bitset tree is copied, very little data is actually
 * duplicated:  the old and the new bitset tree initially share state.
 * Instead, copying takes place when (and if) either bitset is later
 * modified.
 * 
 * Before reading this section, it is a good idea to first understand
 * the material in xref:"Bitset Trees".
 */




/************************************************************************
 *(h2 "Allocating Shared Bitset Trees")
 * 
 * 
 * 
 */

/*(c bits_alloc)
 * bits bits_alloc (alloc_limits lim, struct bits_tree_rule * rule);
 * 
 * Create a new shared bitset tree, subject to allocation limits `lim',
 * using branching structure `rule'.
 * 
 * For more information about allocation limits, see xref:"Allocation
 * With Limitations".
 * 
 * For more information about branching structure rules, see
 * xref:"Bitset Tree Rules".
 */
bits
bits_alloc (alloc_limits lim, struct bits_tree_rule * rule)
{
  bits b;

  b = (bits)lim_malloc (lim, sizeof (struct bits));
  if (!b)
    return 0;
  b->lim = lim;
  b->rule = rule;
  b->stree = (struct bits_tree_shared *)lim_malloc (lim, sizeof (struct bits_tree_shared));
  b->stree->refs = 1;
  b->stree->tree = bits_tree_alloc (lim, rule);
  return b;
}


/*(c bits_free)
 * void bits_free (bits b);
 * 
 * Free a previously allocated shared bitset tree.
 */
void
bits_free (bits b)
{
  if (!b)
    return;
  --b->stree->refs;
  if (!b->stree->refs)
    {
      bits_tree_free (b->lim, b->rule, b->stree->tree);
      lim_free (b->lim, (void *)b->stree);
    }
  lim_free (b->lim, b);
}


/*(c bits_dup)
 * bits bits_dup (bits a);
 * 
 * Copy a shared bitset tree.
 * 
 * This operation is inexpensive -- most data is shared between the
 * two trees until one of the two is modified.
 * 
 * If set `a' was created with no allocation limits, and allocation
 * fails, this function does not return.
 * 
 * If set `a' was created with allocation limits, and allocation
 * fails, this function returns 0.
 */
bits
bits_dup (bits a)
{
  bits b;

  b = (bits)lim_malloc (a->lim, sizeof (struct bits));
  if (!b)
    return 0;
  b->lim = a->lim;
  b->rule = a->rule;
  b->stree = a->stree;
  ++b->stree->refs;
  return b;
}

static int
bits_cow (bits a)
{
  if (a->stree->refs == 1)
    return 0;
  else
    {
      struct bits_tree_shared * s;
      s = (struct bits_tree_shared *)lim_malloc (a->lim, sizeof (struct bits_tree_shared));
      if (!s)
	return -1;
      s->refs = 1;
      s->tree = bits_tree_dup (a->lim, a->rule, a->stree->tree);
      if (!s->tree)
	{
	  lim_free (a->lim, (void *)s);
	  return -1;
	}
      --a->stree->refs;
      a->stree = s;
      return 0;
    }
}

/*(c bits_compact)
 * void bits_compact (bits a);
 * 
 * Optimize a shared bitset tree by compacting homogenous
 * sub-trees.  See xref:"The Bitset Tree Data Structure".
 * 
 */
void
bits_compact (bits a)
{
  bits_tree_compact (a->lim, a->rule, a->stree->tree);
}



/************************************************************************
 *h2 "Accessing Individual Subsets of a Shared Bitset Tree")
 * 
 * 
 * 
 */


/*c bits_get_subset)
 * int bits_get_subset (bitset_subset * answer, bits b, int n);
 * 
 * Return (in `*answer') the `bitset_subet' containing bit `n' of
 * shared bitset tree `b'.  See xref:"bitset_subset".
 * 
 * If the indicated subset does not exist (because it is part of
 * a homogenous subtree whose representation has been optimized)
 * the corresponding subtree is newly allocated.
 * 
 * If allocation fails, and `b' was created with allocation limits, 
 * return -1.
 * 
 * If allocation fails, and `b' was created without allocation limits,
 * this function does not return.
 * 
 * Otherwise, 0 is returned.
 * 
 */
int
bits_get_subset (bitset_subset * answer, bits b, int n)
{
  return bits_tree_get_subset (answer, b->lim, b->rule, b->stree->tree, n);
}



/************************************************************************
 *(h2 "Operations on Shared Bitset Trees")
 * 
 * 
 * Each of the operations defined for flat bitsets has a corresponding
 * operation for shared bitset trees.  See xref:"Flat Bitsets".
 * 
 * The shared bitset tree operations are:
 * 
 * 
 * |$bits_is_member| |$bits_is_equal| |$bits_is_subset| |$bits_is_empty| |$bits_is_full|
 * |$bits_is_empty_range| |$bits_is_full_range| |$bits_adjoin| |$bits_remove|
 * |$bits_toggle| |$bits_clear| |$bits_fill| |$bits_clear_range| |$bits_fill_range|
 * |$bits_complement| |$bits_assign| |$bits_union| |$bits_intersection|
 * |$bits_difference| |$bits_revdifference| |$bits_xor| |$bits_population|
 * |$bits_population_range| |$bits_ffs| |$bits_ffc| |$bits_ffs_range| |$bits_ffc_range|
 * 
 *     int bits_is_member (bits b, int n);
 *     int bits_is_equal (bits a, bits b);
 *     int bits_is_subset (bits a, bits b);
 *     int bits_is_empty (bits a);
 *     int bits_is_full (bits a);
 *     int bits_is_empty_range (bits a, int from, int to);
 *     int bits_is_full_range (bits a, int from, int to);
 *     int bits_adjoin (bits b, int n);
 *     int bits_remove (bits b, int n);
 *     int bits_toggle (bits b, int n);
 *     int bits_clear (bits b);
 *     int bits_fill (bits b);
 *     int bits_clear_range (bits b, int from, int to);
 *     int bits_fill_range (bits b, int from, int to);
 *     int bits_complement (bits b);
 *     int bits_assign (bits a, bits b);
 *     int bits_union (bits a, bits b);
 *     int bits_intersection (bits a, bits b);
 *     int bits_difference (bits a, bits b);
 *     int bits_revdifference (bits a, bits b);
 *     int bits_xor (bits a, bits b);
 *     int bits_population (bits a);
 *     int bits_population_range (bits a, int from, int to);
 *     int bits_ffs (bits b);
 *     int bits_ffc (bits b);
 *     int bits_ffs_range (bits b, int from, int to);
 *     int bits_ffc_range (bits b, int from, int to);
 *    
 * Each function performs the same operation as the corresponding
 * `bitset_' function (replace `bits_' with `bitset_'.)
 * For documentation, see xref:"Flat Bitsets".  For that reason,
 * the `bits_' functions are not individually documented.
 * 
 * These functions:
 * 
 *    bits_adjoin
 *    bits_remove
 *    bits_toggle
 *    bits_clear
 *    bits_fill
 *    bits_clear_range
 *    bits_fill_range
 *    bits_complement
 *    bits_assign
 *    bits_union
 *    bits_intersection
 *    bits_difference
 *    bits_revdifference
 *    bits_xor
 * 
 * return a value of type `int'. All of them will sometimes allocate
 * memory. 
 * 
 * If no allocation limit is being used, and an allocation fails,
 * these functions do not return.
 * 
 * If an allocation limit is being used, and an allocation fails,
 * these functions return -1 and have indeterminate side effect on the
 * set being operated upon.
 * 
 * If allocation succeeds, they return 0 (and have the intended side
 * effect on the set being operated upon).
 * 
 */

int
bits_is_member (bits b, int n)
{
  return bits_tree_is_member (b->lim, b->rule, b->stree->tree, n);
}


int
bits_is_equal (bits a, bits b)
{
  return bits_tree_is_equal (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_is_subset (bits a, bits b)
{
  return bits_tree_is_subset (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_is_empty (bits a)
{
  return bits_tree_is_empty (a->lim, a->rule, a->stree->tree);
}


int
bits_is_full (bits a)
{
  return bits_tree_is_full (a->lim, a->rule, a->stree->tree);
}


int
bits_is_empty_range (bits a, int from, int to)
{
  return bits_tree_is_empty_range (a->lim, a->rule, a->stree->tree, from, to);
}


int
bits_is_full_range (bits a, int from, int to)
{
  return bits_tree_is_full_range (a->lim, a->rule, a->stree->tree, from, to);
}


int
bits_adjoin (bits b, int n)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_adjoin (b->lim, b->rule, b->stree->tree, n);
}


int
bits_remove (bits b, int n)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_remove (b->lim, b->rule, b->stree->tree, n);
}


int
bits_toggle (bits b, int n)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_toggle (b->lim, b->rule, b->stree->tree, n);
}


int
bits_clear (bits b)
{
  if (bits_cow (b))
    return -1;
  bits_tree_clear (b->lim, b->rule, b->stree->tree);
  return 0;
}


int
bits_fill (bits b)
{
  if (bits_cow (b))
    return -1;
  bits_tree_fill (b->lim, b->rule, b->stree->tree);
  return 0;
}


int
bits_clear_range (bits b, int from, int to)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_clear_range (b->lim, b->rule, b->stree->tree, from, to);
}


int
bits_fill_range (bits b, int from, int to)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_fill_range (b->lim, b->rule, b->stree->tree, from, to);
}


int
bits_complement (bits b)
{
  if (bits_cow (b))
    return -1;
  bits_tree_complement (b->lim, b->rule, b->stree->tree);
  return 0;
}


int
bits_assign (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_assign (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_union (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_union (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_intersection (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_intersection (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_difference (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_difference (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_revdifference (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_revdifference (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_xor (bits a, bits b)
{
  if (bits_cow (b))
    return -1;
  return bits_tree_xor (a->lim, a->rule, a->stree->tree, b->stree->tree);
}


int
bits_population (bits a)
{
  return bits_tree_population (a->lim, a->rule, a->stree->tree);
}


int
bits_population_range (bits a, int from, int to)
{
  return bits_tree_population_range (a->lim, a->rule, a->stree->tree, from, to);
}


int
bits_ffs (bits b)
{
  return bits_tree_ffs (b->lim, b->rule, b->stree->tree);
}


int
bits_ffc (bits b)
{
  return bits_tree_ffc (b->lim, b->rule, b->stree->tree);
}


int
bits_ffs_range (bits b, int from, int to)
{
  return bits_tree_ffs_range (b->lim, b->rule, b->stree->tree, from, to);
}


int
bits_ffc_range (bits b, int from, int to)
{
  return bits_tree_ffc_range (b->lim, b->rule, b->stree->tree, from, to);
}


