/* tag: Tom Lord Tue Dec  4 14:41:34 2001 (bitset-tree.c)
 */
/* bitset-tree.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/mem/mem.h"
#include "hackerlab/bitsets/bitset-tree.h"


/************************************************************************
 *(h1 "Bitset Trees"
 *    :includes ("hackerlab/bitsets/bitset-tree.h"))
 * 
 * A "bitset tree" is a sparse representation for a large bitset which
 * is mostly homogenous (i.e., containing large runs which are all 0
 * or all 1).  A bitset tree saves time and memory by compressing the
 * representation of homogenous subsets.
 * 
 * It is important to note that if your bitsets do not contain large,
 * contiguous, homogenous (all 0 or all 1) subsets, then bitset trees
 * are of no help -- they add space and time overhead and offer no
 * advantage.  If your sets do have homogenous subsets, bitset trees
 * can lead to significant space and time savings.
 * 
 * Usually, you will not want to use bitset trees directly.  Instead,
 * you will want to use ``shared bitset trees''.  (See xref:"Shared
 * Bitset Trees".)  Nevertheless, even if you will use shared bitset
 * trees, it is important to understand how bitset trees work.
 */


/************************************************************************
 *(h2 "The Bitset Tree Data Structure")
 * 
 * This section describes the internal representation of bitset trees.
 * The public interface to bitset trees hides most of the details of
 * this representation -- but understanding it will help you predict
 * the performance of programs which use bitset trees, and design
 * bitset tree rules.  (See xref:"Bitset Tree Rules".)
 * 
 * Bitset trees are defined recursively: a bitset tree is an array of
 * (pointers to) bitset trees.  The leaf nodes of a bitset trees are
 * (pointers to) ordinary bitsets.  (See xref:"Flat Bitsets".)
 * 
 * A bitset tree represents a single logical bitset which is the
 * concatenation of its sub-trees.  Each subtree at a given distance
 * from the root has the same number of members and the same depth and
 * branching structure.
 * 
 * As an optimization, if a particular subtree is all 0s, that subtree
 * may be replaced by a null pointer.  If a particular subtree is all
 * 1s, it may be replaced by the pointer `(bits_tree)-1'.  Whenever
 * practical, functions which operate on bitset trees use this
 * optimization automatically.  For example, `bits_fill_range' will
 * store a subtree of all 0s or all 1s efficiently.  In some cases,
 * the optimization is not practical.  For example, `bits_adjoin'
 * does not attempt to optimize the subtrees it modifies.
 * 
 * The function `bits_tree_compact' recursively searches a bitset tree,
 * replacing homogenous sub-trees with 0 or `-1'.
 * 
 */

/*(include-documentation "bitset-tree.h")
 */



/************************************************************************
 *(h2 "Allocating Bitset Trees")
 * 
 * 
 * 
 */

/*(c bits_tree_alloc)
 * bits_tree bits_tree_alloc (alloc_limits lim,
 *                            struct bits_tree_rule * rule);
 * 
 * Allocate a new bitset tree.
 * 
 * `lim' describes allocation limits that apply to this bitset tree.
 * For more information about allocation limits, see xref:"Allocation
 * With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 * The new set is initialized to all `0's.
 * 
 * If allocation fails, 0 is returned.
 */
bits_tree
bits_tree_alloc (alloc_limits lim,
		 struct bits_tree_rule * rule)
{
  bits_tree answer;

  if (!rule->fanout)
    return (bits_tree)bitset_alloc (lim, rule->subset_size);

  answer = (bits_tree)lim_malloc (lim, rule->fanout * sizeof (bits_tree));
  if (answer)
    mem_set0 ((t_uchar *)answer, rule->fanout * sizeof (bits_tree));
  return answer;
}


/*(c bits_tree_free)
 * void bits_tree_free (alloc_limits lim,
 *                      struct bits_tree_rule * rule,
 *                      bits_tree b);
 * 
 * Free all storage associated with a bitset tree.
 * 
 * `lim' describes allocation limits that apply to this bitset tree.
 * For more information about allocation limits, see xref:"Allocation
 * With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 */
void
bits_tree_free (alloc_limits lim,
		struct bits_tree_rule * rule,
		bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_free (lim, (bitset)b);
      return;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (   (((bits_tree *)b)[x] != bits_tree_full_bitset)
	  && (((bits_tree *)b)[x] != bits_tree_empty_bitset))
	bits_tree_free (lim, rule + 1, ((bits_tree *)b)[x]);
    }
  lim_free (lim, (t_uchar *)b);
}


/*(c bits_tree_compact)
 * bit_t bits_tree_compact (alloc_limits lim,
 *                          struct bits_tree_rule * rule,
 *                          bits_tree a);
 * 
 * Optimize a bitset tree by compacting homogenous
 * sub-trees.  See xref:"The Bitset Tree Data Structure".
 * 
 * `lim' describes allocation limits that apply to this bitset tree.
 * For more information about allocation limits, see xref:"Allocation
 * With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 * The population size of the set is returned.
 */
bit_t
bits_tree_compact (alloc_limits lim,
		   struct bits_tree_rule * rule,
		   bits_tree a)
{
  int x;
  bit_t pop;

  if (!rule->fanout)
    {
      return bits_tree_population (lim, rule, a);
    }

  pop = 0;
  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	{
	  pop += rule->subset_size;
	}
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	continue;
      else 
	{
	  bit_t sub_pop;

	  sub_pop = bits_tree_compact (lim, rule + 1, ((bits_tree *)a)[x]);

	  if (sub_pop == 0)
	    {
	      bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	      ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	    }
	  else if (sub_pop == rule->subset_size)
	    {
	      bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	      ((bits_tree *)a)[x] = bits_tree_full_bitset;
	    }

	  pop += sub_pop;
	}
    }
  return pop;
}


/*(c bits_tree_dup)
 * bits_tree bits_tree_dup (alloc_limits lim,
 *                          struct bits_tree_rule * rule,
 *                          bits_tree a);
 * 
 * Allocate a new copy of bitset tree `a'.
 * 
 * `lim' describes allocation limits that apply to this bitset tree.
 * For more information about allocation limits, see xref:"Allocation
 * With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 * If allocation fails, 0 is returned.
 */
bits_tree
bits_tree_dup (alloc_limits lim,
	       struct bits_tree_rule * rule,
	       bits_tree a)
{
  bits_tree answer;
  int x;

  if (!rule->fanout)
    {
      return (bits_tree)bitset_dup (lim, rule->subset_size, (bitset)a);
    }

  answer = (bits_tree)lim_malloc (lim, rule->fanout * sizeof (bits_tree));
  if (answer)
    {
      for (x = 0; x < rule->fanout; ++x)
	{
	  if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	    ((bits_tree *)answer)[x] = bits_tree_full_bitset;
	  else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	    ((bits_tree *)answer)[x] = bits_tree_empty_bitset;
	  else
	    {
	      ((bits_tree *)answer)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)a)[x]);
	      if ((!((bits_tree *)answer)[x]) && ((bits_tree *)a)[x])
		{
		  int y;
		  for (y = x; y < rule->fanout; ++y)
		    ((bits_tree *)answer)[y] = bits_tree_empty_bitset;
		  bits_tree_free (lim, rule, answer);
		  return 0;
		}
	    }
	}
    }
  return answer;
}


/************************************************************************
 *h2 "Accessing Individual Subsets of a Bitset Tree")
 * 
 * 
 * 
 */

/*c bits_tree_get_subset)
 * int bits_tree_get_subset (bitset_subset * answer,
 *                           alloc_limits lim,
 *                           struct bits_tree_rule * rule,
 *                           bits_tree b,
 *                           bit_t n);
 * 
 * Return (in `*answer') the `bitset_subet' containing bit `n' of
 * bitset tree `b'.  See xref:"bitset_subset".
 * 
 * `lim' describes allocation limits that apply to this
 * bitset tree.  For more information about allocation limits, see
 * xref:"Allocation With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 * If the indicated subset does not exist (because it is part of
 * a homogenous subtree whose representation has been optimized)
 * the corresponding subtree is newly allocated.
 * 
 * If allocation fails, -1 is returned.  Otherwise, 0 is returned.
 */
int
bits_tree_get_subset (bitset_subset * answer,
		      alloc_limits lim,
		      struct bits_tree_rule * rule,
		      bits_tree b,
		      bit_t n)
{
  int bs;

  if (!rule->fanout)
    {
      *answer = ((bitset)b)[bitset_which_subset (n)];
      return 0;
    }

  bs = bits_tree_which_bitset (lim, rule, n);

  if (((bits_tree *)b)[bs] == bits_tree_empty_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_empty_bitset;
	  return -1;
	}
    }
  else if (((bits_tree *)b)[bs] == bits_tree_full_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_full_bitset;
	  return -1;
	}
      bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[bs]);
    }

  return bits_tree_get_subset (answer, lim, rule + 1, ((bits_tree *)b)[bs], bits_tree_which_bit(lim, rule, n));
}


/************************************************************************
 *(h2 "Operations on Bitset Trees")
 * 
 * 
 * Each of the operations defined for flat bitsets has a corresponding
 * operation for bitset trees.  See xref:"Flat Bitsets".
 * 
 * The bitset tree operations are:
 * 
 * 
 * |$bits_tree_is_member| |$bits_tree_is_equal| |$bits_tree_is_subset|
 * |$bits_tree_is_empty| |$bits_tree_is_full| |$bits_tree_is_empty_range|
 * |$bits_tree_is_full_range| |$bits_tree_adjoin| |$bits_tree_remove|
 * |$bits_tree_toggle| |$bits_tree_clear| |$bits_tree_fill| |$bits_tree_clear_range|
 * |$bits_tree_fill_range| |$bits_tree_complement| |$bits_tree_assign|
 * |$bits_tree_union| |$bits_tree_intersection| |$bits_tree_difference|
 * |$bits_tree_revdifference| |$bits_tree_xor| |$bits_tree_population|
 * |$bits_tree_population_range| |$bits_tree_ffs| |$bits_tree_ffc|
 * |$bits_tree_ffs_range| |$bits_tree_ffc_range|
 * 
 *     int bits_tree_is_member (alloc_limits lim,
 *     			 struct bits_tree_rule * rule,
 *     			 bits_tree b,
 *     			 int n);
 *     int bits_tree_is_equal (alloc_limits lim,
 *     			struct bits_tree_rule * rule,
 *     			bits_tree a, bits_tree b);
 *     int bits_tree_is_subset (alloc_limits lim,
 *     			 struct bits_tree_rule * rule,
 *     			 bits_tree a,
 *     			 bits_tree b);
 *     int bits_tree_is_empty (alloc_limits lim,
 *     			struct bits_tree_rule * rule,
 *     			bits_tree a);
 *     int bits_tree_is_full (alloc_limits lim,
 *     		       struct bits_tree_rule * rule,
 *     		       bits_tree a);
 *     int bits_tree_is_empty_range (alloc_limits lim,
 *     			      struct bits_tree_rule * rule,
 *     			      bits_tree a,
 *     			      int from,
 *     			      int to);
 *     int bits_tree_is_full_range (alloc_limits lim,
 *     			     struct bits_tree_rule * rule,
 *     			     bits_tree a,
 *     			     int from,
 *     			     int to);
 *     int bits_tree_adjoin (alloc_limits lim,
 *     		      struct bits_tree_rule * rule,
 *     		      bits_tree b,
 *     		      int n);
 *     int bits_tree_remove (alloc_limits lim,
 *     		      struct bits_tree_rule * rule,
 *     		      bits_tree b, int n);
 *     int bits_tree_toggle (alloc_limits lim,
 *     		      struct bits_tree_rule * rule,
 *     		      bits_tree b,
 *     		      int n);
 *     void bits_tree_clear (alloc_limits lim,
 *     		      struct bits_tree_rule * rule,
 *     		      bits_tree b);
 *     void bits_tree_fill (alloc_limits lim,
 *     		     struct bits_tree_rule * rule,
 *     		     bits_tree b);
 *     int bits_tree_clear_range (alloc_limits lim,
 *     			   struct bits_tree_rule * rule,
 *     			   bits_tree b,
 *     			   int from,
 *     			   int to);
 *     int bits_tree_fill_range (alloc_limits lim,
 *     			  struct bits_tree_rule * rule,
 *     			  bits_tree b,
 *     			  int from,
 *     			  int to);
 *     void bits_tree_complement (alloc_limits lim,
 *     			   struct bits_tree_rule * rule,
 *     			   bits_tree b);
 *     int bits_tree_assign (alloc_limits lim,
 *     		      struct bits_tree_rule * rule,
 *     		      bits_tree a,
 *     		      bits_tree b);
 *     int bits_tree_union (alloc_limits lim,
 *     		     struct bits_tree_rule * rule,
 *     		     bits_tree a,
 *     		     bits_tree b);
 *     int bits_tree_intersection (alloc_limits lim,
 *     			    struct bits_tree_rule * rule,
 *     			    bits_tree a,
 *     			    bits_tree b);
 *     int bits_tree_difference (alloc_limits lim,
 *     			  struct bits_tree_rule * rule,
 *     			  bits_tree a,
 *     			  bits_tree b);
 *     int bits_tree_revdifference (alloc_limits lim,
 *     			     struct bits_tree_rule * rule,
 *     			     bits_tree a,
 *     			     bits_tree b);
 *     int bits_tree_xor (alloc_limits lim,
 *     		   struct bits_tree_rule * rule,
 *     		   bits_tree a,
 *     		   bits_tree b);
 *     int bits_tree_population (alloc_limits lim,
 *     			  struct bits_tree_rule * rule,
 *     			  bits_tree a);
 *     int bits_tree_population_range (alloc_limits lim,
 *     				struct bits_tree_rule * rule,
 *     				bits_tree a, int from, int to);
 *     int bits_tree_ffs (alloc_limits lim,
 *     		   struct bits_tree_rule * rule,
 *     		   bits_tree b);
 *     int bits_tree_ffc (alloc_limits lim,
 *     		   struct bits_tree_rule * rule,
 *     		   bits_tree b);
 *     int bits_tree_ffs_range (alloc_limits lim,
 *     			 struct bits_tree_rule * rule,
 *     			 bits_tree b,
 *     			 int from,
 *     			 int to);
 *     int bits_tree_ffc_range (alloc_limits lim,
 *     			 struct bits_tree_rule * rule,
 *     			 bits_tree b,
 *     			 int from,
 *     			 int to);
 *
 * Each function performs the same operation as the corresponding
 * `bitset_' function (replace `bits_tree' with `bitset_'.)
 * For documentation, see xref:"Flat Bitsets".  For that reason,
 * the `bits_tree_' functions are not individually documented.
 * 
 * Each `bits_tree' function takes two initial parameters, `lim' and `rule'.
 * 
 * `lim' describes allocation limits that apply to the bitset tree(s)
 * being operated upon.  For more information about allocation limits,
 * see xref:"Allocation With Limitations".
 * 
 * `rule' describes the branching structure of the bitset trees.
 * See xref:"Bitset Tree Rules".
 * 
 * These functions:
 * 
 *    bits_tree_adjoin
 *    bits_tree_remove
 *    bits_tree_toggle
 *    bits_tree_clear
 *    bits_tree_fill
 *    bits_tree_clear_range
 *    bits_tree_fill_range
 *    bits_tree_complement
 *    bits_tree_assign
 *    bits_tree_union
 *    bits_tree_intersection
 *    bits_tree_difference
 *    bits_tree_revdifference
 *    bits_tree_xor
 * 
 * return a value of type `int'. All of them will sometimes allocate
 * memory. 
 * 
 * If an allocation fails, these functions return -1 and have
 * indeterminate side effect on the set being operated upon.
 * 
 * If all allocations succeed, they return 0 (and have the intended
 * side effect on the set being operated upon).
 */

int
bits_tree_is_member (alloc_limits lim,
		     struct bits_tree_rule * rule,
		     bits_tree b,
		     int n)
{
  int bs;

  if (!rule->fanout)
    {
      return bitset_is_member ((bitset)b, n);
    }

  bs = bits_tree_which_bitset (lim, rule, n);
  if (((bits_tree *)b)[bs] == bits_tree_full_bitset)
    return 1;
  else if (((bits_tree *)b)[bs] == bits_tree_empty_bitset)
    return 0;
  else
    return bits_tree_is_member (lim, rule + 1, ((bits_tree *)b)[bs], bits_tree_which_bit (lim, rule, n));
}


static int
bits_tree_is_full_bitset (alloc_limits lim,
			  struct bits_tree_rule * rule,
			  bits_tree a)
{
  return (   (a == bits_tree_full_bitset)
	  || (   (a != bits_tree_empty_bitset)
	      && (rule->subset_size == bits_tree_population (lim, rule + 1, a))));
}


static int
bits_tree_is_empty_bitset (alloc_limits lim,
			   struct bits_tree_rule * rule,
			   bits_tree a)
{
  return (   (a == bits_tree_empty_bitset)
	  || (   (a != bits_tree_full_bitset)
	      && (0 == bits_tree_population (lim, rule + 1, a))));
}



int
bits_tree_is_equal (alloc_limits lim,
		    struct bits_tree_rule * rule,
		    bits_tree a, bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      return bitset_is_equal (rule->subset_size, (bitset)a, (bitset)b);
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	{
	  if (!bits_tree_is_full_bitset (lim, rule, ((bits_tree *)b)[x]))
	    return 0;
	}
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	{
	  if (!bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)b)[x]))
	    return 0;
	}
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	{
	  if (!bits_tree_is_full_bitset (lim, rule, ((bits_tree *)a)[x]))
	    return 0;
	}
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	{
	  if (!bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)a)[x]))
	    return 0;
	}
      else if (!bits_tree_is_equal (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	  return 0;
    }
  return 1;
}


int
bits_tree_is_subset (alloc_limits lim,
		     struct bits_tree_rule * rule,
		     bits_tree a,
		     bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      return bitset_is_subset (rule->subset_size, (bitset)a, (bitset)b);
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	{
	  if (!bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)b)[x]))
	    return 0;
	}
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	return 0;
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	continue;
      else
	if (!bits_tree_is_subset (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	  return 0;
    }
  return 1;
}


int
bits_tree_is_empty (alloc_limits lim,
		    struct bits_tree_rule * rule,
		    bits_tree a)
{
  int x;

  if (!rule->fanout)
    {
      return bitset_is_empty (rule->subset_size, (bitset)a);
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (!bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)a)[x]))
	return 0;
    }
  return 1;
}


int
bits_tree_is_full (alloc_limits lim,
		   struct bits_tree_rule * rule,
		   bits_tree a)
{
  int x;

  if (!rule->fanout)
    {
      return bitset_is_full (rule->subset_size, (bitset)a);
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (!bits_tree_is_full_bitset (lim, rule, ((bits_tree *)a)[x]))
	return 0;
    }
  return 1;
}


int
bits_tree_is_empty_range (alloc_limits lim,
			  struct bits_tree_rule * rule,
			  bits_tree a,
			  int from,
			  int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      return bitset_is_empty_range ((bitset)a, from, to);
    }

  if (to <= from)
    return 1;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      if (((bits_tree *)a)[first_bs] == bits_tree_full_bitset)
	return 0;
      if (((bits_tree *)a)[first_bs] == bits_tree_empty_bitset)
	return 1;
      return bits_tree_is_empty_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, last_bit);
    }
  else
    {
      int x;

      if (   (((bits_tree *)a)[first_bs] == bits_tree_full_bitset)
	  || (   (((bits_tree *)a)[first_bs] != bits_tree_empty_bitset)
	      && !bits_tree_is_empty_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, rule->subset_size)))
	return 0;

      if (   last_bit
	  && ((((bits_tree *)a)[last_bs] == bits_tree_full_bitset)
	      || (   (((bits_tree *)a)[last_bs] != bits_tree_empty_bitset)
		  && !bits_tree_is_empty_range (lim, rule + 1, ((bits_tree *)a)[last_bs], 0, last_bit))))
	return 0;

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  if (!bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)a)[x]))
	    return 0;
	}
      return 1;
    }
}

int
bits_tree_is_full_range (alloc_limits lim,
			 struct bits_tree_rule * rule,
			 bits_tree a,
			 int from,
			 int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      return bitset_is_full_range ((bitset)a, from, to);
    }

  if (to <= from)
    return 1;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      if (((bits_tree *)a)[first_bs] == bits_tree_full_bitset)
	return 1;
      if (((bits_tree *)a)[first_bs] == bits_tree_empty_bitset)
	return 0;
      return bits_tree_is_full_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, last_bit);
    }
  else
    {
      int x;

      if (   (((bits_tree *)a)[first_bs] == bits_tree_empty_bitset)
	  || (   (((bits_tree *)a)[first_bs] != bits_tree_full_bitset)
	      && !bits_tree_is_full_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, rule->subset_size)))
	return 0;

      if (   last_bit
	  && ((((bits_tree *)a)[last_bs] == bits_tree_empty_bitset)
	      || (   (((bits_tree *)a)[last_bs] != bits_tree_full_bitset)
		  && !bits_tree_is_full_range (lim, rule + 1, ((bits_tree *)a)[last_bs], 0, last_bit))))
	return 0;

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  if (!bits_tree_is_full_bitset (lim, rule, ((bits_tree *)a)[x]))
	    return 0;
	}
      return 1;
    }
}

int
bits_tree_adjoin (alloc_limits lim,
		  struct bits_tree_rule * rule,
		  bits_tree b,
		  int n)
{
  int bs;

  if (!rule->fanout)
    {
      bitset_adjoin ((bitset)b, n);
      return 0;
    }

  bs = bits_tree_which_bitset (lim, rule, n);
  if (((bits_tree *)b)[bs] == bits_tree_full_bitset)
    return 0;

  if (((bits_tree *)b)[bs] == bits_tree_empty_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_empty_bitset;
	  return -1;
	}
    }

  return bits_tree_adjoin (lim, rule + 1, ((bits_tree *)b)[bs], bits_tree_which_bit (lim, rule, n));
}


int
bits_tree_remove (alloc_limits lim,
		  struct bits_tree_rule * rule,
		  bits_tree b, int n)
{
  int bs;

  if (!rule->fanout)
    {
      bitset_remove ((bitset)b, n);
      return 0;
    }

  bs = bits_tree_which_bitset (lim, rule, n);

  if (((bits_tree *)b)[bs] == bits_tree_empty_bitset)
    return 0;

  if (((bits_tree *)b)[bs] == bits_tree_full_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_full_bitset;
	  return -1;
	}
      bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[bs]);
    }
  return bits_tree_remove (lim, rule + 1, ((bits_tree *)b)[bs], bits_tree_which_bit (lim, rule, n));
}


int
bits_tree_toggle (alloc_limits lim,
		  struct bits_tree_rule * rule,
		  bits_tree b,
		  int n)
{
  int bs;

  if (!rule->fanout)
    {
      bitset_toggle ((bitset)b, n);
      return 0;
    }

  bs = bits_tree_which_bitset (lim, rule, n);

  if (((bits_tree *)b)[bs] == bits_tree_empty_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_empty_bitset;
	  return -1;
	}
    }
  else if (((bits_tree *)b)[bs] == bits_tree_full_bitset)
    {
      ((bits_tree *)b)[bs] = bits_tree_alloc (lim, rule + 1);
      if (!((bits_tree *)b)[bs])
	{
	  ((bits_tree *)b)[bs] = bits_tree_full_bitset;
	  return -1;
	}
      bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[bs]);
    }
  return bits_tree_toggle (lim, rule + 1, ((bits_tree *)b)[bs], bits_tree_which_bit (lim, rule, n));
}


void
bits_tree_clear (alloc_limits lim,
		 struct bits_tree_rule * rule,
		 bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_clear (rule->subset_size, (bitset)b);
      return;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	((bits_tree *)b)[x] = bits_tree_empty_bitset;
      else if (((bits_tree *)b)[x] != bits_tree_empty_bitset)
	{
	  bits_tree_free (lim, rule + 1, ((bits_tree *)b)[x]);
	  ((bits_tree *)b)[x] = bits_tree_empty_bitset;
	}
    }
}


void
bits_tree_fill (alloc_limits lim,
		struct bits_tree_rule * rule,
		bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_fill (rule->subset_size, (bitset)b);
      return;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	((bits_tree *)b)[x] = bits_tree_full_bitset;
      else if (((bits_tree *)b)[x] != bits_tree_full_bitset)
	{
	  bits_tree_free (lim, rule + 1, ((bits_tree *)b)[x]);
	  ((bits_tree *)b)[x] = bits_tree_full_bitset;
	}
    }
}


int
bits_tree_clear_range (alloc_limits lim,
		       struct bits_tree_rule * rule,
		       bits_tree b,
		       int from,
		       int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      bitset_clear_range ((bitset)b, from, to);
      return 0;
    }

  if (to <= from)
    return 0;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	return 0;

      if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	{
	  ((bits_tree *)b)[first_bs] = bits_tree_alloc (lim, rule + 1);
	  if (!((bits_tree *)b)[first_bs])
	    {
	      ((bits_tree *)b)[first_bs] = bits_tree_full_bitset;
	      return -1;
	    }
	  bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[first_bs]);
	}

      return bits_tree_clear_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, last_bit);
    }
  else
    {
      int x;

      if (((bits_tree *)b)[first_bs] != bits_tree_empty_bitset)
	{
	  if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	    {
	      ((bits_tree *)b)[first_bs] = bits_tree_alloc (lim, rule + 1);
	      if (!((bits_tree *)b)[first_bs])
		{
		  ((bits_tree *)b)[first_bs] = bits_tree_full_bitset;
		  return -1;
		}
	      bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[first_bs]);
	    }
	  if (bits_tree_clear_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, rule->subset_size))
	    return -1;
	}

      if (last_bit)
	{
	  if (((bits_tree *)b)[last_bs] != bits_tree_empty_bitset)
	    {
	      if (((bits_tree *)b)[last_bs] == bits_tree_full_bitset)
		{
		  ((bits_tree *)b)[last_bs] = bits_tree_alloc (lim, rule + 1);
		  if (!((bits_tree *)b)[last_bs])
		    {
		      ((bits_tree *)b)[last_bs] = bits_tree_full_bitset;
		      return -1;
		    }
		  bits_tree_fill (lim, rule + 1, ((bits_tree *)b)[last_bs]);
		}
	      if (bits_tree_clear_range (lim, rule + 1, ((bits_tree *)b)[last_bs], 0, last_bit))
		return -1;
	    }
	}

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	    continue;
	  if (((bits_tree *)b)[x] != bits_tree_full_bitset)
	    bits_tree_free (lim, rule + 1, ((bits_tree *)b)[x]);
	  ((bits_tree *)b)[x] = bits_tree_empty_bitset;
	}
      return 0;
    }
}


int
bits_tree_fill_range (alloc_limits lim,
		      struct bits_tree_rule * rule,
		      bits_tree b,
		      int from,
		      int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      bitset_fill_range ((bitset)b, from, to);
      return 0;
    }

  if (to <= from)
    return 0;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	return 0;

      if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	{
	  ((bits_tree *)b)[first_bs] = bits_tree_alloc (lim, rule + 1);
	  if (!((bits_tree *)b)[first_bs])
	    {
	      ((bits_tree *)b)[first_bs] = bits_tree_empty_bitset;
	      return -1;
	    }
	}

      return bits_tree_fill_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, last_bit);
    }
  else
    {
      int x;

      if (((bits_tree *)b)[first_bs] != bits_tree_full_bitset)
	{
	  if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	    {
	      ((bits_tree *)b)[first_bs] = bits_tree_alloc (lim, rule + 1);
	      if (!((bits_tree *)b)[first_bs])
		{
		  ((bits_tree *)b)[first_bs] = bits_tree_full_bitset;
		  return -1;
		}
	    }
	  if (bits_tree_fill_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, rule->subset_size))
	    return -1;
	}

      if (last_bit)
	{
	  if (((bits_tree *)b)[last_bs] != bits_tree_full_bitset)
	    {
	      if (((bits_tree *)b)[last_bs] == bits_tree_empty_bitset)
		{
		  ((bits_tree *)b)[last_bs] = bits_tree_alloc (lim, rule + 1);
		  if (!((bits_tree *)b)[last_bs])
		    {
		      ((bits_tree *)b)[last_bs] = bits_tree_empty_bitset;
		      return -1;
		    }
		}
	      if (bits_tree_fill_range (lim, rule + 1, ((bits_tree *)b)[last_bs], 0, last_bit))
		return -1;
	    }
	}

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	    continue;
	  if (((bits_tree *)b)[x] != bits_tree_empty_bitset)
	    bits_tree_free (lim, rule + 1, ((bits_tree *)b)[x]);
	  ((bits_tree *)b)[x] = bits_tree_full_bitset;
	}
      return 0;
    }
}


void
bits_tree_complement (alloc_limits lim,
		      struct bits_tree_rule * rule,
		      bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_complement (rule->subset_size, (bitset)b);
      return;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	((bits_tree *)b)[x] = bits_tree_full_bitset;
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	((bits_tree *)b)[x] = bits_tree_empty_bitset;
      else
	bits_tree_complement (lim, rule + 1, ((bits_tree *)b)[x]);
    }
}


int
bits_tree_assign (alloc_limits lim,
		  struct bits_tree_rule * rule,
		  bits_tree a,
		  bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_assign (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (   (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	  || (((bits_tree *)b)[x] == bits_tree_full_bitset))
	{
	  if (   (((bits_tree *)a)[x] != bits_tree_empty_bitset)
		 && (((bits_tree *)a)[x] != bits_tree_full_bitset))
	    bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	  ((bits_tree *)a)[x] = ((bits_tree *)b)[x];
	}
      else
	{
	  if (   (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	      || (((bits_tree *)a)[x] == bits_tree_full_bitset))
	    {
	      ((bits_tree *)a)[x] = bits_tree_alloc (lim, rule + 1);
	      if (!((bits_tree *)a)[x])
		{
		  ((bits_tree *)a)[x] = bits_tree_empty_bitset;
		  return -1;
		}
	    }
	  if (bits_tree_assign (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	    return -1;
	}
    }
  return 0;
}


int
bits_tree_union (alloc_limits lim,
		 struct bits_tree_rule * rule,
		 bits_tree a,
		 bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_union (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	{
	  if (   (((bits_tree *)a)[x] != bits_tree_empty_bitset)
	      && (((bits_tree *)a)[x] != bits_tree_full_bitset))
	    bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	  ((bits_tree *)a)[x] = bits_tree_full_bitset;
	}
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	{
	  ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (!((bits_tree *)a)[x])
	    {
	      ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	      return -1;
	    }
	}
      else
	if (bits_tree_union (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	  return -1;
    }
  return 0;
}


int
bits_tree_intersection (alloc_limits lim,
			struct bits_tree_rule * rule,
			bits_tree a,
			bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_intersection (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	{
	  if (   (((bits_tree *)a)[x] != bits_tree_empty_bitset)
	      && (((bits_tree *)a)[x] != bits_tree_full_bitset))
	    bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	  ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	}
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	{
	  ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (!((bits_tree *)a)[x])
	    {
	      ((bits_tree *)a)[x] = bits_tree_full_bitset;
	      return -1;
	    }
	}
      else
	if (bits_tree_intersection (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	  return -1;
    }
  return 0;
}


int
bits_tree_difference (alloc_limits lim,
		      struct bits_tree_rule * rule,
		      bits_tree a,
		      bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_difference (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	{
	  if (   (((bits_tree *)a)[x] != bits_tree_empty_bitset)
	      && (((bits_tree *)a)[x] != bits_tree_full_bitset))
	    bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	  ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	}
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	{
	  ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (!((bits_tree *)a)[x])
	    {
	      ((bits_tree *)a)[x] = bits_tree_full_bitset;
	      return -1;
	    }
	  bits_tree_complement (lim, rule + 1, ((bits_tree *)a)[x]);
	}
      else
	if (bits_tree_difference (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	  return -1;
    }
  return 0;
}


int
bits_tree_revdifference (alloc_limits lim,
			 struct bits_tree_rule * rule,
			 bits_tree a,
			 bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_revdifference (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	((bits_tree *)a)[x] = bits_tree_empty_bitset;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	{
	  if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	    continue;
	  else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	    ((bits_tree *)a)[x] = ((bits_tree *)b)[x];
	  else
	    {
	      ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	      if (!((bits_tree *)a)[x])
		{
		  ((bits_tree *)a)[x] = bits_tree_full_bitset;
		  return -1;
		}
	    }
	}
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	bits_tree_complement (lim, rule + 1, ((bits_tree *)a)[x]);
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	{
	  if (   (((bits_tree *)a)[x] != bits_tree_empty_bitset)
	      && (((bits_tree *)a)[x] != bits_tree_full_bitset))
	    bits_tree_free (lim, rule + 1, ((bits_tree *)a)[x]);
	  ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	}
      else
	{
	  if (bits_tree_revdifference (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	    return -1;
	}
    }
  return 0;
}


int
bits_tree_xor (alloc_limits lim,
	       struct bits_tree_rule * rule,
	       bits_tree a,
	       bits_tree b)
{
  int x;

  if (!rule->fanout)
    {
      bitset_xor (rule->subset_size, (bitset)a, (bitset)b);
      return 0;
    }

  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	{
	  if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	    ((bits_tree *)a)[x] = bits_tree_full_bitset;
	  else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	    ((bits_tree *)a)[x] = bits_tree_empty_bitset;
	  else
	    bits_tree_complement (lim, rule + 1, ((bits_tree *)a)[x]);
	}
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	{
	  if (   (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	      || (((bits_tree *)b)[x] == bits_tree_full_bitset))
	    ((bits_tree *)a)[x] = ((bits_tree *)b)[x];
	  else
	    {
	      ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	      if (!((bits_tree *)a)[x])
		{
		  ((bits_tree *)a)[x] = bits_tree_empty_bitset;
		  return -1;
		}
	    }
	}
      else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	{
	  ((bits_tree *)a)[x] = bits_tree_dup (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (!((bits_tree *)a)[x])
	    {
	      ((bits_tree *)a)[x] = bits_tree_full_bitset;
	      return -1;
	    }
	  bits_tree_complement (lim, rule + 1, ((bits_tree *)a)[x]);
	}
      else
	{
	  if (bits_tree_xor (lim, rule + 1, ((bits_tree *)a)[x], ((bits_tree *)b)[x]))
	    return -1;
	}
    }
  return 0;
}


int
bits_tree_population (alloc_limits lim,
		      struct bits_tree_rule * rule,
		      bits_tree a)
{
  int x;
  int pop;

  if (!rule->fanout)
    {
      return bitset_population (rule->subset_size, (bitset)a);
    }

  pop = 0;
  for (x = 0; x < rule->fanout; ++x)
    {
      if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	pop += rule->subset_size;
      else if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	continue;
      else
	pop += bits_tree_population (lim, rule + 1, ((bits_tree *)a)[x]);
    }
  return pop;
}


int
bits_tree_population_range (alloc_limits lim,
			    struct bits_tree_rule * rule,
			    bits_tree a, int from, int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      return bitset_population_range ((bitset)a, from, to);
    }

  if (to <= from)
    return 0;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      if (((bits_tree *)a)[first_bs] == bits_tree_full_bitset)
	return last_bit - first_bit;
      if (((bits_tree *)a)[first_bs] == bits_tree_empty_bitset)
	return 0;
      return bits_tree_population_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, last_bit);
    }
  else
    {
      int pop;
      int x;

      if (bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)a)[first_bs]))
	pop = 0;
      else if (bits_tree_is_full_bitset (lim, rule, ((bits_tree *)a)[first_bs]))
	pop = rule->subset_size - first_bit;
      else
	pop = bits_tree_population_range (lim, rule + 1, ((bits_tree *)a)[first_bs], first_bit, rule->subset_size);

      if (last_bit)
	{
	  if (bits_tree_is_empty_bitset (lim, rule, ((bits_tree *)a)[last_bs]))
	    pop += 0;
	  else if (bits_tree_is_full_bitset (lim, rule, ((bits_tree *)a)[last_bs]))
	    pop += last_bit;
	  else
	    pop += bits_tree_population_range (lim, rule + 1, ((bits_tree *)a)[last_bs], 0, last_bit);
	}

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  if (((bits_tree *)a)[x] == bits_tree_empty_bitset)
	    pop += 0;
	  else if (((bits_tree *)a)[x] == bits_tree_full_bitset)
	    pop += rule->subset_size;
	  else
	    pop += bits_tree_population (lim, rule + 1, ((bits_tree *)a)[x]);
	}
      return pop;
    }
}


int
bits_tree_ffs (alloc_limits lim,
	       struct bits_tree_rule * rule,
	       bits_tree b)
{
  int x;
  int offset;

  if (!rule->fanout)
    {
      return bitset_ffs (rule->subset_size, (bitset)b);
    }

  for (x = 0, offset = 0; x < rule->fanout; ++x, offset += rule->subset_size)
    {
      int fs;

      if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	continue;
      else if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	return offset;
      else
	{
	  fs = bits_tree_ffs (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (fs >= 0)
	    return fs + offset;
	}
    }
  return -1;
}


int
bits_tree_ffc (alloc_limits lim, struct bits_tree_rule * rule, bits_tree b)
{
  int x;
  int offset;

  if (!rule->fanout)
    {
      return bitset_ffc (rule->subset_size, (bitset)b);
    }

  for (x = 0, offset = 0; x < rule->fanout; ++x, offset += rule->subset_size)
    {
      int fc;

      if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	continue;
      else if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	return offset;
      else
	{
	  fc = bits_tree_ffc (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (fc >= 0)
	    return fc + offset;
	}
    }
  return -1;
}


int
bits_tree_ffs_range (alloc_limits lim,
		     struct bits_tree_rule * rule,
		     bits_tree b,
		     int from,
		     int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      return bitset_ffs_range ((bitset)b, from, to);
    }

  if (to <= from)
    return -1;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      int fs;

      if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	return -1;

      if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	return from;

      fs = bits_tree_ffs_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, last_bit);

      if (fs < 0)
	return -1;
      else
	return (first_bs * rule->subset_size) + fs;
    }
  else
    {
      int x;
      
      if (((bits_tree *)b)[first_bs] != bits_tree_empty_bitset)
	{
	  int fs;
	  if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	    return from;

	  fs = bits_tree_ffs_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, rule->subset_size);
	  if (fs >= 0)
	    return (first_bs * rule->subset_size) + fs;
	}

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  int fs;
	  if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	    continue;
	  if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	    return (x * rule->subset_size);
	  fs = bits_tree_ffs (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (fs >= 0)
	    return (x * rule->subset_size) + fs;
	}

      if (last_bit)
	{
	  if (((bits_tree *)b)[last_bs] != bits_tree_empty_bitset)
	    {
	      if (((bits_tree *)b)[last_bs] == bits_tree_full_bitset)
		return (last_bs * rule->subset_size);
	      else
		{
		  int fs;

		  fs = bits_tree_ffs_range (lim, rule + 1, ((bits_tree *)b)[last_bs], 0, last_bit);
		  if (fs >= 0)
		    return (last_bs * rule->subset_size) + fs;
		}
	    }
	}

      return -1;
    }
}


int
bits_tree_ffc_range (alloc_limits lim,
		     struct bits_tree_rule * rule,
		     bits_tree b,
		     int from,
		     int to)
{
  int first_bs;
  int first_bit;
  int last_bs;
  int last_bit;

  if (!rule->fanout)
    {
      return bitset_ffc_range ((bitset)b, from, to);
    }

  if (to <= from)
    return -1;

  first_bs = bits_tree_which_bitset (lim, rule, from);
  first_bit = bits_tree_which_bit (lim, rule, from);

  last_bs = bits_tree_which_bitset (lim, rule, to);
  last_bit = bits_tree_which_bit (lim, rule, to);

  if (first_bs == last_bs)
    {
      int fs;

      if (((bits_tree *)b)[first_bs] == bits_tree_full_bitset)
	return -1;

      if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	return from;

      fs = bits_tree_ffc_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, last_bit);

      if (fs < 0)
	return -1;
      else
	return (first_bs * rule->subset_size) + fs;
    }
  else
    {
      int x;
      
      if (((bits_tree *)b)[first_bs] != bits_tree_full_bitset)
	{
	  int fs;
	  if (((bits_tree *)b)[first_bs] == bits_tree_empty_bitset)
	    return from;

	  fs = bits_tree_ffc_range (lim, rule + 1, ((bits_tree *)b)[first_bs], first_bit, rule->subset_size);
	  if (fs >= 0)
	    return (first_bs * rule->subset_size) + fs;
	}

      for (x = first_bs + 1; x < last_bs; ++x)
	{
	  int fs;
	  if (((bits_tree *)b)[x] == bits_tree_full_bitset)
	    continue;
	  if (((bits_tree *)b)[x] == bits_tree_empty_bitset)
	    return (x * rule->subset_size);
	  fs = bits_tree_ffc (lim, rule + 1, ((bits_tree *)b)[x]);
	  if (fs >= 0)
	    return (x * rule->subset_size) + fs;
	}

      if (last_bit)
	{
	  if (((bits_tree *)b)[last_bs] != bits_tree_full_bitset)
	    {
	      if (((bits_tree *)b)[last_bs] == bits_tree_empty_bitset)
		return (last_bs * rule->subset_size);
	      else
		{
		  int fs;

		  fs = bits_tree_ffc_range (lim, rule + 1, ((bits_tree *)b)[last_bs], 0, last_bit);
		  if (fs >= 0)
		    return (last_bs * rule->subset_size) + fs;
		}
	    }
	}

      return -1;
    }
}


