/* tag: Tom Lord Tue Dec  4 14:41:33 2001 (bitset-tree.h)
 */
/* bitset-tree.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__BITSETS__BITSET_TREE_H
#define INCLUDE__BITSETS__BITSET_TREE_H



#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/bitsets/bitset.h"



/************************************************************************
 *(h2 "Bitset Tree Rules")
 * 
 * The branching structure of a bitset tree is determined by an array
 * of structures of type `struct bits_tree_rule':
 * 
 */

/* update bits-print.c when changing fields here */
/*(c #s"struct bits_tree_rule" :category type)
 * struct bits_tree_rule;
 *
 insert*/

struct bits_tree_rule
{
  int fanout;
  size_t subset_size;
  size_t subset_shift;
  size_t subset_mask;
};

/*end-insert
 * 
 * An array of `struct bits_tree_rule' values determines the branching
 * structure of a bitset tree.  Nodes at distance `N' from the the
 * root of the tree are defined by the `N'th element of the array.
 * (See also xref:"The Bitset Tree Data Structure".)
 * 
 * `fanout' is the number of sub-trees a node has.  It is 0 for leaf 
 * nodes.
 * 
 * `subset_size' is the number of bits in each sub-tree.  For leaf
 * nodes, this should be the number of bits in the leaf node.  For
 * optimal performance, `subset_size' should be a multiple of `sizeof
 * (bitset_subset)'.
 * 
 * Given a non-leaf bitset tree `T', and a bit address within that
 * tree, `B', the subset containing that bit is:
 * 
 * 		T[B / subset_size]
 * 
 * The relative address of the bit within that subtree is:
 * 
 * 		B % subset_size
 * 
 * The fields `subset_shift' and `subset_mask' are not used for leaf
 * nodes.  For non-leaf nodes, if `subset_size' is not a power of two,
 * the fields `subset_shift' and `subset_mask' should be 0.
 * Otherwise, they should be set as follows:
 * 
 * 		subset_shift = log2(subset_size)
 * 		subset_mask = subset_size - 1
 * 
 * Here is an example for bitset containing `1<<21' elements.  In this
 * example, the root node of the tree has 32 sub-trees; each second
 * and third level tree has 16 sub-trees; leaf nodes have 256 bits
 * (`32 * 16 * 16 * 256 == 1<<21'):
 * 
 * 	struct bits_tree_rule bitset_rule[] =
 * 	{
 *	  {32, 1<<16, 16, 0xffff}, // root has 32 subtrees
 *	  {16, 1<<12, 12, 0xfff},  // level 1 nodes have 16 subtrees
 *	  {16, 256, 8, 0xff},      // level 2 nodes have 16 subtrees
 * 	  {0, 256, 0, 0}           // leaf nodes have 256 bits
 *	};
 * 
 * Bitset trees of the same size (`1<<21' bits) could be represented
 * other ways.  For example:
 * 
 * 	struct bits_tree_rule bitset_rule[] =
 * 	{
 *	  {16, 1<<17, 17, 0x1ffff}, // root has 16 subtrees
 *	  {2, 1<<16, 16, 0xffff},   // level 1 nodes have 2 subtrees
 *	  {16, 1<<12, 12, 0xfff},   // level 2 nodes have 16 subtrees
 *	  {16, 256, 8, 0xff},	    // level 3 nodes have 16 subtrees
 * 	  {0, 256, 0, 0}	    // leaf nodes have 256 bits
 *	};
 * 
 * Some care is necessary when choosing the values for an array of
 * `struct bits_tree_rule'.  For a given bitset size, a deeper bitset
 * tree (more elements in the rules array) means that the worst-case
 * cost of accessing or modifying a single bit is raised.  On the
 * other hand, homogenous sub-trees (at any depth) are (often)
 * replaced by a 0 or `-1' pointer saving both space and time -- a
 * deeper tree may offer more opportunities for that optimization.
 * The best branching structure depends on the particular sets your
 * programs uses and the particular access pattern of your program;
 * experimentation with different branching structures may be
 * necessary.
 * 
 */


struct bits_tree_no_such_structure;

typedef struct bits_tree_no_such_structure * bits_tree;

#define bits_tree_which_bitset(L, R, X)		((R)->subset_shift ? ((X) >> (R)->subset_shift) : ((X) / (R)->subset_size))

#define bits_tree_which_bit(L, R, X)		((R)->subset_mask ? ((X) & (R)->subset_mask) : ((X) % (R)->subset_size))

#define bits_tree_full_bitset			((bits_tree)-1L)

#define bits_tree_empty_bitset			((bits_tree)0)




/* automatically generated __STDC__ prototypes */
extern bits_tree bits_tree_alloc (alloc_limits lim,
				  struct bits_tree_rule * rule);
extern void bits_tree_free (alloc_limits lim,
			    struct bits_tree_rule * rule,
			    bits_tree b);
extern bit_t bits_tree_compact (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree a);
extern bits_tree bits_tree_dup (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree a);
extern int bits_tree_get_subset (bitset_subset * answer,
				 alloc_limits lim,
				 struct bits_tree_rule * rule,
				 bits_tree b,
				 bit_t n);
extern int bits_tree_is_member (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree b,
				int n);
extern int bits_tree_is_equal (alloc_limits lim,
			       struct bits_tree_rule * rule,
			       bits_tree a, bits_tree b);
extern int bits_tree_is_subset (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree a,
				bits_tree b);
extern int bits_tree_is_empty (alloc_limits lim,
			       struct bits_tree_rule * rule,
			       bits_tree a);
extern int bits_tree_is_full (alloc_limits lim,
			      struct bits_tree_rule * rule,
			      bits_tree a);
extern int bits_tree_is_empty_range (alloc_limits lim,
				     struct bits_tree_rule * rule,
				     bits_tree a,
				     int from,
				     int to);
extern int bits_tree_is_full_range (alloc_limits lim,
				    struct bits_tree_rule * rule,
				    bits_tree a,
				    int from,
				    int to);
extern int bits_tree_adjoin (alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree b,
			     int n);
extern int bits_tree_remove (alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree b, int n);
extern int bits_tree_toggle (alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree b,
			     int n);
extern void bits_tree_clear (alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree b);
extern void bits_tree_fill (alloc_limits lim,
			    struct bits_tree_rule * rule,
			    bits_tree b);
extern int bits_tree_clear_range (alloc_limits lim,
				  struct bits_tree_rule * rule,
				  bits_tree b,
				  int from,
				  int to);
extern int bits_tree_fill_range (alloc_limits lim,
				 struct bits_tree_rule * rule,
				 bits_tree b,
				 int from,
				 int to);
extern void bits_tree_complement (alloc_limits lim,
				  struct bits_tree_rule * rule,
				  bits_tree b);
extern int bits_tree_assign (alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree a,
			     bits_tree b);
extern int bits_tree_union (alloc_limits lim,
			    struct bits_tree_rule * rule,
			    bits_tree a,
			    bits_tree b);
extern int bits_tree_intersection (alloc_limits lim,
				   struct bits_tree_rule * rule,
				   bits_tree a,
				   bits_tree b);
extern int bits_tree_difference (alloc_limits lim,
				 struct bits_tree_rule * rule,
				 bits_tree a,
				 bits_tree b);
extern int bits_tree_revdifference (alloc_limits lim,
				    struct bits_tree_rule * rule,
				    bits_tree a,
				    bits_tree b);
extern int bits_tree_xor (alloc_limits lim,
			  struct bits_tree_rule * rule,
			  bits_tree a,
			  bits_tree b);
extern int bits_tree_population (alloc_limits lim,
				 struct bits_tree_rule * rule,
				 bits_tree a);
extern int bits_tree_population_range (alloc_limits lim,
				       struct bits_tree_rule * rule,
				       bits_tree a, int from, int to);
extern int bits_tree_ffs (alloc_limits lim,
			  struct bits_tree_rule * rule,
			  bits_tree b);
extern int bits_tree_ffc (alloc_limits lim, struct bits_tree_rule * rule, bits_tree b);
extern int bits_tree_ffs_range (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree b,
				int from,
				int to);
extern int bits_tree_ffc_range (alloc_limits lim,
				struct bits_tree_rule * rule,
				bits_tree b,
				int from,
				int to);
#endif  /* INCLUDE__BITSETS__BITSET_TREE_H */
