/* tag: Tom Lord Tue Dec  4 14:41:51 2001 (pow2-array.c)
 */
/* pow2-array.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/stdarg.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/arrays/pow2-array.h"


/************************************************************************
 *(h1 "Power-of-Two Sparse Arrays"
 *    :includes ("hackerlab/arrays/pow2-array.h"))
 * 
 * |power of two size array|
 * |sparse array|
 * 
 * A `pow2_array' ("power-of-two sparse array") is an array-like
 * data structure.  It always holds a number of elements which is
 * some power of two.  It provides reasonably fast access to elements
 * (but slower than ordinary arrays).  It provides good memory efficiency
 * for sparsely populated arrays.
 * 
 * \NOTE:/ this interface is net yet complete.  Some details of the
 * existing interface may change in future releases.
 */
/*(menu)
 */


/************************************************************************
 *(h2 "The pow2_array Data Structure")
 * 
 * A `pow2_array' is represented by a tree structure of uniform depth.
 * Leaf elements are ordinary (dynamically allocated) arrays, each
 * leaf having the same number of elements.
 * 
 * So that sparsely populated arrays can be stored efficiently in
 * memory, subtrees which are populated entirely with default values
 * are represented in one of two ways: the root of such subtrees may
 * be represented as a `NULL' pointer; or the root of such as subtree
 * may be represented by a "default node".  In the latter case, one
 * default node exists for each level of the tree (a default root, a
 * default second-level node, a default leaf node, and so on).
 * Representation by null pointer saves memory by not allocating
 * default nodes.  Representation by default nodes speeds up access,
 * in some cases.
 * 
 * For each level of the tree, two values are defined: a `shift' and a
 * `mask'.  For a given internal node of the tree, the subtree
 * containing the `N'th element below that node is stored in the
 * subtree:
 *  
 * 		(N >> shift) & mask
 * 
 * The index of the same element within that subtree is:
 * 
 * 		N & ((1 << shift) - 1)
 * 
 * For leaf nodes, `shift' is 0.
 * 
 * The opaque type `pow2_array_rules' holds the set of `shift' and `mask'
 * values which define a tree structure for arrays of some size.
 * The opaque type `pow2_array' holds a particular array.
 * 
 * Here is a simple example: a sparse array containing 8 elements.
 * (In ordinary use, we would presumably choose a much larger power
 * of two.)
 * 
 * We will define oen possible tree structures for this array: a two
 * level tree with four elements in each leaf.  Other structures are
 * possible: we might have defined a two-level structure with two
 * elements in each leaf or a three-level structure with 2 in each
 * leaf, and two sub-trees below each internal node.
 * 
 * For the two level tree with four elements per leaf node, we have:
 * 
 * 	root:			shift == 2
 * 				mask == 1
 * 
 * 	leaf nodes:		shift == 0
 * 				mask == 3
 * 
 * The default leaf node, at address `Ld' is a four element array:
 * 
 *  
 * 	Ld:
 *  	---------------------------- 
 *  	| dflt | dflt | dflt | dflt| 
 *  	---------------------------- 
 * 
 * The default root node, at address `Rd' is a two element array:
 * 
 * 	Rd:
 *	-----------
 *	| Ld | Ld |
 *	-----------
 * 
 * An array with a non-default value (`V') in element 2, but default
 * values everywhere else might look like:
 * 
 * 			root:
 * 			----------------
 * 			|  leaf  |  Ld |
 * 			--/----------|--
 * 			 /	     |
 * 			/	     |
 * 	     /---------/	     |
 * 	leaf:			    Ld:
 * 	--------------------------  -----------------------------
 * 	| dflt | V | dflt | dflt |  | dflt | dflt | dflt | dflt |
 * 	--------------------------  -----------------------------
 * 
 * Suppose that elements in this array are of type `T'.  Then, using
 * the `shift' and `mask' values given above, the address of element
 * `N' in that tree is:
 * 
 * 	(T *)((char *)root[(N >> 2) & 1] + ((N & ((1 << 2) - 1)) & 3))
 *  
 * That is the address returned by the function `pow2_array_rref'.
 * Note that this address might be in `leaf', or it might be in the
 * default leaf `Ld'.
 * 
 * When modifying a particular element, it is important to not modify
 * the default leaf.  A copy-on-write strategy is used.  For example,
 * before modifying element 7, the tree is rewritten:
 * 
 * 			root:
 * 			--------------------
 * 			|  leaf0  |  Leaf1 |
 * 			--/------------/----
 * 			 /            /
 * 			/            /
 * 	     /---------/	    /
 *     leaf0:			   leaf1:
 *     --------------------------  -----------------------------
 *     | dflt | V | dflt | dflt |  | dflt | dflt | dflt | dflt |
 *     --------------------------  -----------------------------
 * 
 * The function `pow2_array_ref' performs that copy-on-write operation
 * and then returns an element address similarly to `pow2_array_rref'.
 * 
 * If the default value for elements is a region of memory filled with
 * 0 bytes, a tree can be represented without using default nodes.
 * For example, the array containing an element only in element 2
 * would be represented:
 * 
 * 			root:
 * 			----------------
 * 			|  leaf  |  0  |
 * 			--/-------------
 * 			 /	     
 * 			/	     
 * 	     /---------/	     
 * 	leaf:			    
 * 	-----------------  
 * 	| 0 | V | 0 | 0 |  
 * 	-----------------  
 * 
 * A tree of this variety is created by not specifying a default leaf
 * node when calling `make_pow2_array_rules'.  `pow2_array_ref' returns
 * a NULL pointer if an element is accessed which is not currently in 
 * such a tree.
 * 
 * The function `pow2_array_compact' compresses the representation of a
 * sparse array by eliminating identical subtrees.  For example, after
 * calling `pow2_array_compact' on an array with default values everywhere
 * except elements 1 and 5, the tree would look like:
 * 
 * 			root:
 * 			-------------------
 * 			|  leaf  |  Leaf |
 * 			-------\-------/---
 * 			        \     /
 *				 \   /
 *				  \ /
 *				   leaf:
 *				   --------------------------
 *				   | dflt | V | dflt | dflt |
 *				   --------------------------
 * 
 * After calling `pow2_array_compact', it is no longer safe to call
 * `pow2_array_ref' for the same array.  `pow2_array_ref' is safe.
 * `pow2_array_compact' is useful in combination with
 * `pow2_array_print'.
 */



/************************************************************************
 *(h2 "Allocating Power-of-Two Sparse Arrays")
 * 
 * 
 * 
 */


/*(c make_pow2_array_rules)
 * pow2_array_rules make_pow2_array_rules (alloc_limits limits,
 *                                         size_t elt_size,
 *                                         void * default_page,
 *                                         int shift,
 *                                         size_t mask,
 *                                         ...);
 * 
 * Return the `pow2_array_rules' which defines the tree structure
 * for a particular type of sparse array.
 * 
 * `limits' is used when allocating the `pow2_array_rules' and default
 * nodes.  See xref:"Allocation With Limitations".
 * 
 * `elt_size' is the size, in bytes, of individual elements.
 * 
 * `default_page' is either 0 or a default leaf node. 
 * 
 * The remaining arguments are a series of `shift' and `mask' pairs,
 * ending with a pair in which `shift' is 0.
 * 
 * See xref:"The pow2_array Data Structure" for more information about
 * default leaf nodes, shifts, and masks.
 * 
 * If allocation fails, this funtion returns 0.
 */
pow2_array_rules
make_pow2_array_rules (alloc_limits limits,
		       size_t elt_size,
		       void * default_page,
		       int shift,
		       size_t mask,
		       ...)
{
  va_list args;
  int n_levels;
  struct pow2_array_level_rule * levels;
  pow2_array_rules rules;

  rules = (pow2_array_rules)lim_malloc (limits, sizeof (*rules));
  if (!rules)
    return 0;
  mem_set0 ((t_uchar *)rules, sizeof (*rules));
  rules->elt_size = elt_size;

  va_start (args, mask);
  n_levels = 1;

  if (shift)
    {
      int s;
      size_t m;
      do
	{
	  s = va_arg (args, int);
	  m = va_arg (args, size_t);
	  ++n_levels;
	}
      while (s);
    }

  levels = (struct pow2_array_level_rule *)lim_malloc (limits, n_levels * sizeof (struct pow2_array_level_rule));
  if (!levels)
    {
      lim_free (limits, (void *)rules);
      return 0;
    }

  rules->levels = levels;
  levels[0].addr_shift = shift;
  levels[0].addr_mask = mask;
  n_levels = 1;
  va_start (args, mask);
  while (shift)
    {
      shift = va_arg (args, int);
      mask = va_arg (args, size_t);
      levels[n_levels].addr_shift = shift;
      levels[n_levels].addr_mask = mask;
      ++n_levels;
    }

  if (default_page)
    {
      void ** defaults;
      int x;
      
      defaults = (void **)lim_malloc (limits, n_levels * sizeof (void *));
      if (!defaults)
	{
	  lim_free (limits, (void *)levels);
	  lim_free (limits, (void *)rules);
	  return 0;
	}
      rules->defaults = defaults;

      defaults[n_levels - 1] = default_page;
      for (x = n_levels - 2; x >= 0; --x)
	{
	  int level_elts;

	  level_elts = levels[x].addr_mask + 1;
	  defaults[x] = (void *)lim_malloc (limits, level_elts * sizeof (void *));
	  if (!defaults[x])
	    {
	      while (x < (n_levels - 1))
		{
		  lim_free (limits, defaults[x]);
		}
	      lim_free (limits, (void *)defaults);
	      lim_free (limits, (void *)levels);
	      lim_free (limits, (void *)rules);
	      return 0;
	    }
	  {
	    int y;

	    for (y = 0; y < level_elts; ++y)
	      ((void **)defaults[x])[y] = defaults[x + 1];
	  }
	}
    }

  return rules;
}


/*(c pow2_array_alloc)
 * pow2_array pow2_array_alloc (alloc_limits limits,
 *                              pow2_array_rules rules);
 * 
 * Allocate a sparse array.
 * 
 * `limits' is used when allocating the array.  It is also used by
 * `pow2_array_ref' when allocating nodes within the array.
 * See xref:"Allocation With Limitations".
 * 
 * `rules' defines the tree structure for the array and should be
 * an object returned by `make_pow2_array_rules'.
 * 
 * If allocation fails, this funtion returns 0.
 */
pow2_array
pow2_array_alloc (alloc_limits limits,
		  pow2_array_rules rules)
{
  struct pow2_array * array;

  array = (struct pow2_array *)lim_malloc (limits, sizeof (struct pow2_array));
  if (!array)
    return 0;
  array->limits = limits;
  array->rules = rules;
  if (rules->defaults)
    array->root = (void *)rules->defaults[0];
  else
    array->root = 0;
  return array;
}


/************************************************************************
 *(h2 "Accessing Elements in Sparse Arrays")
 * 
 * 
 * 
 */

/*(c pow2_array_rref)
 * void * pow2_array_rref (pow2_array array, size_t addr);
 * 
 * Return the address if the `addr' element within `array'.
 * 
 * The value pointed to by this address should not be modified.
 * 
 * If the element has never been modified, and no default leaf node
 * was passed to `make_pow2_array_rules', this function returns 0.
 */
void *
pow2_array_rref (pow2_array array, size_t addr)
{
  struct pow2_array_level_rule * levels;
  int level;
  void * node;
  size_t node_addr;
  size_t remainder;

  levels = array->rules->levels;
  level = 0;
  node = array->root;
  while (1)
    {
      if (!node)
	return 0;

      if (!levels[level].addr_shift)
	{
	  return (void *)((char *)node + (addr & levels[level].addr_mask) * array->rules->elt_size);
	}

      node_addr = (addr >> levels[level].addr_shift) & levels[level].addr_mask;
      remainder = (addr & ((1 << levels[level].addr_shift) - 1));

      node = ((void **)node)[node_addr];
      addr = remainder;
      ++level;
    }
}



/*(c pow2_array_ref)
 * void * pow2_array_ref (pow2_array array, size_t addr);
 * 
 * Return the address if the `addr' element within `array'.
 * 
 * The value pointed to by this address may be modified.
 * 
 * This function might allocate memory if the element has not
 * previously been modified.  If allocation fails, this function
 * returns 0.
 */
void *
pow2_array_ref (pow2_array array, size_t addr)
{
  struct pow2_array_level_rule * levels;
  int level;
  void ** node;
  void ** defaults;
  size_t node_addr;
  size_t remainder;

  levels = array->rules->levels;
  level = 0;
  node = &array->root;
  defaults = array->rules->defaults;
  while (1)
    {
      if ((!*node) || (defaults && (defaults[level] == *node)))
	{
	  size_t page_size;

	  if (!levels[level].addr_shift)
	    {
	      page_size = array->rules->elt_size * (levels[level].addr_mask + 1);
	    }
	  else
	    {
	      page_size = sizeof (void *) * (levels[level].addr_mask + 1);
	    }
	  *node = lim_malloc (array->limits, page_size);
	  if (!*node)
	    return 0;

	  if (array->rules->defaults)
	    mem_move ((t_uchar *)*node, array->rules->defaults[level], page_size);
	  else
	    mem_set0 ((t_uchar *)*node, page_size);
	}

      if (!levels[level].addr_shift)
	{
	  return (void *)((char *)*node + (addr & levels[level].addr_mask) * array->rules->elt_size);
	}

      node_addr = (addr >> levels[level].addr_shift) & levels[level].addr_mask;
      remainder = (addr & ((1 << levels[level].addr_shift) - 1));

      node = ((void **)*node + node_addr);
      addr = remainder;
      ++level;
    }
}



/*include-documentation "pow2-array-compact.c")
 */

/*include-documentation "pow2-array-print.c")
 */

