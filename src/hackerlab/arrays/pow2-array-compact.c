/* tag: Tom Lord Tue Dec  4 14:41:52 2001 (pow2-array-compact.c)
 */
/* pow2-array-compact.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/mem/mem.h"
#include "hackerlab/hash/hashtree.h"
#include "hackerlab/hash/hash-utils.h"
#include "hackerlab/arrays/pow2-array-compact.h"


/************************************************************************
 *(h2 "Compacting Sparse Arrays")
 * 
 * 
 * 
 */





struct pow2_array_compact_key
{
  pow2_array array;
  int depth;
  void * canonical;
  int (*compare_elts) (void * a, void * b, size_t n_elts);
};



static int
item_eq (void * key1, void * key2, struct hashtree_rules * rules)
{
  struct pow2_array_compact_key * k1;
  struct pow2_array_compact_key * k2;
  int is_leaf;
  size_t n_elts;
  size_t size;

  k1 = (struct pow2_array_compact_key *)key1;
  k2 = (struct pow2_array_compact_key *)key2;

  if (k1->depth != k2->depth)
    return 0;

  is_leaf = !k1->array->rules->levels[k1->depth].addr_shift;
  n_elts = k1->array->rules->levels[k1->depth].addr_mask + 1;
  size = (is_leaf
	  ? (k1->array->rules->elt_size * n_elts)
	  : (sizeof (void *) * n_elts));

  if (!is_leaf || !k1->compare_elts)
    {
      return !mem_cmp (k1->canonical, k2->canonical, size);
    }
  else
    return k1->compare_elts (k1->canonical, k2->canonical, n_elts);
}

static void
free_item (struct hashtree_item * it, struct hashtree_rules * rules)
{
  lim_free (lim_use_must_malloc, it->key);
}


static void *
compact (struct hashtree * table,
	 struct hashtree_rules * rules,
	 pow2_array array,
	 int depth,
	 void * node,
	 t_ulong (*hash_elts) (void * elts, size_t n_elts),
	 int (*compare_elts) (void * a, void * b, size_t n_elts),
	 void (*free_elts) (void * elts, size_t n_elts))
{
  struct pow2_array_compact_key key;
  int is_leaf;
  size_t n_elts;
  size_t size;
  t_ulong hash;
  struct hashtree_item * item;

  if (!node)
    return node;

  is_leaf = !array->rules->levels[depth].addr_shift;
  n_elts = array->rules->levels[depth].addr_mask + 1;
  size = (is_leaf
	  ? (array->rules->elt_size * n_elts)
	  : (sizeof (void *) * n_elts));

  if (array->rules->defaults && (node == array->rules->defaults[depth]))
    return node;

  if (!is_leaf)
    {
      size_t x;

      for (x = 0; x < n_elts; ++x)
	((void **)node)[x] = compact (table, rules, array, depth + 1, ((void **)node)[x], hash_elts, compare_elts, free_elts);
    }

  if (array->rules->defaults)
    {
      int equals_default;

      equals_default = ((is_leaf && compare_elts)
			? !compare_elts (node, array->rules->defaults[depth], n_elts)
			: !mem_cmp (node, array->rules->defaults[depth], size));
      if (equals_default)
	{
	  if (is_leaf && free_elts)
	    {
	      free_elts (node, n_elts);
	    }
	  lim_free (array->limits, node);
	  return array->rules->defaults[depth];
	}
    }

  key.array = array;
  key.depth = depth;
  key.canonical = node;
  key.compare_elts = compare_elts;

  if (!is_leaf)
    {
      hash = hash_pointers ((void **)node, n_elts);
    }
  else
    {
      hash = (hash_elts
	      ? hash_elts (node, n_elts)
	      : hash_mem (node, size));
    }


  item = hashtree_store (table, hash, (void *)&key, rules);

  if (item->key != (void *)&key)
    {
      if (is_leaf && free_elts)
	{
	  free_elts (node, n_elts);
	}
      lim_free (array->limits, node);
      return item->binding;
    }
  else
    {
      struct pow2_array_compact_key * key2;

      key2 = (struct pow2_array_compact_key *)lim_malloc (lim_use_must_malloc, sizeof (*key2));
      *key2 = key;
      item->key = (void *)key2;
      item->binding = node;
      return node;
    }
}

	 

/*(c pow2_array_compact)
 * void pow2_array_compact (pow2_array array,
 *                          t_ulong (*hash_elts) (void * elts, size_t n_elts),
 *                          int (*compare_elts) (void * a, void * b, size_t n_elts),
 *                          void (*free_elts) (void * elts, size_t n_elts));
 * 
 * Rearrange the sparse array `array' so that equal subtrees are
 * represented by a single set of nodes.  (See xref:"The pow2_array
 * Data Structure".)
 * 
 * `hash_elts' computes a hash value for for `n_elts' stored at
 * address `elts'.
 * 
 * `compare_elts' compares two arrays of `n_elts' stored at addresses `a' 
 * and `b' returning 1 if they are equal, 0 otherwise.
 * 
 * `free_elts' frees storage associated with `n_elts' stored at
 * address `elts'.
 * 
 * This function presumes that `array' was created with allocation limits
 * `lim_use_must_malloc'.  If an allocation failure occurs, the process 
 * is terminated by calling `panic'.
 * 
 * This function is primarily intended for use in combination with
 * `pow2_array_print'.
 */
void
pow2_array_compact (pow2_array array,
		    t_ulong (*hash_elts) (void * elts, size_t n_elts),
		    int (*compare_elts) (void * a, void * b, size_t n_elts),
		    void (*free_elts) (void * elts, size_t n_elts))
{
  struct hashtree_rules * rules;
  struct hashtree * table;

  rules = hashtree_make_rules (lim_use_must_malloc);
  table = hashtree_alloc (rules);
  hashtree_init_rules (rules, item_eq, 0, 0, 0, 0, lim_use_must_malloc);
  array->root = compact (table, rules, array, 0, array->root, hash_elts, compare_elts, free_elts);
  hashtree_free (table, free_item, rules);
}
