/* hashtree.h - hash table decls
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__HASH__HASHTREE_H
#define INCLUDE__HASH__HASHTREE_H


#include "hackerlab/bitsets/bitset.h"


/************************************************************************
 *(h2 "Hashtree Data Structures")
 * 
 */
/*(menu)
 */


/************************************************************************
 *(h3 "How Hashtrees Work")
 * 
 * A hash tree is an associative data structure mapping "keys" to
 * "values".  Both keys and values are stored as pointers of type
 * `void *' and may point to any type of value.
 * 
 * When creating a new hash table, programs specify how parts of the
 * hash table will be allocated and freed.  For example, programs
 * might simply use `malloc' and `free', or they might use allocation
 * from a size-limited pool, or they might arrange so that parts of
 * the hashtree don't have to be allocated at all because they are
 * stored in hashtree keys.
 * 
 * Programs also specify, in advance, how keys are compared for
 * equality.
 * 
 * Programs do not specify in advance how hash values are computed for
 * keys.  Instead, when storing or looking up keys in a hash table,
 * programs first compute a hash value for the key and then call
 * either `hashtree_find' or `hashtree_store'.  Hash values are
 * `unsigned long' integers.
 * 
 * Internally, hashtrees are stored as trees.  Each tree node has 16
 * children.  The children of leaf nodes are lists of key/value pairs.
 * At each level of the tree, four bits from the hash value are used
 * to select a child.  The minimum depth of the tree is two: a root
 * node at level 0, lists of key/value pairs at level 1.  The maximum
 * depth of the tree is:
 * 
 * 	(2 * sizeof (unsigned long))
 * 
 * If a list of key/value pairs grows too long (more than 5 elements),
 * and the node containing that list is not a maximum-depth node, the
 * node "overflows".  When overflow occurs, the leaf node is made an
 * internal node with 16 subtrees.  Each of those subtrees is a new
 * leaf node.  All of the key/value pairs from the overflowing node
 * are redistributed among the new leaf nodes.
 * 
 * This style of hash tree makes reasonably efficient use of memory
 * for both small and moderately large numbers of key/value pairs.
 * Access times are determined by the depth of the tree and are
 * usually limited by the maximum depth.  In the exceptional
 * circumstance that a maximum-depth node contains a very large number
 * of key/value pairs (indicating a poor distribution of hash values),
 * access times for keys in that node grow linearly with the number of
 * keys in that node.
 * 
 * The cost of adding an element (possibly causing overflow) is most
 * commonly the same as the cost of adding an element.  The cost of
 * overflow is determined by the number of key/value pairs that must
 * be redistributed.  That number is limited by the number of
 * key/value pairs in a leaf node at not-maximal depth (64 == 16 lists
 * of key/value pairs * 4 pairs per list).  Once again, in the
 * exceptional circumstance of a maximum depth node with many
 * key/value pairs, the cost of inserting a key grows linearly with
 * the number of keys in that node.
 *
 * 
 * \WARNINGS:/ 
 * 
 * Hashtrees are not a panacea.
 * 
 * If you know advance roughly how many key/value pairs
 * a hash table will contain, you can obtain better memory use and
 * better access times by using a flat, fixed-size hash table.
 * 
 * Hashtrees are most useful when the number of key/value pairs may 
 * vary over a wide range, when it is important that tables with only 
 * a few key/value pairs remain small, and when the cost of adding
 * an element (possibly causing overflow), must remain small.
 * 
 * Hashtrees are also a reasonable default choice when good hash-table
 * performance is desirable but optimal performance is not necessary.
 * In most situations, hashtrees will give at least good performance.
 */




/************************************************************************
 *(h3 "Types for Hashtrees")
 * 
 * 
 * 
 */



/*(c hashtree :category type)
 * struct hashtree;
 *
 * A `struct hashtree' represents a hash table.  It is an opaque
 * structure that should be allocated by `hashtree_new' or else
 * initialized by being filled with 0 bytes.
 */ 
struct hashtree
{
  int refs;
  struct hashtree * parent;
  bitset_subset nested_p;
  void ** children[16];
};

/*(c hashtree_item :category type)
 * struct hashtree_item
 *
 * A `struct hashtree_item' represents one key-value pair stored in a
 * hash table.  It contains (at least) the fields:
 *
 *	void * key;
 *	void * binding;
 *
 * It is safe to modify either field, but modifications must not
 * change either the hash value of the key or its equality 
 * to other keys.
 */ 
struct hashtree_item
{
  struct hashtree_item * next_same_hash;
  struct hashtree * table;

  unsigned long hash;
  void * key;
  void * binding;
};

struct hashtree_rules;

/*(c hashtree_rules :category type)
 * struct hashtree_rules;
 * 
 * A `struct hashtree_rules' contains function pointers, and an 
 * allocation limits.
 * 
 * It has these fields, in this order:
 * 
 *   // a function to compare keye:
 *   hashtree_eq_fn eq;
 * 
 *   // a function to allocate tree nodes
 *   hash_alloc_fn hash_alloc;
 * 
 *   // a function to free tree nodes:
 *   free_hashtree_fn free_hash;
 * 
 *   // a function to allocate key/value pairs:
 *   hashtree_alloc_item_fn hash_item_alloc;
 * 
 *   // a function to free key/value pairs:
 *   free_hashtree_item_fn free_hash_item;
 * 
 *   // allocation limits that apply to hash tables
 *   // using these rules:
 *   alloc_limits limits;
 * 
 *
 insert*/
typedef int
(*hashtree_eq_fn) (void * key1,
		   void * key2,
		   struct hashtree_rules * rules);

/*end-insert
 * 
 * `hashtree_eq_fn' compares two keys for equality.  It returns 1 if they
 * are equal, 0 otherwise.
 *
 insert*/
typedef struct hashtree * 
(*hashtree_alloc_fn) (struct hashtree_rules * rules);
/*end-insert
 * 
 * `hashtree_alloc_fn' allocates a new hash table tree node.  Hash tables
 * are nested to form trees; this function allocates one node of such
 * a tree.  This function may return 0 if allocation fails.
 *
 insert*/
typedef void
(*free_hashtree_fn) (struct hashtree * node,
		     struct hashtree_rules * rules);
/*end-insert
 * 
 * `free_hashtree_fn' frees an empty hash table tree node.
 * 
 insert*/
typedef struct hashtree_item * 
(*hashtree_alloc_item_fn) (void * key, struct hashtree_rules * rules);
/*end-insert
 * 
 * `hashtree_alloc_item_fn' allocates a hash table item (key/value
 * pair) for the indicated key.  It may return 0 if allocation fails.
 * 
 * `hashtree_alloc_item_fn' must fill in the `key' and `binding'
 * fields of the `struct hashtree_item' that it returns.  Typically,
 * the field `key' is set equal to the parameter `key', and the field
 * `binding' is initialized to 0, indicating that the key initially
 * has no binding.
 * 
 * Sometimes, if a key will be stored in at most one hashtree, a
 * useful optimization is to store a `struct hashtree_item' within
 * each key.  In that case, `hashtree_alloc_item_fn' doesn't have to
 * allocate memory at all: it can return a pointer to the `struct
 * hashtree_item' in the key.
 * 
 insert*/
typedef void
(*free_hashtree_item_fn) (struct hashtree_item * node,
			  struct hashtree_rules * rules);
/*end-insert
 * 
 * `free_hashtree_item_fn' frees a hash table item (key/value pair).
 * 
 * 
 * The field `limits' points to allocation limits which are used
 * by the default implementations of these functions.  By convention,
 * non-default implementations should also use `limits' when performing
 * allocations.
 * 
 * Defaults are provided for the functions in a `struct
 * hashtree_rules'.  If any particular function pointer is 0 or if the
 * `struct hashtree_rules *' passed to `hashtree_alloc' is 0, the
 * default implementations are used.
 * 
 * The default functions perform allocations by calling `lim_malloc'
 * with the allocation limits stored in the field `limits'.  (See
 * xref:"Allocation With Limitations".)  If `limits' is 0, or if the
 * `struct hashtree_rules *' pointer is 0, the allocation limits
 * `lim_use_must_malloc' is used.  In that case, if allocation fails,
 * the process is exitted by calling `panic'.
 * 
 * The default implementation of `eq' compares pointers to keys using
 * `=='.
 *  
 */
struct hashtree_rules
{
  hashtree_eq_fn eq;
  hashtree_alloc_fn hash_alloc;
  free_hashtree_fn free_hash;
  hashtree_alloc_item_fn hash_item_alloc;
  free_hashtree_item_fn free_hash_item;
  alloc_limits limits;
};



typedef void (*hashtree_free_data_fn) (struct hashtree_item * it,
				       struct hashtree_rules * rules);


/* automatically generated __STDC__ prototypes */
extern struct hashtree_rules * hashtree_make_rules (alloc_limits limits);
extern void hashtree_init_rules (struct hashtree_rules * rules,
				 hashtree_eq_fn eq,
				 hashtree_alloc_fn hash_alloc,
				 free_hashtree_fn free_hash,
				 hashtree_alloc_item_fn hash_item_alloc,
				 free_hashtree_item_fn free_hash_item,
				 alloc_limits limits);
extern void hashtree_free_limit_rules (struct hashtree_rules * rules);
extern struct hashtree * hashtree_alloc (struct hashtree_rules * rules);
extern void hashtree_free (struct hashtree * it,
			   hashtree_free_data_fn freefn,
			   struct hashtree_rules * rules);
extern void hashtree_free_static (struct hashtree * tab,
				  hashtree_free_data_fn freefn,
				  struct hashtree_rules * rules);
extern struct hashtree_item * hashtree_find (struct hashtree * table,
					     unsigned long hash,
					     void * key,
					     struct hashtree_rules * rules);
extern struct hashtree_item * hashtree_store (struct hashtree * table,
					      unsigned long hash,
					      void * key,
					      struct hashtree_rules * rules);
extern void hashtree_delete (struct hashtree_item * it, struct hashtree_rules * rules);
#endif  /* INCLUDE__HASH__HASHTREE_H */
