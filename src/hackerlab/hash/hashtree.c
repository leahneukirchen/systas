/* hashtree.c - hash tables
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/mem/mem.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/hash/hashtree.h"


/************************************************************************
 *(h1 "Hash Table Trees"
 *    :include ("hash/hashtree.h"))
 *
 * The hashtree library implements in-core hash tables that are
 * automatically dynamically resized.  Callers are given complete
 * control over memory allocation of hash table data structures.
 */
/*(menu)
 */
/*(include-documentation "hashtree.h")
 */



#define RULES_LIMITS(R)		((R) ? (R)->limits : lim_use_must_malloc)

static struct hashtree *
default_hash_alloc (struct hashtree_rules * rules)
{
  return (struct hashtree *)lim_malloc (RULES_LIMITS (rules), sizeof (struct hashtree));
}

static struct hashtree_item *
default_hash_item_alloc (void * key, struct hashtree_rules * rules)
{
  struct hashtree_item * it;
  it = (struct hashtree_item *)lim_malloc (RULES_LIMITS (rules), sizeof (*it));
  if (!it)
    return 0;
  it->key = key;
  it->binding = 0;
  return it;
}

static void
default_free_hash (struct hashtree * tab, struct hashtree_rules * rules)
{
  lim_free (RULES_LIMITS (rules), (char *)tab);
}

static void
default_free_hash_item (struct hashtree_item * item, struct hashtree_rules * rules)
{
  lim_free (RULES_LIMITS (rules), (char *)item);
}


static int 
default_eq (void * va, void * vb, struct hashtree_rules * rules)
{
  return va == vb;
}




#define EQ(rules) ((rules && rules->eq) ? rules->eq : default_eq)
#define HASH_ALLOC(rules) ((rules && rules->hash_alloc) ? rules->hash_alloc : default_hash_alloc)
#define FREE_HASH(rules) ((rules && rules->free_hash) ? rules->free_hash : default_free_hash)
#define ITEM_ALLOC(rules) ((rules && rules->hash_item_alloc) \
			   ? rules->hash_item_alloc : default_hash_item_alloc)
#define FREE_HASH_ITEM(rules) ((rules && rules->free_hash_item) \
			       ? rules->free_hash_item : default_free_hash_item)



/************************************************************************
 *(h2 "Creating Hashtree Rules")
 * 
 * 
 * 
 */


/*(c hashtree_make_rules)
 * struct hashtree_rules * hashtree_make_rules (alloc_limits limits);
 * 
 * Allocate a new `hashtree_rules' structure.  (See
 * xref:"hashtree_rules".)
 * 
 * The `limits' field of the allocated structure is initialized to `limits'.
 * 
 * The function pointers in the allocated structure point to
 * default implementations.  Those implementations will use `limits'
 * when performing allocations.
 * 
 * If an allocation failure occurs, this function returns 0.
 */
struct hashtree_rules *
hashtree_make_rules (alloc_limits limits)
{
  struct hashtree_rules * _;

  _ = (struct hashtree_rules *)lim_malloc (limits, sizeof (*_));
  if (!_)
    return 0;

  _->eq = default_eq;
  _->hash_alloc = default_hash_alloc;
  _->free_hash = default_free_hash;
  _->hash_item_alloc = default_hash_item_alloc;
  _->free_hash_item = default_free_hash_item;
  _->limits = limits;
  return _;
}


/*(c hashtree_init_rules)
 * void hashtree_init_rules (struct hashtree_rules * rules,
 *                           hashtree_eq_fn eq,
 *                           hashtree_alloc_fn hash_alloc,
 *                           free_hashtree_fn free_hash,
 *                           hashtree_alloc_item_fn hash_item_alloc,
 *                           free_hashtree_item_fn free_hash_item,
 *                           alloc_limits limits);
 * 
 * Initialize a `struct hashtree_rules' structure.
 * 
 * It is also possible to initialize a rules structure by filling in
 * the fields directly.  The advantage of using `hashtree_init_rules'
 * is that all fields must be specified -- omitting a field will cause
 * a compilation error.
 */
void
hashtree_init_rules (struct hashtree_rules * rules,
		     hashtree_eq_fn eq,
		     hashtree_alloc_fn hash_alloc,
		     free_hashtree_fn free_hash,
		     hashtree_alloc_item_fn hash_item_alloc,
		     free_hashtree_item_fn free_hash_item,
		     alloc_limits limits)
{
  rules->eq = eq;
  rules->hash_alloc = hash_alloc;
  rules->free_hash = free_hash;
  rules->hash_item_alloc = hash_item_alloc;
  rules->free_hash_item = free_hash_item;
  rules->limits = limits;
}


/*(c hashtree_free_limit_rules)
 * void hashtree_free_limit_rules (struct hashtree_rules * rules);
 * 
 * Free storage of a `struct hashtree_rules' allocated by
 * `hashtree_make_rules'.
 */
void
hashtree_free_limit_rules (struct hashtree_rules * rules)
{
  lim_free (rules->limits, rules);
}


/************************************************************************
 *(h2 "Hash Tree Allocation")
 * 
 * 
 * 
 */


/*(c hashtree_new)
 * struct hashtree * hashtree_new (struct hashtree_rules * rules);
 * 
 * Allocate a new hash table.
 * 
 * `rules' specifies how memory is allocated for this hash tree, and
 * how keys are compared for equality.  `rules' may be 0, in which
 * case default implementations are used.  See xref:"Types for
 * Hashtrees".
 * 
 * If an allocation failure occurs, this function returns 0.
 */
struct hashtree *
hashtree_alloc (struct hashtree_rules * rules)
{
  struct hashtree * it;

  it = HASH_ALLOC(rules) (rules);
  if (!it)
    return 0;
  mem_set ((t_uchar *)it, 0, sizeof (*it));
  return it;
}


/*(c hashtree_free)
 * void hashtree_free (struct hashtree * it,
 *		       hashtree_free_data_fn freefn,
 *		       struct hashtree_rules * rules);
 * 
 * Free all storage allocated to a hash table, including the `struct
 * hashtree' itself.  (See `hashtree_free_static'.)
 *
 * The `freefn' is:
 *
 *   typedef void
 *   (*hashtree_free_data_fn) (struct hashtree_item * it,
 *                             struct hashtree_rules * rules);
 *
 * When called, it should free any storage associated with
 * the `key' and `binding' fields of `it'.
 *
 */
void
hashtree_free (struct hashtree * it,
	       hashtree_free_data_fn freefn,
	       struct hashtree_rules * rules)
{
  hashtree_free_static (it, freefn, rules);
  FREE_HASH(rules) (it, rules);
}


/*(c hashtree_free_static)
 * void hashtree_free_static (struct hashtree * tab,
 *			      hashtree_free_data_fn freefn,
 *			      struct hashtree_rules * rules);
 * 
 * Free all storage allocated to a hash table, but not the `struct
 * hashtree' itself. (See `hashtree_free'.)
 */
void
hashtree_free_static (struct hashtree * tab,
		      hashtree_free_data_fn freefn,
		      struct hashtree_rules * rules)
{
  int x;

  for (x = 0; x < 16; ++x)
    if (bitset_is_member (&tab->nested_p, x))
      {
	hashtree_free_static ((struct hashtree *)tab->children[x], freefn, rules);
	FREE_HASH (rules) ((struct hashtree *)tab->children[x], rules);
      }
    else
      {
	struct hashtree_item * them = (struct hashtree_item *)tab->children[x];
	while (them)
	  {
	    struct hashtree_item * that;
	    that = them;
	    them = that->next_same_hash;
	    if (freefn)
	      freefn (that, rules);
	    FREE_HASH_ITEM (rules) (that, rules);
	  }
      }
}


/************************************************************************
 *(h2 "Hash Tree Access")
 * 
 * 
 * 
 */


static unsigned long hashtree_masks[4] =
{
  0x12488421,
  0x96699669,
  0xbe7dd7eb,
  0xffffffff
};


static int shuffled_bytes[] =
{
  245, 184, 171, 36, 93, 194, 192, 143, 207, 89, 63, 175, 203, 231, 47, 238,
  103, 67, 176, 102, 80, 133, 24, 155, 91, 141, 234, 58, 44, 191, 218, 157,
  13, 168, 160, 113, 211, 213, 252, 236, 2, 19, 21, 148, 111, 251, 165, 74,
  124, 25, 181, 210, 250, 195, 235, 97, 185, 1, 179, 198, 105, 101, 5, 220,
  35, 162, 142, 41, 200, 209, 224, 71, 201, 134, 69, 48, 65, 170, 72, 167,
  145, 205, 28, 88, 215, 81, 214, 78, 118, 26, 123, 84, 140, 49, 45, 8,
  7, 107, 227, 60, 59, 32, 30, 82, 31, 189, 131, 17, 66, 239, 64, 10,
  149, 40, 130, 146, 54, 147, 9, 114, 4, 254, 241, 116, 110, 249, 57, 233,
  37, 55, 206, 100, 177, 119, 139, 158, 108, 75, 94, 23, 186, 152, 244, 27,
  38, 33, 188, 87, 76, 166, 228, 52, 120, 99, 247, 174, 51, 183, 3, 161,
  246, 135, 14, 178, 11, 216, 77, 172, 122, 154, 39, 253, 104, 34, 164, 230,
  219, 242, 68, 151, 180, 115, 173, 73, 212, 90, 125, 29, 22, 221, 56, 121,
  255, 204, 83, 169, 182, 112, 96, 187, 20, 106, 79, 15, 61, 223, 70, 85,
  53, 197, 217, 232, 196, 95, 136, 150, 243, 109, 129, 202, 208, 237, 144, 156,
  86, 127, 62, 248, 138, 229, 153, 226, 240, 199, 50, 12, 193, 98, 137, 126,
  0, 159, 222, 18, 163, 117, 190, 46, 225, 132, 16, 43, 128, 42, 92, 6
};

/* hash to bucket */

#define H2B(X) \
  (0xf & (  shuffled_bytes[X & 0xff] \
	  ^ shuffled_bytes[((X) >> 4) & 0xff] \
  	  ^ shuffled_bytes[((X) >> 8) & 0xff] \
	  ^ shuffled_bytes[((X) >> 12) & 0xff] \
	  ^ shuffled_bytes[((X) >> 16) & 0xff] \
	  ^ shuffled_bytes[((X) >> 20) & 0xff] \
	  ^ shuffled_bytes[((X) >> 24) & 0xff] \
	  ^ shuffled_bytes[((X) >> 28) & 0xff]))


/*(c hashtree_find)
 * struct hashtree_item * hashtree_find (struct hashtree * table,
 *				         unsigned long hash,
 *				         void * key,
 *				         struct hashtree_rules * rules);
 * 
 * Search for an entry for `key' in hash table `table' and return the
 * hash table item for that key (or 0).  `hash' is the hash value for
 * `key'.
 */
struct hashtree_item * 
hashtree_find (struct hashtree * table,
	      unsigned long hash,
	      void * key,
	      struct hashtree_rules * rules)
{
  hashtree_eq_fn eq;
  int maskc;
  unsigned long mask;
  int bucket;

  eq = EQ (rules);
  maskc = 0;
  mask = hashtree_masks [0];
  bucket = H2B(hash & mask);

  while (bitset_is_member (&table->nested_p, bucket))
    {
      table = (struct hashtree *)(table->children [bucket]);
      ++maskc;
      mask = hashtree_masks[maskc];
      bucket = H2B (hash & mask);
    }

  {
    struct hashtree_item * it;
    it = (struct hashtree_item *)(table->children[bucket]);
    while (it)
      if (eq (it->key, key, rules))
	return it;
      else
	it = it->next_same_hash;
  }

  return 0;
}


static int
overflows (struct hashtree_item * bucket)
{
  return (   bucket
	  && bucket->next_same_hash
	  && bucket->next_same_hash->next_same_hash
	  && bucket->next_same_hash->next_same_hash->next_same_hash);
}


/*(c hashtree_store)
 * struct hashtree_item * hashtree_store (struct hashtree * table,
 *					  unsigned long hash,
 *					  void * key,
 *					  struct hashtree_rules * rules);
 * 
 * Ensure that there is an entry for `key' in hash table `table' and
 * return the hash table item for that key.  `hash' is the hash value
 * for `key'.
 * 
 * If `key' is not already present in `table', this function uses
 * `rules->hash_item_alloc' to create a new key/value pair.
 * 
 * If an allocation failure occurs, this function returns 0.
 */
struct hashtree_item *
hashtree_store (struct hashtree * table,
		unsigned long hash,
		void * key,
		struct hashtree_rules * rules)
{
  hashtree_eq_fn eq;
  int maskc;
  long mask;
  int bucket;
  int depth;
  
  eq = EQ (rules);
  maskc = 0;
  mask = hashtree_masks [0];
  bucket = H2B(hash & mask);
  depth = 0;
  
  while (bitset_is_member (&table->nested_p, bucket))
    {
      table = (struct hashtree *)(table->children [bucket]);
      ++maskc;
      mask = hashtree_masks[maskc];
      bucket = H2B(hash & mask);
      ++depth;
    }
  
  {
    struct hashtree_item * it;
    it = (struct hashtree_item *)(table->children[bucket]);
    while (it)
      if (eq (it->key, key, rules))
	return it;
      else
	it = it->next_same_hash;
  }
  
  {
    if (   (depth < 3)
	&& (overflows ((struct hashtree_item *)table->children [bucket])))
      {
	struct hashtree * newtab;

	newtab = HASH_ALLOC(rules) (rules);
	if (!newtab)
	  return 0;
	mem_set0 ((char *)newtab, sizeof (*newtab));
	newtab->parent = table;
	{
	  struct hashtree_item * them;
	  unsigned long newmask;
	  them = (struct hashtree_item *)table->children[bucket];
	  newmask = hashtree_masks[maskc + 1];
	  while (them)
	    {
	      struct hashtree_item * save = them->next_same_hash;
	      int new_buck = H2B(them->hash & newmask);
	      them->next_same_hash = ((struct hashtree_item *)
				      newtab->children[new_buck]);
	      ((struct hashtree_item **)newtab->children)[new_buck] = them;
	      them->table = newtab;
	      them = save;
	      ++newtab->refs;
	      --table->refs;
	    }
	  ((struct hashtree **)table->children)[bucket] = newtab;
	  bitset_adjoin (&table->nested_p, bucket);
	  ++table->refs;
	  table = newtab;
	  bucket = H2B(hash & newmask);
	}
      }
  }

  {
    struct hashtree_item  * it;

    it = ITEM_ALLOC(rules) (key, rules);
    if (!it)
      return 0;
    it->hash = hash;
    it->table = table;
    /* KEY and BINDING are to be set in hash_item_alloc */
    it->next_same_hash = (struct hashtree_item *)table->children [bucket];
    ((struct hashtree_item **)table->children)[bucket] = it;
    ++table->refs;
    return it;
  }
}


/*(c hashtree_delete)
 * void hashtree_delete (struct hashtree_item * it,
 *			 struct hashtree_rules * rules);
 * 
 * Remove hash table item `it' from its hash table.
 * 
 * To remove a particular key, use `hashtree_find' to retrieve
 * its key/value pair, and `hashtree_delete' to remove that pair
 * from the tree.
 * 
 * This function does not free any storage assocated with
 * the key or the binding.
 * 
 * This function does call `rules->free_hash_item' to free `it'.
 */
void
hashtree_delete (struct hashtree_item * it, struct hashtree_rules * rules)
{
  if (it)
    {
      struct hashtree * table;
      unsigned long hash;
      int depth;
      int bucket;
      struct hashtree_item ** pos;
      
      table = it->table;
      hash = it->hash;
      depth = (table->parent
	       ? (table->parent->parent
		  ? (table->parent->parent->parent
		     ? 3
		     : 2)
		  : 1)
	       : 0);
      bucket = H2B (hash & hashtree_masks [depth]);
      pos = (struct hashtree_item **)&table->children [bucket];
      
      while (*pos != it)
	pos = &(*pos)->next_same_hash;

      *pos = it->next_same_hash;

      FREE_HASH_ITEM(rules) (it, rules);

      --table->refs;
      while (!table->refs && depth)
	{
	  struct hashtree * empty_table;
	  empty_table = table;
	  table = table->parent;
	  --depth;
	  bucket = H2B(hash & hashtree_masks [depth]);
	  --table->refs;
	  table->children[bucket] = 0;
	  bitset_remove (&table->nested_p, bucket);
	  FREE_HASH (rules) (empty_table, rules);
	}
    }
}

