/* super.c - lazilly constructed DFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/rx/bits-tree-rules.h"
#include "hackerlab/rx/dfa-cache.h"
#include "hackerlab/rx/super.h"


/* __STDC__ prototypes for static functions */
static void free_some_dfa_memory (void * ign, size_t needed);
static int rx_dfa_cache_prepare (size_t amt);
static void * rx_dfa_cache_malloc (size_t size);
static void * rx_dfa_cache_soft_malloc (size_t size);
static void rx_dfa_cache_free (void * mem);
static struct rx_superset * rx_protect_superset (struct rx_nfa * rx, struct rx_superset * set);
static void rx_release_superset (struct rx_superset *set);
static struct rx_superset * rx_superset_cons (struct rx_nfa * rx,
					      struct rx_nfa_state *car,
					      struct rx_superset *cdr);
static struct hashtree_item * superset_allocator (void * val, struct hashtree_rules * rules);
static struct rx_superset * rx_superstate_eclosure_union (struct rx_nfa * rx,
							  struct rx_superset *set,
							  struct rx_nfa_state_set *ecl) ;
static int supersetcmp (void * va, void * vb, struct hashtree_rules * rules);
static struct hashtree * super_hash_allocator (struct hashtree_rules * rules);
static void super_hash_liberator (struct hashtree * hash, struct hashtree_rules * rules);
static void superset_hash_item_liberator (struct hashtree_item * it,
					  struct hashtree_rules * rules);
static void install_cache_miss_transition (struct rx_super_edge * e);
static void semifree_superstate ();
static void refresh_semifree_superstate (struct rx_superstate * super);
static void rx_refresh_this_superstate (struct rx_superstate * superstate);
static struct rx_superstate * rx_superstate (struct rx_nfa *rx,
					     struct rx_superset *set,
					     int storage_unit_size);
static int solve_destination (struct rx_nfa * rx,
			      struct rx_inx * inx_out,
			      struct rx_super_edge * e,
			      int storage_unit_size);
static int compute_super_edge_cset (struct rx_nfa *rx,
				    bits csetout,
				    struct rx_superstate * superstate,
				    unsigned int chr);
static struct rx_super_edge * rx_super_edge (struct rx_nfa *rx, struct rx_superstate *super, bits cset);
static int install_partial_transition  (struct rx_superstate *super,
					struct rx_inx *answer,
					struct rx_super_edge * edge,
					bitset_subset set,
					int chr);


#define COPIES_32	\
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X


#define COPIES_64	\
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X

#define COPIES_256 \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, \
  X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X

#if rx_page1_size21 == 256
#  define COPIES_PAGE1_21_SIZE  COPIES_256
#elif rx_page1_size21 == 32
#  define COPIES_PAGE1_21_SIZE  COPIES_32
#else
#  error you broke fragile code
#endif

#if rx_page2_size21 == 256
#  define COPIES_PAGE2_21_SIZE  COPIES_256
#elif rx_page2_size21 == 32
#  define COPIES_PAGE2_21_SIZE  COPIES_32
#else
#  error you broke fragile code
#endif


#if bits_per_subset == 32
#  define COPIES_BPS COPIES_32
#  define COPIES_PAGE2_SIZE X, X, X, X, X, X, X, X
#  define COPIES_PAGE3_21_SIZE X, X, X, X, X, X, X, X
#elif bits_per_subset == 64
#  define COPIES_BPS COPIES_64
#  define COPIES_PAGE2_SIZE X, X, X, X 
#  define COPIES_PAGE3_21_SIZE X, X, X, X
#else
#  error "odd bits_per_subset in super.c"
#endif


/* Default value for leaf nodes of multi-level transition tables:
 */
#undef X
#define X {0, 0, (void *)rx_cache_miss, 0}
static struct rx_inx rx_shared_cache_miss_page[256] = {COPIES_256};

/* Fixed backtrack page for leaf nodes of multi-level transition tables:
 */
#undef X
#define X {0, 0, (void *)rx_backtrack, 0}
static struct rx_inx rx_shared_backtrack_page[256] = {COPIES_256};

/* Fixed value for high-surrogate leaf nodes of 16-bit tables:
 */
#undef X
#define X {0, 0, (void *)rx_huge_char, 0}
static struct rx_inx rx_huge_char_page[256] = {COPIES_256};

#ifdef RX_LARGE_TABLES

/* Default value for page2 of 21-bit tables:
 */
#undef X
#define X rx_shared_cache_miss_page
static struct rx_inx * rx_huge_char_second_level_page[rx_page2_size21] = { COPIES_256 };

/* Default value for huge_char_transitions:
 */
#undef X
#define X rx_huge_char_second_level_page
static struct rx_inx ** rx_default_huge_char_transitions[rx_page1_size21] = { COPIES_32 };

#else /* ndef RX_LARGE_TABLES */

#undef X
#define X rx_shared_cache_miss_page
static struct rx_inx * rx_default_small_table_page2[rx_page2_size] = { COPIES_PAGE2_SIZE };

#undef X
#define X rx_shared_backtrack_page
static struct rx_inx * rx_small_table_backtrack_page2[rx_page2_size] = { COPIES_PAGE2_SIZE };

#undef X
#define X rx_huge_char_page
static struct rx_inx * rx_huge_char_page2[rx_page2_size] = { COPIES_PAGE2_SIZE };


#undef X
#define X rx_shared_cache_miss_page
static struct rx_inx * rx_huge_char_third_level_page[rx_page3_size21] = { COPIES_PAGE3_21_SIZE };

#undef X 
#define X rx_huge_char_third_level_page
static struct rx_inx ** rx_huge_char_second_level_page[rx_page2_size21] = { COPIES_PAGE2_21_SIZE };

#undef X
#define X rx_huge_char_second_level_page
static struct rx_inx *** rx_default_huge_char_transitions[rx_page1_size21] = { COPIES_PAGE1_21_SIZE };

#endif


/************************************************************************
 *(h0 "The Superstate DFA" 
 *    :subtitle "rx/super.c"
 *    :includes ("rx/super.h"))
 * 
 * It is well known that there is a mapping from non-deterministic
 * finite-state automata to equivalent deterministic finite-state
 * automata (a good text book about compiler construction explains
 * this). That mapping is important to applications which use regular
 * expressions for efficient pattern matching.  When a regular
 * expression comparison is implemented using a single pass with a
 * non-deterministic automata with `K' states, the comparison
 * algorithm may run for a number of steps which is proportional to
 * `K*N' for a length `N' input string (where each step is relatively
 * expensive).  When implemented using backtracking, `N**K'
 * (relatively inexpensive) steps may be required.  On the other hand,
 * when regular expression comparison is implemented using a
 * deterministic automata, the algorithm requires a number of
 * instructions which is linear in the length of the input string
 * (order `N' (inexpensive) steps).
 *
 * The catch is that, using brute force, it is expensive (in time and
 * space) to build a DFA from a typical regular expression NFA.  An
 * NFA with `K' states can yield a DFA with order `2^K' states.  
 * 
 * The trick is that, during most matches using typical regexps, only
 * a small portion of the total potential number of DFA states are
 * actually needed -- time and space can be saved by building DFA
 * states on-demand.  During any match, only two DFA states are needed
 * at a time -- if the total number of DFA states encountered during a
 * match is very large, space can be saved (at the expense of time) by
 * discarding less frequently used DFA states and reconstructing them
 * on demand.
 * 
 * The functions in this chapter do the trick: they build a DFA on
 * demand, keeping a cache of frequently used states, and discarding
 * infrequently used states if the cache grows too large.
 * 
 * As a result, DFA-style matching using the functions here provide
 * matching in order `N' (inexpensive) steps for most patterns (by
 * acting like a DFA match).  In the worst case (when the cache is not
 * effective), these functions provide matching in order `N*K'
 * (relatively expensive) steps (by acting like a one-pass NFA match).
 *
 */
/*(menu)
 */

/*(h1 "The DFA Virtual Machine")
 *
 * The view taken by the design of Rx is that a DFA built from a
 * regexp is a kind of program which can be interpreted by a 
 * virtual machine.
 *
 * The basic operation of this machine is that it has one register: a
 * DFA state.  In each cycle, it reads one input character and
 * tries to advance to the next DFA state by looking that character up
 * in the transition table for the current state.  
 *
 * The machine's outputs are the state label of the current DFA state,
 * a flag that indicates whether or not the current state has outgoing
 * edges (whether or not any input characters are valid), and a signal
 * that occurs when there is no transition for an input character.
 *
 * Implementing this virtual machine is complicated by the fact that
 * its program, the DFA, is not constructed all at once and might not
 * exist in its entirety at any one time.  The virtual machine interpreter
 * has to be able to handle the case when the next state reached after
 * some input character hasn't been built yet or has been flushed
 * from the DFA cache.
 */



/*(include-documentation "super.h")
 */

/************************************************************************
 * The Rx DFA Cache
 * 
 * The DFA state cache is kept in a structure referenced through
 * the global pointer `rx_default_cache'.  In the future, this
 * may be exposed as an interface to the library: allowing separate
 * caches for regexps with different priorities.
 */

struct rx_cache;

struct rx_cache
{
  struct hashtree_rules superset_hashtree_rules;

  struct rx_superstate * lru_superstate;
  /*
   * The least recently used live superstate.  This is the next
   * superstate that will become semifree.  Superstates are linked on
   * this queue by `next_recyclable' and `prev_recyclable'.
   */
     
  struct rx_superstate * semifree_superstate;
  /*
   * The least recently used semifree superstate.  This is the next
   * superstate that will become completely freed.  Superstates are
   * linked on this queue by `next_recyclable' and `prev_recyclable'.
   */
     
  /* Allocation counts.  The number of various kinds of objects
   * currently allocated.  This is used for debugging and for tuning
   * the rate at which states are semifreed.
   */
  int hash_tables;
  int supersets;
  int super_edges;
  int superstates;
  int semifree_superstates;

  /* The ratio of cache hits to cache misses when searching for
   * superstates.  This ratio is weighted in favor of recent
   * cache probes.
   */
  int hits;
  int misses;
  int total_hits;
  int total_misses;

  /* A hash table mapping `car' and `cdr' to NFA states.
   */
  struct hashtree superset_table;
};

static struct rx_cache * rx_default_cache;


int rx_superstate_counter = 0;



/************************************************************************
 * DFA Cache Allocation
 * 
 * These malloc-like functions add or subtract from the `bytes_used' field
 * of the DFA state cache.  They discard states from an over-full cache.
 */

#ifndef RX_DEFAULT_DFA_CACHE_SIZE
/* This is an upper bound on the number of bytes that may (normally)
 * be allocated for DFA states.  If this threshold would be exceeded,
 * Rx tries to flush some DFA states from the cache.
 */
#define RX_DEFAULT_DFA_CACHE_SIZE (1 << 20)
#endif

alloc_limits rx__dfa_alloc_limits;


static void
free_some_dfa_memory (void * ign, size_t needed)
{
  while (   (lim_in_use (rx__dfa_alloc_limits) + needed > lim_threshold (rx__dfa_alloc_limits))
	 && (0 <= rx__really_free_superstate ()))
    ;
}


void
rx__init_dfa_alloc_limits (void)
{
  static int init = 1;

  if (init)
    {
      rx__dfa_alloc_limits = make_alloc_limits ("Rx DFA cache",
						RX_DEFAULT_DFA_CACHE_SIZE,
						0,
						0,
						free_some_dfa_memory,
						0);
      init = 0;
    }
}


size_t
rx_dfa_cache_failure_pt (void)
{
  rx__init_dfa_alloc_limits ();
  return lim_failure_pt (rx__dfa_alloc_limits);
}

void
rx__dfa_cache_statistics (size_t * threshold,
			 size_t * failure_pt,
			 size_t * in_use,
			 size_t * high_water_mark,
			 int * hits,
			 int * misses,
			 int * total_hits,
			 int * total_misses)
{
  rx__init_dfa_alloc_limits ();
  if (threshold)
    *threshold = rx_dfa_cache_threshold ();
  if (failure_pt)
    *failure_pt = rx_dfa_cache_failure_pt ();
  if (in_use)
    *in_use = rx_dfa_cache_in_use ();
  if (high_water_mark)
    *high_water_mark = rx_dfa_cache_high_water_mark ();
  if (hits)
    *hits = rx_default_cache->hits;
  if (misses)
    *misses = rx_default_cache->misses;
  if (total_hits)
    *total_hits = rx_default_cache->total_hits;
  if (total_misses)
    *total_misses = rx_default_cache->total_misses;
}


void
rx_set_dfa_cache_failure_pt (size_t n)
{
  rx__init_dfa_alloc_limits ();
  lim_set_failure_pt (rx__dfa_alloc_limits, n);
}


static int
rx_dfa_cache_prepare (size_t amt)
{
  rx__init_dfa_alloc_limits ();
  return lim_prepare (rx__dfa_alloc_limits, amt);
}

/* static char * rx_dfa_cache_malloc (int size);
 * 
 * Allocate memory for the DFA state cache.  If this allocation would
 * overflow the cache then this function first tries to reduce the cache 
 * size by flushing old DFA statess.
 */
static void *
rx_dfa_cache_malloc (size_t size)
{
  void * answer;

  rx__init_dfa_alloc_limits ();
  answer = lim_malloc (rx__dfa_alloc_limits, size);
  return answer;
}


static void *
rx_dfa_cache_soft_malloc (size_t size)
{
  void * answer;

  rx__init_dfa_alloc_limits ();
  answer = lim_soft_malloc (rx__dfa_alloc_limits, size);
  return answer;
}


/* static void rx_dfa_cache_free (char * mem);
 * 
 * Free memory for the DFA state cache.
 */
static void
rx_dfa_cache_free (void * mem)
{
  lim_free (rx__dfa_alloc_limits, mem);
}


/************************************************************************
 * The NFA State Set Cache
 * 
 * Each DFA state points to a set of NFA states.  That set is stored
 * in the NFA state set cache which is part of the DFA state cache.
 *
 * These are the hash-table hook functions for the NFA state set
 * cache.
 */


/*  (c rx_protect_superset)
 * struct rx_superset * rx_protect_superset (struct rx_nfa * rx, struct rx_superset * set);
 * 
 * Increment the reference count of a superset.  See xref:"struct
 * rx_superset" and xref:"rx_release_superset".
 */
static struct rx_superset *
rx_protect_superset (struct rx_nfa * rx, struct rx_superset * set)
{
  if (set)
    ++set->refs;
  return set;
}


/*  (c rx_release_superset)
 * void rx_release_superset (struct rx_superset *set);
 * 
 * Decrement the reference count on a superset.  If it becomes 0, 
 * then free storage used by the set.
 */
static void 
rx_release_superset (struct rx_superset *set)
{
  if (set && !--set->refs)
    {
      if (set->nfa_state)
	set->nfa_state->superstate_set = 0;
      rx_release_superset (set->cdr);
      hashtree_delete (&set->hash_item, &rx_default_cache->superset_hashtree_rules);
      rx_dfa_cache_free ((char *)set);
      --rx_default_cache->supersets;
    }
}


/* This adds an element to a superstate set.  These sets are lists, such
 * that lists with == elements are ==.  
 * 
 * 
 * on entry: 
 *   	CDR has at least one reference which is taken over by this
 * 	function (becomes the reference of the set returned to its cdr)
 * 
 * on exit:
 * 	set returned has one reference which is for the caller.
 */

static struct rx_superset *
rx_superset_cons (struct rx_nfa * rx,
		  struct rx_nfa_state *car,
		  struct rx_superset *cdr)
{
  struct rx_cache * cache = rx_default_cache;
  if (!car && !cdr)
    return 0;
  {
    struct rx_superset template;
    struct hashtree_item * hit;
    struct rx_superset * answer;

    template.car = car;
    template.cdr = cdr;
    template.id = rx->rx_id;
    
    rx_dfa_cache_prepare (4 * sizeof (struct hashtree *) + sizeof (struct rx_superset));
    hit = hashtree_store (&cache->superset_table,
			  (unsigned long)car ^ car->id ^ (unsigned long)cdr,
			  (void *)&template,
			  &cache->superset_hashtree_rules);
    if (!hit)
      return 0;
    answer = (struct rx_superset *)hit->key;
    rx_protect_superset (rx, answer);
    if (answer->refs != 1)
      rx_release_superset (cdr);
    return answer;
  }
}


#define rx_abs(A) (((A) > 0) ? (A) : -(A))

/*
 * static int superset_allocator (int * errn,
 *                                struct hashtree_item ** retv,
 *                                void * val,
 *                                struct hashtree_rules * rules);
 * 
 * Hash tree allocation function for supersets.
 * 
 * on entry: 
 *   	CDR of `val' at least one reference which is taken over by this
 * 	function (becomes the reference of the set returned to its cdr)
 * 
 * on exit:
 * 	Set returned has a reference count of 0.
 */
static struct hashtree_item *
superset_allocator (void * val, struct hashtree_rules * rules)
{
  struct rx_cache * cache;
  struct rx_superset * template;
  struct rx_superset * newset;
  
  cache = ((struct rx_cache *)
	   ((char *)rules
	    - (unsigned long)(&((struct rx_cache *)0)->superset_hashtree_rules)));
  template = (struct rx_superset *)val;
  newset = ((struct rx_superset *)
	    rx_dfa_cache_soft_malloc (sizeof (*template)));
  if (!newset)
    return 0;
  ++cache->supersets;

  {
    int cdrfinal;
    int cdredges;

    cdrfinal = (template->cdr ? template->cdr->state_label : 0);
    cdredges = (template->cdr ? template->cdr->has_cset_edges : 0);

    if (!template->car->state_label)
      newset->state_label = cdrfinal;
    else if (rx_abs (template->car->state_label) < rx_abs (cdrfinal))
      newset->state_label = template->car->state_label;
    else if (template->car->state_label > 0)
      newset->state_label = template->car->state_label;
    else
      newset->state_label = cdrfinal;

    newset->has_cset_edges = (template->car->has_cset_edges || cdredges);
  }

  newset->refs = 0;
  newset->id = template->id;
  newset->car = template->car;
  newset->cdr = template->cdr;
  newset->superstate[0] = 0;
  newset->superstate[1] = 0;
  newset->nfa_state = 0;
  newset->hash_item.key = (void *)newset;
  newset->hash_item.binding = 0;
  return &newset->hash_item;
}


/* This computes a union of two NFA state sets.  The sets do not have the
 * same representation though.  One is a RX_SUPERSET structure (part
 * of the superstate NFA) and the other is an NFA_STATE_SET (part of the NFA).
 * 
 * On exit, `set' has at least one reference which is for the caller.
 */
static struct rx_superset *
rx_superstate_eclosure_union (struct rx_nfa * rx,
			      struct rx_superset *set,
			      struct rx_nfa_state_set *ecl) 
{
  if (!ecl)
    {
      rx_protect_superset (rx, set);
      return set;
    }

  if (!set)
    return rx_superset_cons (rx, ecl->car,
			     rx_superstate_eclosure_union (rx, 0, ecl->cdr));

  if (set->car == ecl->car)
    return rx_superstate_eclosure_union (rx, set, ecl->cdr);

  {
    struct rx_superset * tail;
    struct rx_nfa_state * first;

    if (set->car->id < ecl->car->id)
      {
	tail = rx_superstate_eclosure_union (rx, set->cdr, ecl);
	if (!tail)
	  return 0;
	first = set->car;
      }
    else
      {
	tail = rx_superstate_eclosure_union (rx, set, ecl->cdr);
	if (!tail)
	  return 0;
	first = ecl->car;
      }

    {
      struct rx_superset * answer;
      answer = rx_superset_cons (rx, first, tail);
      if (!answer)
	{
	  rx_release_superset (tail);
	  return 0;
	}
      return answer;
    }
  }
}


static int
supersetcmp (void * va, void * vb, struct hashtree_rules * rules)
{
  struct rx_superset * a = (struct rx_superset *)va;
  struct rx_superset * b = (struct rx_superset *)vb;

  a = (struct rx_superset *)va;
  b = (struct rx_superset *)vb;
  return (   (a == b)
	  || (   a
	      && b
	      && (a->id == b->id)
	      && (a->car == b->car)
	      && (a->cdr == b->cdr)));
}


static struct hashtree *
super_hash_allocator (struct hashtree_rules * rules)
{
  struct rx_cache * cache;
  struct hashtree * it;

  cache = ((struct rx_cache *)
	   ((char *)rules
	    - (unsigned long)(&((struct rx_cache *)0)->superset_hashtree_rules)));
  it = ((struct hashtree *)
 	rx_dfa_cache_soft_malloc (sizeof (struct hashtree)));
  if (!it)
    return 0;
  ++cache->hash_tables;
  return it;
}


static void
super_hash_liberator (struct hashtree * hash, struct hashtree_rules * rules)
{
  rx_dfa_cache_free ((char *)hash);
  --rx_default_cache->hash_tables;
}

static void
superset_hash_item_liberator (struct hashtree_item * it,
			      struct hashtree_rules * rules)
{
}


/************************************************************************
 * The Default DFA State Cache
 * 
 * 
 * 
 */

static struct rx_cache default_cache = 
{
  {
    supersetcmp,
    super_hash_allocator,
    super_hash_liberator,
    superset_allocator,
    superset_hash_item_liberator,
  },
  0,				/* lru_superstate */
  0,				/* semifree_superstate */
  0,				/* hash_tables */
  0,				/* super_edges */
  0,				/* supersets */
  0,				/* superstates */
  0,				/* semifree_superstates */
  0,				/* hits */
  0,				/* misses */
  0,				/* total hits */
  0,				/* total misses */
  {				/* hash table */
    0,
    0,
    0,
    {0}
  }
};


static struct rx_cache * rx_default_cache = &default_cache;

/************************************************************************
 * Making DFA States Semifree and Restoring Them to Live State
 * 
 */

/* static void install_cache_miss_transition (struct rx_super_edge * e);
 * 
 * Install the instruction `answer' in the transition table of `super'
 * for all instructions built for `e'.  
 */
static void 
install_cache_miss_transition (struct rx_super_edge * e)
{
  struct rx_inx * transitions;

  transitions = e->inx_list;
  while (transitions)
    {
      transitions->inx = (void *)rx_cache_miss;
      transitions->data = 0;
      transitions->data_2 = (void *)e;
      transitions = transitions->next_same_edge;
    }

  e->inx_list = 0;
}


/* static void semifree_superstate ();
 * 
 * Select the most recently used live superstate that is not locked and
 * make it semifree.
 *
 * This function makes all incoming instruction frames cache miss
 * instructions.  It moves the state from the `lru_superstate' queue
 * of the cache to the `semifree_superstate' queue.
 */
static void
semifree_superstate ()
{
  struct rx_cache * cache = rx_default_cache;
  int disqualified;

  disqualified = cache->semifree_superstates;
  if (disqualified == cache->superstates)
    return;

  while (cache->lru_superstate->locks)
    {
      cache->lru_superstate = cache->lru_superstate->next_recyclable;
      ++disqualified;
      if (disqualified == cache->superstates)
	return;
    }

  {
    struct rx_superstate * it;

    it = cache->lru_superstate;
    it->next_recyclable->prev_recyclable = it->prev_recyclable;
    it->prev_recyclable->next_recyclable = it->next_recyclable;
    
    cache->lru_superstate = (it == it->next_recyclable
			     ? 0
			     : it->next_recyclable);

    if (!cache->semifree_superstate)
      {
	cache->semifree_superstate = it;
	it->next_recyclable = it;
	it->prev_recyclable = it;
      }
    else
      {
	it->prev_recyclable = cache->semifree_superstate->prev_recyclable;
	it->next_recyclable = cache->semifree_superstate;
	it->prev_recyclable->next_recyclable = it;
	it->next_recyclable->prev_recyclable = it;
      }
    {
      struct rx_super_edge *e;

      it->is_semifree = 1;
      ++cache->semifree_superstates;

      e = it->incoming_edges;
      if (e)
	{
	  e->prev_same_dest->next_same_dest = 0;
	  while (e)
	    {
	      install_cache_miss_transition (e);
	      e = e->next_same_dest;
	    }
	  e = it->incoming_edges;
	  e->prev_same_dest->next_same_dest = e;
	}
    }
  }
}


/* static void refresh_semifree_superstate (struct rx_superstate * super);
 * 
 * Make `super', which may or may not be in a semifree state, a live
 * superstate.
 * 
 * This moves the state to the back of the `lru_superstate' queue.
 */
static void 
refresh_semifree_superstate (struct rx_superstate * super)
{
  struct rx_cache * cache = rx_default_cache;

  if (cache->semifree_superstate == super)
    cache->semifree_superstate = (super->prev_recyclable == super
				  ? 0
				  : super->prev_recyclable);
  else if (cache->lru_superstate == super)
    cache->lru_superstate = (super->prev_recyclable == super
			     ? 0
			     : super->prev_recyclable);

  super->next_recyclable->prev_recyclable = super->prev_recyclable;
  super->prev_recyclable->next_recyclable = super->next_recyclable;

  if (!cache->lru_superstate)
    (cache->lru_superstate
     = super->next_recyclable
     = super->prev_recyclable
     = super);
  else
    {
      super->next_recyclable = cache->lru_superstate;
      super->prev_recyclable = cache->lru_superstate->prev_recyclable;
      super->next_recyclable->prev_recyclable = super;
      super->prev_recyclable->next_recyclable = super;
    }
  if (super->is_semifree)
    {
      super->is_semifree = 0;
      --cache->semifree_superstates;
    }
}



/* void rx_refresh_this_superstate (struct rx_superstate * superstate);
 * 
 * If this state is semifree, pass it to `refresh_semifree_superstate'.
 * Otherwise, move this state to the rear of the `lru_superstate' queue.
 */
static void
rx_refresh_this_superstate (struct rx_superstate * superstate)
{
  struct rx_cache * cache = rx_default_cache;
  if (superstate->is_semifree)
    refresh_semifree_superstate (superstate);
  else if (cache->lru_superstate == superstate)
    cache->lru_superstate = superstate->next_recyclable;
  else if (superstate != cache->lru_superstate->prev_recyclable)
    {
      superstate->next_recyclable->prev_recyclable
	= superstate->prev_recyclable;
      superstate->prev_recyclable->next_recyclable
	= superstate->next_recyclable;
      superstate->next_recyclable = cache->lru_superstate;
      superstate->prev_recyclable = cache->lru_superstate->prev_recyclable;
      superstate->next_recyclable->prev_recyclable = superstate;
      superstate->prev_recyclable->next_recyclable = superstate;
    }
}


/************************************************************************
 * Really Freeing DFA States
 * 
 * After becoming semifree, a DFA state may eventually be freed for
 * real -- meaning that the memory it occupies is recycled.
 * 
 */

/* This tries to add a new superstate to the superstate freelist.
 * It might, as a result, free some edge pieces or hash tables.
 * If nothing can be freed because too many locks are being held, fail.
 */

/* int rx__really_free_superstate ();
 * 
 * Attempt to reduce memory usage of the DFA state cache by truly
 * freeing one superstate.  Return -1 if no state could be freed, 0
 * otherwise.
 */
int
rx__really_free_superstate (void)
{
  struct rx_cache * cache = rx_default_cache;
  struct rx_superstate * it;

  if (!cache->superstates)
    return -1;

  /* If the rate of cache misses is high, we semifree superstates at a
   * rate proportional to our need to truly free states, but greater.
   * That way the total amount of work done freeing states remains
   * proportional to the work needed, but a spike in demand for memory
   * will cause a larger spike in the amount of effort put into
   * rescuing the most desirable states in the cache.
   *
   * For example, as this is written, when the cache miss rate is
   * high, states are semifreed at a rate 3x the rate at which they
   * are truly freed.  If, after a period of running mostly out of the
   * cache, we suddenly have to free 1/3 of the states in the cache to
   * make room for new states, the remaining 2/3 of the old states
   * will all have been marked semifree and thus the most useful of
   * those states will have a high probability of being rescued even
   * if the demand for memory continues.  They will have a fighting
   * chance against those new states which were needed once but which
   * aren't otherwise very useful.
   * 
   * We semifree states at this accelerated rate when the cache miss
   * rate is high because in that case, we expect the cache miss rate
   * to remain high, and so we expect the high demand on memory to
   * continue.
   *
   * When the cache miss rate is low, we look at the number of
   * semifree states compared to the total number of states.  If that
   * ratio is low (there aren't many semifree states), then we again
   * semifree states at an accelerated rate.  If that ratio is high
   * (there are quite a few semifree states), then we only semifree
   * one state for every state truly freed.  The idea is that a low
   * cache miss rate predicts a low demand on memory.  When demand on
   * memory is low, we want to maintain a constant sized queue of
   * semifree states which is a fraction of the total number of
   * states.  The existence of that queue helps us from throwing away
   * useful DFA states while demand on memory is low while the fixed
   * size of that queue means that the overhead of maintaining it is a
   * small constant multiple of the time spent truly freeing states.
   */
  if (   (cache->misses < cache->hits)
      || (3 * cache->semifree_superstates >= cache->superstates))
    semifree_superstate (cache);
  else
    {
      semifree_superstate (cache);
      semifree_superstate (cache);
      semifree_superstate (cache);
    }


  /* At this point, if no states are semifree, then all states are
   * locked and no states can be freed.
   */
  if (!cache->semifree_superstate)
    return -1;
  
  /* Remove the oldest semifree state from the queue of all semifree
   * states.  Correct the counters that count DFA states.
   */
  {
    it = cache->semifree_superstate;
    it->next_recyclable->prev_recyclable = it->prev_recyclable;
    it->prev_recyclable->next_recyclable = it->next_recyclable;
    cache->semifree_superstate = ((it == it->next_recyclable)
				  ? 0
				  : it->next_recyclable);
    --cache->semifree_superstates;
    --cache->superstates;
  }


  /* Unlink the queue of incoming edges and clear the `future' field
   * of those edges which otherwise points to this state.
   */
  if (it->incoming_edges)
    {
      struct rx_super_edge * e;
      it->incoming_edges->prev_same_dest->next_same_dest = 0;
      e = it->incoming_edges;
      while (e)
	{
	  struct rx_super_edge * et;
	  et = e;
	  e = e->next_same_dest;
	  et->prev_same_dest = et->next_same_dest = 0;
	  et->future = 0;
	}
    }


  /* Free all outgoing edges and remove them from the `incoming_edges'
   * queue of their destination states.
   */
  {
    struct rx_super_edge *tc;
    tc = it->outgoing_edges;
    while (tc)
      {
	struct rx_super_edge *tct;

	tct = tc;
	tc = tc->next_same_present;

	if (tct->future)
	  {
	    if (tct->future->incoming_edges == tct)
	      {
		tct->future->incoming_edges = tct->next_same_dest;
		if (tct->future->incoming_edges == tct)
		  tct->future->incoming_edges = 0;
	      }
	    tct->next_same_dest->prev_same_dest = tct->prev_same_dest;
	    tct->prev_same_dest->next_same_dest = tct->next_same_dest;
	  }
	bits_free (tct->cset);
	rx_dfa_cache_free ((char *)tct);
	--cache->super_edges;
      }
  }

#ifdef RX_LARGE_TABLES
  {
    /* free second level transition tables */
    if (it->storage_unit_size == 2)
      {
	int x;

	for (x = 0; x < 256; ++x)
	  {
	    if (   (((struct rx_inx **)it->transitions)[x] != rx_shared_cache_miss_page)
		&& (((struct rx_inx **)it->transitions)[x] != rx_shared_backtrack_page)
		&& (((struct rx_inx **)it->transitions)[x] != rx_huge_char_page))
	      rx_dfa_cache_free ((void *)((struct rx_inx **)it->transitions)[x]);
	  }
      }
  }
#else
  {
    /* free second level transition tables */
    if (it->storage_unit_size == 2)
      {
	int x;

	for (x = 0; x < 256; ++x)
	  {
	    if (   (((struct rx_inx ***)it->transitions)[x] != rx_default_small_table_page2)
		&& (((struct rx_inx ***)it->transitions)[x] != rx_small_table_backtrack_page2)
		&& (((struct rx_inx ***)it->transitions)[x] != rx_huge_char_page2))
	      {
		int y;
		struct rx_inx ** page2;

		page2 = ((struct rx_inx ***)it->transitions)[x];
		for (y = 0; y < rx_page2_size; ++y)
		  {
		    if (page2[y] != rx_shared_cache_miss_page)
		      {
			rx_dfa_cache_free ((void *)page2[y]);
		      }
		  } 
		rx_dfa_cache_free ((void *)page2);
	      }
	  }
      }
  }
#endif

  if (it->huge_char_transitions != rx_default_huge_char_transitions)
    {
      int x;

#ifdef RX_LARGE_TABLES
      for (x = 0; x < rx_page1_size21; ++x)
	{
	  struct rx_inx ** sub;

	  sub = it->huge_char_transitions[x];
	  if (sub != rx_huge_char_second_level_page)
	    {
	      int z;

	      for (z = 0; z < rx_page2_size21; ++z)
		{
		  struct rx_inx * leaf;

		  leaf = sub[z];
		  if (leaf != rx_shared_cache_miss_page)
		    rx_dfa_cache_free ((void *)leaf);
		}
	      rx_dfa_cache_free ((void *)sub);
	    }
	}
#else
      for (x = 0; x < rx_page1_size21; ++x)
	{
	  struct rx_inx *** sub;

	  sub = it->huge_char_transitions[x];
	  if (sub != rx_huge_char_second_level_page)
	    {
	      int z;

	      for (z = 0; z < rx_page2_size21; ++z)
		{
		  struct rx_inx ** subsub;

		  subsub = sub[z];

		  if (subsub != rx_huge_char_third_level_page)
		    {
		      int zz;

		      for (zz = 0; zz < rx_page3_size21; ++zz)
			{
			  struct rx_inx * leaf;

			  leaf = subsub[zz];
			  if (leaf != rx_shared_cache_miss_page)
			    rx_dfa_cache_free ((void *)leaf);
			}
		      rx_dfa_cache_free ((void *)subsub);
		    }
		}
	      rx_dfa_cache_free ((void *)sub);
	    }
	}
#endif
      rx_dfa_cache_free ((void *)it->huge_char_transitions);
    }
  
  /* Clear the pointer to this state in the NFA set of this state.
   */
  if (it->members->superstate[0] == it)
    it->members->superstate[0] = 0;
  if (it->members->superstate[1] == it)
    it->members->superstate[1] = 0;

  /* Release the NFA set.
   */
  rx_release_superset (it->members);

  /* Free the memory of this DFA state.
   */
  rx_dfa_cache_free ((char *)it);

  return 0;
}


/************************************************************************
 *(h1 "Building DFA States")
 * 
 * 
 * 
 */

/* struct rx_superstate * rx_superstate (struct rx_nfa *rx,
 *					 struct rx_superset *set,
 *					 int storage_unit_size);
 * 
 * Retrieve a DFA state from the cache or construct a new state for a
 * give NFA state set.
 * 
 * This may cause old states to be flushed from the DFA state cache.
 */
static struct rx_superstate *
rx_superstate (struct rx_nfa *rx,
	       struct rx_superset *set,
	       int storage_unit_size)
{
  struct rx_cache * cache;
  struct rx_superstate * superstate;
  int has_huge_table;

  has_huge_table = 0;

  if (rx->local_cset_size == 256)
    has_huge_table = 0;
  else if (rx->local_cset_size == (1 << 21))
    has_huge_table = 1;
  else
    panic ("odd sized cset in rx_superstate");

  cache = rx_default_cache;
  superstate = 0;

  /* Does the superstate already exist in the cache? */
  if (set->superstate[storage_unit_size - 1])
    {
      ++cache->hits;
      ++cache->total_hits;
      while ((cache->hits + cache->misses) > 256)
	{
	  cache->hits >>= 1;
	  cache->misses >>= 1;
	}
      
      superstate = set->superstate[storage_unit_size - 1];
      rx_refresh_this_superstate (superstate);
      return superstate;
    }


  /* This point reached only for cache misses. */
  ++cache->misses;
  ++cache->total_misses;
  while ((cache->hits + cache->misses) > 256)
    {
      cache->hits >>= 1;
      cache->misses >>= 1;
    }


  if (!has_huge_table)
    {
      int superstate_size;
      superstate_size = (sizeof (*superstate) + (sizeof (struct rx_inx) * 256));
      superstate = ((struct rx_superstate *) rx_dfa_cache_malloc (superstate_size));

      if (!superstate)
	return 0;

      superstate->storage_unit_size = storage_unit_size;
      superstate->has_huge_table = 0;

      mem_set0 ((void *)&superstate->transitions, sizeof (rx_shared_cache_miss_page));
      /* mem_move ((void *)&superstate->transitions, (void *)rx_shared_cache_miss_page, sizeof (rx_shared_cache_miss_page)); */
    }
  else
    {
      int x;
      int superstate_size;

      superstate_size = 0;

      if (storage_unit_size == 2)
	superstate_size = (sizeof (*superstate) + (sizeof (struct rx_inx *) * 256));
      else if (storage_unit_size == 1)
	superstate_size = (sizeof (*superstate) + (sizeof (struct rx_inx) * 256));
      else
	panic ("odd storage unit size in rx_superstate");

      superstate = ((struct rx_superstate *) rx_dfa_cache_malloc (superstate_size));
      if (!superstate)
	return 0;

      superstate->storage_unit_size = storage_unit_size;
      superstate->has_huge_table = 1;
      
      if (storage_unit_size == 2)
	{
#ifdef RX_LARGE_TABLES
	  {
	    for (x = 0; x < 0xd8; ++x)
	      ((struct rx_inx **)superstate->transitions)[x] = rx_shared_cache_miss_page;
	    for (x = 0xd8; x < 0xdc; ++x)
	      ((struct rx_inx **)superstate->transitions)[x] = rx_huge_char_page;
	    for (x = 0xdc; x < 0xe0; ++x)
	      ((struct rx_inx **)superstate->transitions)[x] = rx_shared_backtrack_page;
	    for (x = 0xe0; x < 256; ++x)
	      ((struct rx_inx **)superstate->transitions)[x] = rx_shared_cache_miss_page;
	  }
#else
	  {
	    for (x = 0; x < 0xd8; ++x)
	      ((struct rx_inx ***)superstate->transitions)[x] = rx_default_small_table_page2;
	    for (x = 0xd8; x < 0xdc; ++x)
	      ((struct rx_inx ***)superstate->transitions)[x] = rx_huge_char_page2;
	    for (x = 0xdc; x < 0xe0; ++x)
	      ((struct rx_inx ***)superstate->transitions)[x] = rx_small_table_backtrack_page2;
	    for (x = 0xe0; x < 256; ++x)
	      ((struct rx_inx ***)superstate->transitions)[x] = rx_default_small_table_page2;
	  }
#endif
	}
      else if (storage_unit_size == 1)
	{
	  mem_move ((void *)&superstate->transitions, (void *)rx_shared_cache_miss_page, 128 * sizeof (struct rx_inx));

	  /* no valid multi-byte encodings begin with 80..c1
	   */
	  for (x = 0x80; x < 0xc2; ++x)
	    {
	      ((struct rx_inx *)&superstate->transitions)[x].data = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].data_2 = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].inx = (void *)rx_bogus_utf8;
	      ((struct rx_inx *)&superstate->transitions)[x].next_same_edge = (struct rx_inx *)0;
	    }
	  /* valid 2 byte encodings begin with c2..df
	   */
	  for (x = 0xc2; x < 0xe0; ++x)
	    {
	      ((struct rx_inx *)&superstate->transitions)[x].data = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].data_2 = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].inx = (void *)rx_2byte_utf8;
	      ((struct rx_inx *)&superstate->transitions)[x].next_same_edge = (struct rx_inx *)0;
	    }
	  /* valid 3 byte encodings begin with e0..ef
	   */
	  for (x = 0xe0; x < 0xf0; ++x)
	    {
	      ((struct rx_inx *)&superstate->transitions)[x].data = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].data_2 = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].inx = (void *)rx_3byte_utf8;
	      ((struct rx_inx *)&superstate->transitions)[x].next_same_edge = (struct rx_inx *)0;
	    }
	  /* valid 4 byte encodings begin with f0..f4
	   */
	  for (x = 0xf0; x < 0xf5; ++x)
	    {
	      ((struct rx_inx *)&superstate->transitions)[x].data = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].data_2 = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].inx = (void *)rx_4byte_utf8;
	      ((struct rx_inx *)&superstate->transitions)[x].next_same_edge = (struct rx_inx *)0;
	    }
	  /* no valid multi-byte encodings begin with f5..ff
	   */
	  for (x = 0xf5; x <= 0xff; ++x)
	    {
	      ((struct rx_inx *)&superstate->transitions)[x].data = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].data_2 = (void *)0;
	      ((struct rx_inx *)&superstate->transitions)[x].inx = (void *)rx_bogus_utf8;
	      ((struct rx_inx *)&superstate->transitions)[x].next_same_edge = (struct rx_inx *)0;
	    }
	}
      else
	panic ("odd storage unit size in rx_superstate");
    }

  superstate->huge_char_transitions = rx_default_huge_char_transitions;
  ++cache->superstates;
  superstate->seq = rx_superstate_counter++;
  superstate->table_id = -1;

  if (!cache->lru_superstate)
    (cache->lru_superstate
     = superstate->next_recyclable
     = superstate->prev_recyclable
     = superstate);
  else
    {
      superstate->next_recyclable = cache->lru_superstate;
      superstate->prev_recyclable = cache->lru_superstate->prev_recyclable;
      (  superstate->prev_recyclable->next_recyclable
       = superstate->next_recyclable->prev_recyclable
       = superstate);
    }
  superstate->outgoing_edges = 0;
  superstate->incoming_edges = 0;
  superstate->members = set;
  set->superstate[storage_unit_size - 1] = superstate;
  rx_protect_superset (rx, set);
  superstate->is_semifree = 0;
  superstate->rx_id = rx->rx_id;
  superstate->locks = 0;
  return superstate;
}


/*(c rx_nfa_state_to_superstate)
 * struct rx_superstate * 
 * rx_nfa_state_to_superstate (struct rx_nfa * rx,
 *			       struct rx_nfa_state * nfa_state,
 * 			       int storage_unit_size);
 * 
 * Construct the DFA state whose NFA state set is the epsilon closure
 * of `nfa_state' in the NFA `rx'.  Return that superstate, with no
 * new locks applied to it.
 */
struct rx_superstate *
rx_nfa_state_to_superstate (struct rx_nfa * rx,
			    struct rx_nfa_state * nfa_state,
			    int storage_unit_size)
{
  struct rx_nfa_state_set * epsilon_closure;
  struct rx_superset * contents;
  struct rx_superstate * answer;
  
  /* Compute the superstate set for this NFA state and
   * aquire a reference count for it.
   * 
   * Ensure that a reference to this set is cached in 
   * the NFA state.
   */
  if (nfa_state->superstate_set)
    {
      contents = nfa_state->superstate_set;
      rx_protect_superset (rx, contents);
    }
  else
    {
      epsilon_closure = rx_state_closure (rx, nfa_state);
      if (!epsilon_closure)
	return 0;

      contents = rx_superstate_eclosure_union (rx,
					       0,
					       epsilon_closure);
      if (!contents)
	return 0;

      /* rx_superstate_eclosure_union gives us a reference count
       */
      nfa_state->superstate_set = contents;
      contents->nfa_state = nfa_state;
    }

  /* Aquire a superstate for this superstate_set and drop
   * the reference count on the set.
   */
  if (   contents->superstate[storage_unit_size - 1]
      && (contents->superstate[storage_unit_size - 1]->rx_id == rx->rx_id))
    {
      answer = contents->superstate[storage_unit_size - 1];
      /* Treat this as a cache hit that contributes longevity
       * to the cache entry and that turns a semifree state
       * into a live state:
       */
      rx_refresh_this_superstate (answer);
      rx_release_superset (contents);
      return answer;
    }
  else
    {
      answer = rx_superstate (rx, contents, storage_unit_size);
      rx_release_superset (contents);
      return answer;		/* might be 0 */
    }
}




/* void rx_clear_superstate_table_ids (struct rx_nfa * nfa);
 * 
 * SHOULD BE VOID?
 */
void
rx_clear_superstate_table_ids (struct rx_nfa * nfa)
{
  struct rx_superstate * qs[2];
  int x;

  qs[0] = rx_default_cache->semifree_superstate;
  qs[1] = rx_default_cache->lru_superstate;
  for (x = 0; x < 2; ++x)
    {
      struct rx_superstate * q;
      struct rx_superstate * it;

      q = qs[x];
      it = q;
      if (q)
	do
	  {
	    it->table_id = -1;
	    it = it->next_recyclable;
	  }
	while (it != q);
    }
}


/************************************************************************
 * Low Level Support for Handling Cache Misses in the Dfa State Cache 
 * 
 * 
 */

/*(c solve_destination)
 * static void solve_destination (struct rx_nfa * rx,
 *                                struct rx_inx * inx_out,
 *                                struct rx_super_edge * e,
 *                                int storage_unit_size);
 * 
 * Compute the destination state for DFA edge `e'.
 *
 * As a side effect, set the `future' field of `e'.
 * 
 * Return an `rx_next_char' instruction frame in `inx_out'.  The
 * `data' field of the instruction will point to the destination
 * state.  This instruction can be copied into transition table
 * entries for edge `e'.
 */
static int
solve_destination (struct rx_nfa * rx,
		   struct rx_inx * inx_out,
		   struct rx_super_edge * e,
		   int storage_unit_size)
{
  struct rx_superset * nfa_state;
  struct rx_superset * solution;
  struct rx_superstate *dest;

  if (e->is_backtrack)
    {
      inx_out->inx = (void *) rx_backtrack;
      inx_out->data = 0;
      inx_out->data_2 = 0;
      return 0;
    }

  solution = 0;

  /* Iterate over all NFA states in the state set of this superstate. */
  for (nfa_state = e->present->members; nfa_state; nfa_state = nfa_state->cdr)
    {
      struct rx_nfa_edge * ne;

      /* Iterate over all edges of each NFA state. */
      for (ne = nfa_state->car->edges; ne; ne = ne->next)

        /* If we find an edge that is labeled with 
	 * the characters we are solving for.....
	 */
	if ((ne->type == ne_cset) && bits_is_subset (ne->cset, e->cset))
	  {
	    struct rx_nfa_state * n;
	    struct rx_superset * old_sol;

	    n = ne->dest;
	    old_sol = solution;
	    {
	      struct rx_nfa_state_set * set;
	      set = rx_state_closure (rx, n);

	      if (!set)
		{
		  rx_release_superset (old_sol);
		  return -1;
		}

	      solution = rx_superstate_eclosure_union (rx, solution, set);

	      if (!solution)
		{
		  rx_release_superset (old_sol);
		  return -1;
		}
	    }
	    rx_release_superset (old_sol);
	  }
    }

  if (solution == 0)
    {
      e->is_backtrack = 1;
      inx_out->inx = (void *) rx_backtrack;
      inx_out->data = 0;
      inx_out->data_2 = 0;
      return 0;
    }

  dest = rx_superstate (rx, solution, storage_unit_size);
  rx_release_superset (solution);
  if (!dest)
    return -1;
  e->future = dest;

  if (!dest->incoming_edges)
    {
      dest->incoming_edges = e;
      e->next_same_dest = e->prev_same_dest = e;
    }
  else
    {
      e->next_same_dest = dest->incoming_edges;
      e->prev_same_dest = dest->incoming_edges->prev_same_dest;
      e->next_same_dest->prev_same_dest = e;
      e->prev_same_dest->next_same_dest = e;
    }
  inx_out->data = (void *)&dest->transitions;
  inx_out->data_2 = (void *) dest->members->state_label;
  inx_out->inx = (void *) rx_next_char;
  return 0;
}


/* static int compute_super_edge_cset (struct rx_nfa *rx,
 *				       bits csetout,
 *				       struct rx_superstate * superstate,
 *				       t_uchar chr);
 * 
 * For DFA state `superstate' and character `chr', compute a character
 * set which is the label of the DFA edge defining transitions out of
 * `superstate' on input character `chr'.
 *
 * This character set can then be used to compute the NFA state set of
 * the destination state of the transition.  See `solve_destination'.
 * 
 * 0 == backtrack
 * n > 0 == n_nfa_edges
 * n < 0 == ESPACE
 */
static int
compute_super_edge_cset (struct rx_nfa *rx,
			 bits csetout,
			 struct rx_superstate * superstate,
			 unsigned int chr)
{
  int n_nfa_edges;
  struct rx_superset * stateset;

  stateset = superstate->members;

  if (bits_fill (csetout))
    return -1;

  n_nfa_edges = 0;
  while (stateset)
    {
      struct rx_nfa_edge *e;

      for (e = stateset->car->edges; e; e = e->next)
	if (e->type == ne_cset)
	  {
	    if (!bits_is_member (e->cset, chr))
	      {
		if (bits_difference (csetout, e->cset))
		  return -1;
	      }
	    else
	      {
		if (bits_intersection (csetout, e->cset))
		  return -1;
	      }
	    ++n_nfa_edges;
	  }
      stateset = stateset->cdr;
    }
  return n_nfa_edges;
}


/* static struct rx_super_edge * rx_super_edge (struct rx_nfa *rx, struct rx_superstate *super, bits cset);
 * 
 * Construct a new DFA edge, originating in state `super', and
 * defining transitions for characters in `cset'.
 */
static struct rx_super_edge *
rx_super_edge (struct rx_nfa *rx, struct rx_superstate *super, bits cset)
{
  struct rx_super_edge *tc;

  tc = ((struct rx_super_edge *) rx_dfa_cache_malloc (sizeof (struct rx_super_edge)));
  if (!tc)
    return 0;
  mem_set0 ((void *)tc, sizeof (struct rx_super_edge));
  ++rx_default_cache->super_edges;
  tc->next_same_present = super->outgoing_edges;
  super->outgoing_edges = tc;
  tc->cset = cset;
  tc->present = super;
  tc->inx_list = 0;
  tc->is_backtrack = 0;
  return tc;
}



/* static void install_partial_transition (struct rx_superstate *super,
 *					   struct rx_inx *answer,
 *					   struct rx_super_edge * edge,
 *					   bitset_subset set,
 *					   int offset);
 * 
 * Fill in entries in the transition table of `super'.
 *
 * Entry `N' is filled if `N' is in the range:
 *
 *	offset .. offset + bits_per_subset
 * 
 * and:
 *
 *	 set & (1 << (N - offset))
 *
 * is not 0.
 *
 * Filled entries are filled with a copy of `*answer'.
 * 
 * This function is useful when handling a cache miss.  It is
 * expensive to fill in the entire transition table and often, only a
 * few entries in a narrow range (such as lower case letters) will
 * every be needed.  When a missing entry is discovered, we fill in
 * only a narrow region of the table around the missing entry.
 */
static int
install_partial_transition  (struct rx_superstate *super,
			     struct rx_inx *answer,
			     struct rx_super_edge * edge,
			     bitset_subset set,
			     int chr)
{
  int x;
  bitset_subset pos;
  struct rx_inx * inxs;

  if (!super->has_huge_table || ((super->storage_unit_size == 1) && (chr < 0x80)))
    inxs = rx_subset_transitions8 (super->transitions, chr);
  else
    {
      if ((super->storage_unit_size == 1) || (chr > 0xffff))
	{

#ifdef RX_LARGE_TABLES
	  {
	    struct rx_inx ** page2_21;
	    struct rx_inx * page3_21;
	    if (super->huge_char_transitions == rx_default_huge_char_transitions)
	      {
		struct rx_inx *** ht;
		ht = (struct rx_inx ***)rx_dfa_cache_malloc (rx_page1_size21 * sizeof (struct rx_inx **));
		if (!ht)
		  return -1;
		super->huge_char_transitions = ht;
		mem_move ((t_uchar *)super->huge_char_transitions, (t_uchar *)rx_default_huge_char_transitions, sizeof (rx_default_huge_char_transitions));
	      }
	    
	    page2_21 = rx_page2_21 (super->huge_char_transitions, chr);
	    
	    if (page2_21 == rx_huge_char_second_level_page)
	      {
		page2_21 = (struct rx_inx **)rx_dfa_cache_malloc (rx_page2_size21 * sizeof (struct rx_inx *));
		if (!page2_21)
		  return -1;
		mem_move ((t_uchar *)page2_21, (t_uchar *)rx_huge_char_second_level_page, rx_page2_size21 * sizeof (struct rx_inx *));
		rx_page2_21 (super->huge_char_transitions, chr) = page2_21;
	      }
	    
	    page3_21 = rx_page3_21 (super->huge_char_transitions, chr);
	    
	    if (page3_21 == rx_shared_cache_miss_page)
	      {
		page3_21 = (struct rx_inx *)rx_dfa_cache_malloc (256 * sizeof (struct rx_inx));
		if (!page3_21)
		  return -1;
		mem_move ((t_uchar *)page3_21, (t_uchar *)rx_shared_cache_miss_page, 256 * sizeof (struct rx_inx));
		rx_page3_21(super->huge_char_transitions, chr) = page3_21;
	      }
	  }
#else
	  {
	    struct rx_inx ***  page2_21;
	    struct rx_inx ** page3_21;
	    struct rx_inx * page4_21;

	    if (super->huge_char_transitions == rx_default_huge_char_transitions)
	      {
		struct rx_inx **** ht;
		ht = (struct rx_inx ****)rx_dfa_cache_malloc (rx_page1_size21 * sizeof (struct rx_inx ***));
		if (!ht)
		  return -1;
		super->huge_char_transitions = ht;
		mem_move ((t_uchar *)super->huge_char_transitions, (t_uchar *)rx_default_huge_char_transitions, sizeof (rx_default_huge_char_transitions));
	      }
	    
	    page2_21 = rx_page2_21 (super->huge_char_transitions, chr);
	    
	    if (page2_21 == rx_huge_char_second_level_page)
	      {
		page2_21 = (struct rx_inx ***)rx_dfa_cache_malloc (rx_page2_size21 * sizeof (struct rx_inx **));
		if (!page2_21)
		  return -1;
		mem_move ((t_uchar *)page2_21, (t_uchar *)rx_huge_char_second_level_page, rx_page2_size21 * sizeof (struct rx_inx **));
		rx_page2_21 (super->huge_char_transitions, chr) = page2_21;
	      }
	    
	    page3_21 = rx_page3_21 (super->huge_char_transitions, chr);
	    
	    if (page3_21 == rx_huge_char_third_level_page)
	      {
		page3_21 = (struct rx_inx **)rx_dfa_cache_malloc (rx_page3_size21 * sizeof (struct rx_inx *));
		if (!page3_21)
		  return -1;
		mem_move ((t_uchar *)page3_21, (t_uchar *)rx_huge_char_third_level_page, rx_page3_size21 * sizeof (struct rx_inx *));
		rx_page3_21(super->huge_char_transitions, chr) = page3_21;
	      }

	    page4_21 = rx_page4_21 (super->huge_char_transitions, chr);
	    if (page4_21 == rx_shared_cache_miss_page)
	      {
		page4_21 = (struct rx_inx *)rx_dfa_cache_malloc (rx_page4_size21 * sizeof (struct rx_inx));
		if (!page4_21)
		  return -1;
		mem_move ((t_uchar *)page4_21, (t_uchar *)rx_shared_cache_miss_page, rx_page4_size21 * sizeof (struct rx_inx));
		rx_page4_21 (super->huge_char_transitions, chr) = page4_21;
	      }
	  }
#endif
	  inxs = rx_subset_transition21 (super->huge_char_transitions, chr);
	}
      else
	{
	  /* (storage_unit_size == 2) && (chr > 0xffff)
	   */
#ifdef RX_LARGE_TABLES
	  {
	    struct rx_inx * page2_16;

	    page2_16 = rx_page2_16 (super->transitions, chr);
	    if (page2_16 == rx_shared_cache_miss_page)
	      {
		page2_16 = (struct rx_inx *)rx_dfa_cache_malloc (256 * sizeof (struct rx_inx));
		if (!page2_16)
		  return -1;
		mem_move ((t_uchar *)page2_16, (t_uchar *)rx_shared_cache_miss_page, 256 * sizeof (struct rx_inx));
		rx_page2_16(super->transitions, chr) = page2_16;
	      }

	    inxs = rx_subset_transitions16 (super->transitions, chr);
	  }
#else
	  
	  {
	    struct rx_inx ** page2_16;
	    struct rx_inx * page3_16;

	    page2_16 = rx_page2_16 (super->transitions, chr);
	    if (page2_16 == rx_default_small_table_page2)
	      {
		page2_16 = (struct rx_inx **)rx_dfa_cache_malloc (rx_page2_size * sizeof (struct rx_inx *));
		if (!page2_16)
		  return -1;
		mem_move ((t_uchar *)page2_16, (t_uchar *)rx_default_small_table_page2, rx_page2_size * sizeof (struct rx_inx *));
		rx_page2_16(super->transitions, chr) = page2_16;
	      }

	    page3_16 = rx_page3_16 (super->transitions, chr);
	    if (page3_16 == rx_shared_cache_miss_page)
	      {
		page3_16 = (struct rx_inx *)rx_dfa_cache_malloc (rx_page3_size * sizeof (struct rx_inx));
		if (!page3_16)
		  return -1;
		mem_move ((t_uchar *)page3_16, (t_uchar *)rx_shared_cache_miss_page, rx_page3_size * sizeof (struct rx_inx));
		rx_page3_16(super->transitions, chr) = page3_16;
	      }

	    inxs = rx_subset_transitions16 (super->transitions, chr);
	  }

#endif
	}
    }
  
  
  for (x = 0, pos = 1; x < bits_per_subset; ++x, pos <<= 1)
    {
      if (set & pos)
	{
	  inxs[x] = *answer;
	  inxs[x].next_same_edge = edge->inx_list;
	  edge->inx_list = &inxs[x];
	}
    }
  return 0;
}




/************************************************************************
 *(h1 "DFA State Transition Cache Misses")
 * 
 * 
 */


/*(c rx_handle_cache_miss)
 * struct rx_inx * rx_handle_cache_miss (struct rx_nfa * rx,
 *					 struct rx_superstate * super,
 *					 t_uchar chr,
 *					 void * data_2);
 * 
 * Recover from an `rx_cache_miss' instruction.
 *
 * `rx' and `super' are the NFA and superstate.  `chr' is the input
 * character for which the transition table of `super' contains
 * an `rx_cache_miss' instruction.
 *
 * `data_2' is the `data_2' field of the `rx_cache_miss' instruction
 * that caused us to call `rx_handle_cache_miss'.
 *
 * We return a pointer to another instruction (perhaps the corrected
 * transition table entry) which replaces the cache miss instruction.
 * The caller of this function can continue by re-dispatching on the
 * new instruction (which is permitted to be another cache miss
 * instruction).
 */
struct rx_inx *
rx_handle_cache_miss (struct rx_nfa * rx,
		      struct rx_superstate * super,
		      unsigned int chr,
		      void * data_2) 
{
  struct rx_super_edge * e;

  /* There are three kinds of cache miss.  
   * 
   * The first occurs when a transition is taken that has never been
   * computed during the lifetime of the source superstate.  In this
   * case, no DFA edge exists for this transition.  The cache miss is
   * handled by calling `compute_super_edge_cset' and building a new
   * DFA edge.  If `compute_super_edge_cset' tells us that no
   * transition is defined, we fill in the transition table with
   * `rx_backtrack' instructions and return one of those.  
   *
   * The second kind of cache miss occurs when we have a DFA edge
   * (pointed to by `data_2') but the destination superstate of that
   * edge isn't known.  `solve_destination' is used to construct the
   * destination superstate or find it in the DFA cache.  We return a
   * `rx_next_char' instruction.
   * 
   * Finally, the third kind of cache miss occurs when the destination
   * superstate of a transition is known but is or was in a `semi-free
   * state'.  That case is handled by `refresh_semifree_superstate'.
   * We return an `rx_next_char' instruction.
   *
   */



  /* If the `rx_cache_miss' instruction contained a pointer to an
   * existing DFA edge, that pointer was passed to us as `data_2'.
   */
  e = data_2;

 retry:

  if (!e)
    {
      /* A missing DFA edge.  Look for it in the cache.
       */
      for (e = super->outgoing_edges; e; e = e->next_same_present)
	if (bits_is_member (e->cset, chr))
	  goto retry;

      /* A DFA edge missing from the cache.  Try to build it now.
       */
      {
	bits trcset;

	rx_lock_superstate (rx, super);

	if (super->has_huge_table)
	  trcset = bits_alloc (rx__dfa_alloc_limits, uni_bits_tree_rule);
	else
	  trcset = bits_alloc (rx__dfa_alloc_limits, rx_8bit_bits_tree_rule);

	if (!trcset)
	  {
	    rx_unlock_superstate (rx, super);
	    return 0;
	  }

	{
	  int nfa_edges;

	  nfa_edges = compute_super_edge_cset (rx, trcset, super, chr);
	  if (nfa_edges < 0)
	    {
	      rx_unlock_superstate (rx, super);
	      return 0;
	    }
#if 0
	  else if (!nfa_edges)
	    {
	      static struct rx_inx backtrack = { 0, 0, (void *)rx_backtrack, 0 };
	      rx_unlock_superstate (rx, super);
	      bits_free (trcset);
	      return &backtrack;
	    }
#endif
	  else
	    {
	      e = rx_super_edge (rx, super, trcset);
	      rx_unlock_superstate (rx, super);
	      if (!e)
		return 0;
	      goto retry;
	    }
	}
      }
    }
  else if (e->future)
    {
      struct rx_inx inx;

      /* We know the superstate edge and its destination state, but
       * the instruction frame contained a stale `rx_cache_miss'
       * instruction (the instruction itself, not the state, was the
       * stale part of the cache).  We must now replace the
       * instruction with an `rx_next_char' instruction.
       *
       * Its possible that the destination state is in a semifree
       * state and must be refreshed.
       */
      refresh_semifree_superstate (e->future);
      rx_lock_superstate (rx, super);

      inx.data = (void *)&e->future->transitions;
      inx.data_2 = (void *)e->future->members->state_label;
      inx.inx = (void *)rx_next_char;

      {
	bitset_subset s;
	if (   bits_get_subset (&s, e->cset, chr)
	    || install_partial_transition (super, &inx, e, s, chr))
	  {
	    rx_unlock_superstate (rx, super);
	    return 0;
	  }
      }
      rx_unlock_superstate (rx, super);

      if (!super->has_huge_table || ((super->storage_unit_size == 1) && (chr < 0x80)))
	return rx_transition8 (&super->transitions, chr);
      else if ((super->storage_unit_size == 2) && (chr < (1 << 16)))
	return rx_transition16 (&super->transitions, chr);
      else
	return rx_transition21 (super->huge_char_transitions, chr);
    }
  else
    {
      struct rx_inx inx;

      /* A DFA edge with no known destination state.  We must now
       * compute the destination state and fill in transition table
       * entries with `rx_next_char' instructions.
       */
      rx_lock_superstate (rx, super);
      {
	bitset_subset s;
	struct rx_superstate * destination;

	if (solve_destination (rx, &inx, e, super->storage_unit_size))
	  {
	    rx_unlock_superstate (rx, super);
	    return 0;
	  }

	if (inx.inx == (void *)rx_next_char)
	  {
	    destination = rx_transitions_to_suprestate ((rx_transition_table)(inx.data));
	    rx_lock_superstate (rx, destination);
	  }
	else
	  destination = 0;

	if (bits_get_subset (&s, e->cset, chr)
	    || install_partial_transition (super, &inx, e, s, chr))
	  {
	    if (destination)
	      rx_unlock_superstate (rx, destination);
	    rx_unlock_superstate (rx, super);
	    return 0;
	  }
	if (destination)
	  rx_unlock_superstate (rx, destination);
      }
      rx_unlock_superstate (rx, super);
      if (!super->has_huge_table || ((super->storage_unit_size == 1) && (chr < 0x80)))
	return rx_transition8 (&super->transitions, chr);
      else if ((super->storage_unit_size == 2) && (chr < (1 << 16)))
	return rx_transition16 (&super->transitions, chr);
      else
	return rx_transition21 (super->huge_char_transitions, chr);
    }
}


/************************************************************************
 * DFA Cache Debugging Tools
 * 
 * I sometimes use this code to debug this file.
 */

#if 0
static int
qlen (struct rx_superstate * q)
{
  int count = 1;
  struct rx_superstate * it;
  if (!q)
    return 0;
  for (it = q->next_recyclable; it != q; it = it->next_recyclable)
    ++count;
  return count;
}


static void
check_cache ()
{
  struct rx_cache * cache = rx_default_cache;
  int total = cache->superstates;
  int semi = cache->semifree_superstates;


  total = cache->superstates;
  semi = cache->semifree_superstates;

  if (semi != qlen (cache->semifree_superstate))
    panic ("failed cache check in rx");

  if ((total - semi) != qlen (cache->lru_superstate))
    panic ("failed cache check in rx (2)");
}


#endif
