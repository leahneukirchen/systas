/* nfa.c - functions for manipulating regexps as NFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/os/setjmp.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/uni/coding.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/nfa.h"


/* __STDC__ prototypes for static functions */
static void rx_build_nfa_internal (struct rx_nfa *rx,
				   struct rx_exp_node *rexp,
				   struct rx_nfa_state **start,
				   struct rx_nfa_state **end,
				   jmp_buf * err_escape);
static int unfa_equal (void * va, void * vb, struct hashtree_rules * rules);
static struct hashtree * unfa_hash_alloc (struct hashtree_rules * rules);
static void unfa_free_hash (struct hashtree * tab, struct hashtree_rules * rules);
static struct hashtree_item * unfa_hash_item_alloc (void * key, struct hashtree_rules * rules);
static void unfa_free_hash_item (struct hashtree_item * item, struct hashtree_rules * rules);
static struct rx_cached_rexp * canonical_unfa (struct hashtree * table, struct rx_exp_node * rexp, int cset_size);
static struct rx_nfa * rx_unfa_to_nfa (struct rx_cached_rexp * cr,
				       struct rx_exp_node * exp,
				       int cset_size);
static void rx_free_nfa_graph (struct rx_nfa *rx);
static void nfa_set_freer (struct hashtree_item * node,
			   struct hashtree_rules * rules);
static int nfa_set_cmp (void * va, void * vb);
static int nfa_set_equal (void * va, void * vb, struct hashtree_rules * rules);
static struct hashtree * nfa_set_hash_alloc (struct hashtree_rules * rules);
static void nfa_set_free_hash (struct hashtree * tab, struct hashtree_rules * rules);
static struct hashtree_item * nfa_set_hash_item_alloc (void * key, struct hashtree_rules * rules);
static void nfa_set_free_hash_item (struct hashtree_item * item, struct hashtree_rules * rules);
static struct rx_nfa_state_set * nfa_set_cons (struct rx_nfa * rx,
					       struct hashtree * memo,
					       struct rx_nfa_state * state,
					       struct rx_nfa_state_set * set);
static struct rx_nfa_state_set * nfa_set_adjoin (struct rx_nfa * rx,
						 struct hashtree * memo,
						 struct rx_nfa_state * state,
						 struct rx_nfa_state_set * set);
static struct rx_nfa_state_set * nfa_set_union (struct rx_nfa * rx,
						struct hashtree * memo,
						struct rx_nfa_state_set * a,
						struct rx_nfa_state_set * b);
static int eclose_node (struct rx_nfa * rx,
			struct rx_nfa_state * outnode,
			struct rx_nfa_state * node);


static struct hashtree_rules nfa_set_hash_rules;
static int n_edges_allocated = 0;

/************************************************************************
 *(h0 "Non-deterministic Finite-state Automata"
 *    :includes ("rx/nfa.h"))
 * 
 * 
 * A regexp which is a regular expression has a natural representation
 * as a non-deterministic finite-state automata which in turn has a
 * natural representation as a graph of states and edges.
 * 
 * To compare a regular expression to a string, Rx builds an NFA from
 * the expression, and then a DFA (deterministic automata) from the NFA.
 * 
 * The functions and types in this chapter are concerned with representing
 * and building non-deterministic finite-state automata.
 * 
 * Equal regexps (in the sense of `rx_exp_equal') must always yield
 * equivalent non-deterministic finite-state automata.  We can take
 * advantage of this to save space and time by reusing a single NFA
 * whenever two regexps are equal.
 *
 * The savings is large: not only do we avoid allocating a second NFA,
 * but more importantly, we will build only a single DFA for the two
 * regexps.  Cache space for DFA is precious and it is expensive to
 * build a DFA.
 *
 * Procedures are provided which maintain a cached mapping from
 * regexps to NFA.  Using this interface, equal regexps
 * (`rx_exp_equal') will tend to yield NFA which are equal (`==').
 */
/*(menu)
 */

/*(include-documentation "nfa.h")
 */

/************************************************************************
 *(h1 "Translating an Expression Tree to an NFA")
 * 
 * Given a *regular expression* (not a *true regexp*) syntax tree (see
 * xref:"Regexp Expression Trees"), this function builds an NFA for
 * the expression.
 */

/*(c rx_build_nfa)
 * void rx_build_nfa (struct rx_nfa *rx,
 *                    struct rx_exp_node *rexp,
 *                    struct rx_nfa_state **start,
 *                    struct rx_nfa_state **end);
 * 
 * Build an NFA for regular expression `rexp'.
 * 
 * States and edges of the NFA are added to `rx'.
 * 
 * `*start' and `*end' are filled in with the start
 * and end states of the automata.  They may be initialized
 * to 0, or to pre-existing NFA states.
 *
 * The start state returned by this function does not automatically
 * become the start state of the overall NFA and the end state
 * returned by this function does not automatically have a non-0 state
 * label (unless the expression ends with a "cut" operator).  See
 * xref:"The Last Step of Building an NFA".
 */

int
rx_build_nfa (struct rx_nfa *rx,
	      struct rx_exp_node *rexp,
	      struct rx_nfa_state **start,
	      struct rx_nfa_state **end)
{
  jmp_buf err_escape;

  if (setjmp (err_escape))
    {
      return -1;
    }
  rx_build_nfa_internal (rx, rexp, start, end, &err_escape);  
  return 0;
}

static void
rx_build_nfa_internal (struct rx_nfa *rx,
		       struct rx_exp_node *rexp,
		       struct rx_nfa_state **start,
		       struct rx_nfa_state **end,
		       jmp_buf * err_escape)
{
  struct rx_nfa_edge *edge;

  if (!*start)
    {
      *start = rx_nfa_state (rx);
      if (!*start)
	longjmp (*err_escape, 1);
    }

  if (!*end)
    {
      *end = rx_nfa_state (rx);
      if (!*end)
	longjmp (*err_escape, 1);
    }

  if (!rexp)
    {
      if (!rx_nfa_edge (rx, ne_epsilon, *start, *end))
	longjmp (*err_escape, 1);
      return;
    }

  switch (rexp->type)
    {
    case r_cset:
      {
	bits cset;
	cset = bits_dup (rexp->cset);
	if (!cset)
	  longjmp (*err_escape, 1);
	edge = rx_nfa_cset_edge (rx, ne_cset, cset, *start, *end);
	if (!edge)
	  longjmp (*err_escape, 1);
	return;
      }

    case r_string:
      {
	t_uchar * str;
	size_t pos;
	size_t len;
	struct rx_nfa_state * from;
	bits cset;
	uni_iscan_fn scan;


	str = rexp->str;
	len = rexp->str_len;
	scan = uni_encoding_iscan_fn (rexp->encoding);
	pos = 0;
	from = *start;

	while (pos < len)
	  {
	    t_unicode c;
	    struct rx_nfa_state * to;

	    c = scan (str, &pos, len);
	    invariant ((c != 0xffff) && (c != 0xfffe));

	    if (pos == len)
	      to = *end;
	    else
	      {
		to = rx_nfa_state (rx);
		if (!to)
		  longjmp (*err_escape, 1);
	      }

	    cset = bits_alloc (rx_nfa_cache_limits (), rx->bitset_rule);
	    if (!cset)
	      longjmp (*err_escape, 1);
	    if (bits_adjoin (cset, c))
	      {
		bits_free (cset);
		longjmp (*err_escape, 1);
	      }
	    edge = rx_nfa_cset_edge (rx, ne_cset, cset, from, to);
	    if (!edge)
	      longjmp (*err_escape, 1);
	    from = to;
	  }
	return;
      }
 
    case r_interval:
    case r_star:
      {
	struct rx_nfa_state * star_start;
	struct rx_nfa_state * star_end;

	star_start = 0;
	star_end = 0;
	rx_build_nfa_internal (rx, rexp->left, &star_start, &star_end, err_escape);
	if (   !rx_nfa_edge (rx, ne_epsilon, star_start, star_end)
	    || !rx_nfa_edge (rx, ne_epsilon, *start, star_start)
	    || !rx_nfa_edge (rx, ne_epsilon, star_end, *end)
	    || !rx_nfa_edge (rx, ne_epsilon, star_end, star_start))
	  longjmp (*err_escape, 1);
	return;
      }

    case r_cut:
      {
	struct rx_nfa_state * cut_end;

	cut_end = rx_nfa_state (rx);
	if (!cut_end)
	  longjmp (*err_escape, 1);
	if (!rx_nfa_edge (rx, ne_epsilon, *start, cut_end))
	  longjmp (*err_escape, 1);
	cut_end->state_label = rexp->intval;
	return;
      }

    case r_parens:
      rx_build_nfa_internal (rx, rexp->left, start, end, err_escape);
      return;

    case r_right_concat:
    case r_concat:
      {
	struct rx_nfa_state *shared;

	shared = 0;
	rx_build_nfa_internal (rx, rexp->left, start, &shared, err_escape);
	rx_build_nfa_internal (rx, rexp->right, &shared, end, err_escape);
	return;
      }

    case r_alternate:
      {
	struct rx_nfa_state *ls = 0;
	struct rx_nfa_state *le = 0;
	struct rx_nfa_state *rs = 0;
	struct rx_nfa_state *re = 0;

	ls = 0;
	le = 0;
	rs = 0;
	re = 0;

	rx_build_nfa_internal (rx, rexp->left, &ls, &le, err_escape);
	rx_build_nfa_internal (rx, rexp->right, &rs, &re, err_escape);
	if (   !rx_nfa_edge (rx, ne_epsilon, *start, ls)
	    || !rx_nfa_edge (rx, ne_epsilon, *start, rs)
	    || !rx_nfa_edge (rx, ne_epsilon, le, *end)
	    || !rx_nfa_edge (rx, ne_epsilon, re, *end))
	  longjmp (*err_escape, 1);
	return;
      }

    case r_context:
      if (!rx_nfa_edge (rx, ne_epsilon, *start, *end))
	longjmp (*err_escape, 1);
      return;

    default:
      panic ("unreconized node type in rx_build_nfa_internal");
    }
}


/************************************************************************
 *(h1 "Shared NFA Functions")
 * 
 */

struct rx_cached_rexp
{
  struct rx_unfa unfa;
  struct rx_cached_rexp * next;
  struct rx_cached_rexp * prev;
  struct hashtree_item * hash_item;
};

static int delayed = 0;
static struct hashtree unfa_table = { 0 };
static struct rx_cached_rexp * free_queue = 0;
static int unfa_hits = 0;
static int unfa_misses = 0;
static int unfa_saves = 0;


void
rx__nfa_cache_statistics (size_t * threshold,
			  size_t * failure_pt,
			  size_t * in_use,
			  size_t * high_water_mark,
			  int * hits,
			  int * misses,
			  int * saves)
{
  if (threshold)
    *threshold = rx_nfa_cache_threshold ();
  if (failure_pt)
    *failure_pt = rx_nfa_cache_failure_pt ();
  if (high_water_mark)
    *high_water_mark = rx_nfa_cache_high_water_mark ();
  if (in_use)
    *in_use = rx_nfa_cache_in_use ();
  if (hits)
    *hits = unfa_hits;
  if (misses)
    *misses = unfa_misses;
  if (saves)
    *saves = unfa_saves;
}




static int
unfa_equal (void * va, void * vb, struct hashtree_rules * rules)
{
  return rx_exp_equal ((struct rx_exp_node *)va, (struct rx_exp_node *)vb);
}


static struct hashtree *
unfa_hash_alloc (struct hashtree_rules * rules)
{
  return (struct hashtree *)rx_nfa_cache_soft_malloc (sizeof (struct hashtree));
}


static void
unfa_free_hash (struct hashtree * tab, struct hashtree_rules * rules)
{
  rx_nfa_cache_free ((char *)tab);
}


static struct hashtree_item *
unfa_hash_item_alloc (void * key, struct hashtree_rules * rules)
{
  struct hashtree_item * it;
  it = (struct hashtree_item *)rx_nfa_cache_soft_malloc (sizeof (*it));
  if (!it)
    return 0;
  it->key = key;
  it->binding = 0;
  return it;
}


static void
unfa_free_hash_item (struct hashtree_item * item, struct hashtree_rules * rules)
{
  rx_nfa_cache_free ((char *)item);
}


static struct hashtree_rules unfa_rules =
{
  unfa_equal,
  unfa_hash_alloc,
  unfa_free_hash,
  unfa_hash_item_alloc,
  unfa_free_hash_item
};

static struct rx_cached_rexp *
canonical_unfa (struct hashtree * table, struct rx_exp_node * rexp, int cset_size)
{
  struct hashtree_item * it;

  it = hashtree_store (table, rx_exp_hash (rexp), rexp, &unfa_rules);
  if (!it)
    return 0;

  if (it->binding == 0)
    {
      struct rx_cached_rexp * cr;

      cr = (struct rx_cached_rexp *)rx_nfa_cache_malloc (sizeof (*cr));
      if (!cr)
	{
	  hashtree_delete (it, &unfa_rules);
	  return 0;
	}
      if (it->key == (void *)rexp)
	rx_save_exp (rexp);
      mem_set0 ((char *)cr, sizeof (*cr));
      it->binding = (void *)cr;
      cr->unfa.nfa = 0;
      cr->unfa.exp = rexp;
      cr->hash_item = it;
      if (rexp)
	{
	  rexp->next_same_nfa = rexp;
	  rexp->prev_same_nfa = rexp;
	}
      rx_save_exp (rexp);
    }
  
  return (struct rx_cached_rexp *)it->binding;
}

static struct rx_nfa *
rx_unfa_to_nfa (struct rx_cached_rexp * cr,
		struct rx_exp_node * exp,
		int cset_size)
{
  struct rx_nfa * new_rx;

  if (cr->unfa.nfa)
    return cr->unfa.nfa;

  new_rx = rx_nfa_xalloc (cset_size);
  if (!new_rx)
    return 0;

  {
    struct rx_nfa_state * start;
    struct rx_nfa_state * end;
    start = end = 0;
    if (rx_build_nfa (new_rx, exp, &start, &end))
      {
	rx_free_nfa (new_rx);
	return 0;
      }
    end->state_label = 1;
    rx_set_start_state (new_rx, start);
  }
  cr->unfa.nfa = new_rx;
  return new_rx;
}



/*(c rx_unfa)
 * struct rx_unfa * rx_unfa (struct rx_exp_node * exp,
 *			     int cset_size);
 * 
 * Construct a unique NFA for regexp `exp'.  For equal
 * (`rx_exp_equal') arguments, an equal (`==') value is usually, but
 * not always returned.  For equal (`==') arguments, an equal (`==')
 * value is always returned.
 */
struct rx_unfa *
rx_unfa (struct rx_exp_node * exp,
	 int cset_size)
{
  struct rx_cached_rexp * cr;

  if (exp && exp->cr)
    {
      ++unfa_hits;
      cr = exp->cr;
    }
  else
    {
      cr = canonical_unfa (&unfa_table, exp, cset_size);
      if (!cr)
	return 0;
      if (exp)
	{
	  exp->cr = cr;
	  exp->next_same_nfa = cr->unfa.exp;
	  exp->prev_same_nfa = cr->unfa.exp->prev_same_nfa;
	  exp->next_same_nfa->prev_same_nfa = exp;
	  exp->prev_same_nfa->next_same_nfa = exp;
	}
      if (cr->unfa.nfa)
	++unfa_hits;
      else
	++unfa_misses;
    }

  if (cr->next)
    {
      if (free_queue == cr)
	{
	  free_queue = cr->next;
	  if (free_queue == cr)
	    free_queue = 0;
	}
      cr->next->prev = cr->prev;
      cr->prev->next = cr->next;
      cr->next = 0;
      cr->prev = 0;
      --delayed;
      ++unfa_saves;
    }
  ++cr->unfa.refs;
  if (!rx_unfa_to_nfa (cr, exp, cset_size))
    {
      rx_free_unfa (&cr->unfa);
      return 0;
    }
  return &cr->unfa;
}


/*(c rx_save_unfa)
 * void rx_save_unfa (struct rx_unfa * unfa);
 * 
 * Increment the reference count for a unique NFA.
 */
void
rx_save_unfa (struct rx_unfa * unfa)
{
  ++(unfa->refs);
}



/*(c rx_free_unfa)
 * void rx_free_unfa (struct rx_unfa * unfa);
 * 
 * Decrement the reference count for a unique NFA.  If the
 * reference count drops to 0, the NFA may be destroyed.
 *
 * In fact, a queue is kept of unique NFA with 0 references.  The NFA
 * on that queue can be spared death by a call to `rx_unfa'.  If the
 * queue size exceeds a fixed limit, the oldest NFA are finally
 * discarded from the queue.  See also the next function:
 * `rx_set_unfa_release_delay'.
 */
void
rx_free_unfa (struct rx_unfa * unfa)
{
  struct rx_cached_rexp * cr;

  cr = (struct rx_cached_rexp *)unfa;
  if (!cr)
    return;

  if (!--cr->unfa.refs)
    {
      if (!free_queue)
	{
	  free_queue = cr;
	  cr->next = cr->prev = cr;
	}
      else
	{
	  cr->next = free_queue;
	  cr->prev = free_queue->prev;
	  cr->next->prev = cr;
	  cr->prev->next = cr;
	}

      ++delayed;
    }
  else
    return;
}


int
rx__really_free_unfa (void)
{
  if (!delayed)
    return -1;
  else
    {
      struct rx_cached_rexp * it;

      it = free_queue;
      if (it->unfa.refs)
	panic ("freeing unfa with non-0 refs");
      free_queue = it->next;
      if (!--delayed)
	free_queue = 0;
      it->prev->next = it->next;
      it->next->prev = it->prev;
      if (it->unfa.exp)
	{
	  struct rx_exp_node * same_nfa;

	  same_nfa = it->unfa.exp;
	  same_nfa->prev_same_nfa->next_same_nfa = 0;
	  do
	    {
	      struct rx_exp_node * next;

	      next = same_nfa->next_same_nfa;
	      same_nfa->next_same_nfa = 0;
	      same_nfa->prev_same_nfa = 0;
	      same_nfa->cr = 0;
	      same_nfa = next;
	    }
	  while (same_nfa);
	}
      rx_free_exp ((struct rx_exp_node *)it->hash_item->key);
      hashtree_delete (it->hash_item, &unfa_rules);
      rx_free_nfa (it->unfa.nfa);
      rx_free_exp (it->unfa.exp);
      rx_nfa_cache_free (it);
      return 0;
    }
}



/************************************************************************
 *(h1 "Low-level NFA Functions")
 * 
 * These are the low-level functions for building NFA.
 */
/*(menu)
 */

/************************************************************************
 *(h2 "Allocating and Freeing an NFA")
 * 
 */

static unsigned long rx_id = 0;

/*(c rx_nfa_xalloc)
 * struct rx_nfa * rx_nfa_xalloc (int cset_size);
 * 
 * Allocate and return a new non-deterministic automata.
 * Initially, the automata has no states or edges.
 *
 * This function panics if allocation fails or
 * if more than `2**(8*sizeof(unsigned long))' 
 * automata have been allocated.
 *
 * The next step in creating an automata is to add
 * states and edges.  See xref:"rx_build_nfa" and
 * xref:"Building an NFA State by State".
 */
struct rx_nfa *
rx_nfa_xalloc (int cset_size)
{
  struct rx_nfa * new_rx;

  if ((cset_size != 256) && (cset_size != (1 << 21)))
    panic ("unhandled cset_size in rx_nfa_xalloc");
  new_rx = (struct rx_nfa *)rx_nfa_cache_malloc (sizeof (*new_rx));
  if (!new_rx)
    return 0;
  mem_set0 ((char *)new_rx, sizeof (*new_rx));
  new_rx->rx_id = rx_id++;
  if (rx_id == 0)
    panic ("rx id wraparound!");
  new_rx->local_cset_size = cset_size;
  switch (cset_size)
    {
    case 256:
      new_rx->bitset_rule = rx_8bit_bits_tree_rule;
      break;
    case (1 << 21):
      new_rx->bitset_rule = uni_bits_tree_rule;
      break;
    default:
      panic ("unrecognized cset size in rx_nfa_xalloc");
      break;
    }
  return new_rx;
}


static void
rx_free_nfa_graph (struct rx_nfa *rx)
{
  while (rx->nfa_states)
    {
      while (rx->nfa_states->edges)
	{
	  if (rx->nfa_states->edges->cset)
	    bits_free (rx->nfa_states->edges->cset);
	  {
	    struct rx_nfa_edge * e;
	    e = rx->nfa_states->edges;
	    rx->nfa_states->edges = rx->nfa_states->edges->next;
	    rx_nfa_cache_free (e);
	  }
	  --n_edges_allocated;
	}
      {
	struct rx_nfa_state *n;
	n = rx->nfa_states;
	if (n->superstate_set)
	  n->superstate_set->nfa_state = 0;
	rx->nfa_states = rx->nfa_states->next;
	rx_nfa_cache_free (n);
      }
    }
}


static void 
nfa_set_freer (struct hashtree_item * node,
	       struct hashtree_rules * rules)
{
  rx_nfa_cache_free ((char *)node->key);
}


/*(c rx_free_nfa)
 * void rx_free_nfa (struct rx_nfa * rx);
 * 
 * Release all storage associated with the NFA `rx'.
 */
void
rx_free_nfa (struct rx_nfa * rx)
{
  hashtree_free_static (&rx->set_list_memo, nfa_set_freer, &nfa_set_hash_rules);
  mem_set0 ((char *)&rx->set_list_memo, sizeof (rx->set_list_memo));
  rx_free_nfa_graph (rx);
  rx_nfa_cache_free (rx);
}


/************************************************************************
 *(h2 "Building an NFA State by State")
 * 
 * It is possible to construct an NFA without building an 
 * expression tree first.  These functions construct states
 * and edges.
 */


/*(c rx_nfa_state)
 * struct rx_nfa_state * rx_nfa_state (struct rx_nfa *rx);
 *
 * Allocate a new NFA state for the NFA `rx'.
 *
 * This function calls `panic' and does not return 
 * if an allocation failure occurs.
 */
struct rx_nfa_state *
rx_nfa_state (struct rx_nfa *rx)
{
  struct rx_nfa_state * n;

  n = (struct rx_nfa_state *)rx_nfa_cache_malloc (sizeof (*n));
  if (!n)
    return 0;
  mem_set0 ((char *)n, sizeof (*n));
  n->id = rx->nfa_state_id++;
  n->next = rx->nfa_states;
  rx->nfa_states = n;
  return n;
}


/*(c rx_nfa_edge)
 * struct rx_nfa_edge * rx_nfa_edge (struct rx_nfa *rx,
 *                                   enum rx_nfa_etype type,
 *                                   struct rx_nfa_state *start,
 *                                   struct rx_nfa_state *dest);
 * 
 * 
 * Allocate a new NFA edge for the NFA `rx'.
 *
 * `type' indicates which kind of edge is being built.  If the type is
 * `ne_cset', then the `cset' field of the new edge should be set to a
 * privately allocated character set. See xref:"struct rx_nfa_edge", but
 * also see the next function, `rx_nfa_cset_edge'
 *
 * `start' and `dest' describe the source and destination states
 * of the edge.
 *
 * This function calls `panic' and does not return if an allocation
 * failure occurs.
 */
struct rx_nfa_edge * 
rx_nfa_edge (struct rx_nfa *rx,
	     enum rx_nfa_etype type,
	     struct rx_nfa_state *start,
	     struct rx_nfa_state *dest)
{
  struct rx_nfa_edge *e;
  e = (struct rx_nfa_edge *)rx_nfa_cache_malloc (sizeof (*e));
  if (!e)
    return 0;
  mem_set0 ((t_uchar *)e, sizeof (*e));
  e->type = type;
  e->cset = 0;
  e->dest = dest;
  e->next = start->edges;
  start->edges = e;
  if (type == ne_cset)
    start->has_cset_edges = 1;
  ++n_edges_allocated;
  return e;
}

/* THESE TWO FUNCTIONS MUST BE ADJACENT FOR THE MANUAL TO READ 
 * CORRECTLY. (rx_nfa_edge, rx_nfa_cset_edge)
 */

/*(c rx_nfa_cset_edge)
 * struct rx_nfa_edge * rx_nfa_cset_edge (struct rx_nfa *rx,
 *                                        enum rx_nfa_etype type,
 *				          bits cset,
 *                                        struct rx_nfa_state *start,
 *                                        struct rx_nfa_state *dest);
 * 
 * 
 * Allocate a new NFA edge for the NFA `rx'.  The new edge will
 * have a character set label.
 *
 * `type' indicates which kind of edge is being built.  Ordinarilly
 * this should be `ne_cset'.
 *
 * `cset' is used to initialize the field `cset' field of the edge.
 * `cset' is not copied and will be freed by `rx_nfa_cache_free' when this
 * edge is freed.
 *
 * `start' and `dest' describe the source and destination states
 * of the edge.
 *
 * This function returns 0 if an allocation failure occurs.  In that
 * case, `cset' is not freed.
 */
struct rx_nfa_edge * 
rx_nfa_cset_edge (struct rx_nfa *rx,
		  enum rx_nfa_etype type,
		  bits cset,
		  struct rx_nfa_state *start,
		  struct rx_nfa_state *dest)
{
  struct rx_nfa_edge *e;
  e = rx_nfa_edge (rx, type, start, dest);
  if (!e)
    return 0;
  e->cset = cset;
  return e;
}


/************************************************************************
 *(h2 "The Last Step of Building an NFA")
 * 
 * 
 * After building an NFA graph, the last step is to distinguish the
 * unique start state of the NFA and label all of the final states
 * with non-0 labels.
 */

/*(c rx_set_start_state)
 * void rx_set_start_state (struct rx_nfa * rx, struct rx_nfa_state * n);
 * 
 * Set the start state of the NFA `rx'.
 */
void
rx_set_start_state (struct rx_nfa * rx, struct rx_nfa_state * n)
{
  rx->start_nfa_state = n;
  n->is_start = 1;
}


/*(c rx_set_state_label)
 * void rx_set_state_label (struct rx_nfa * rx, 
 *			    struct rx_nfa_state * n,
 *			    int label)
 * 
 * Set the label of state `n' to `label'.
 */
void
rx_set_state_label (struct rx_nfa * rx,
		    struct rx_nfa_state * n,
		    int label)
{
  n->state_label = label;
}





/************************************************************************
 * Managing Sets of NFA States
 * 
 */

/* static int nfa_set_cmp (void * va, void * vb);
 * 
 * Compare two NFA state sets.  The ordering of state sets
 * is well defined but unspecified.  Return -1 if `va' is 
 * less than `vb', 1 if it is greater, and 0 if they are equal.
 */
static int 
nfa_set_cmp (void * va, void * vb)
{
  struct rx_nfa_state_set * a;
  struct rx_nfa_state_set * b;

  a = (struct rx_nfa_state_set *)va;
  b = (struct rx_nfa_state_set *)vb;

  return ((va == vb)
	  ? 0
	  : (!va
	     ? -1
	     : (!vb
		? 1
		: (a->car->id < b->car->id
		   ? -1
		   : (a->car->id > b->car->id
		      ? 1
		      : nfa_set_cmp ((void *)a->cdr, (void *)b->cdr))))));
}


/* static int nfa_set_equal (void * va, void * vb, struct hashtree_rules * rules);
 * 
 * Return 1 if `va' and `vb' are equal state sets.
 * We know that if they are equal, their `cdr's must be equal
 * in the sense of `==', but their cars need not be.
 */
static int 
nfa_set_equal (void * va, void * vb, struct hashtree_rules * rules)
{
  struct rx_nfa_state_set * a;
  struct rx_nfa_state_set * b;
  a = (struct rx_nfa_state_set *)va;
  b = (struct rx_nfa_state_set *)vb;
  return (   (a == b)
	  || (   a
	      && b
	      && (a->car->id == b->car->id)
	      && (a->cdr == b->cdr)));
}

static struct hashtree *
nfa_set_hash_alloc (struct hashtree_rules * rules)
{
  return (struct hashtree *)rx_nfa_cache_soft_malloc (sizeof (struct hashtree));
}


static void
nfa_set_free_hash (struct hashtree * tab, struct hashtree_rules * rules)
{
  rx_nfa_cache_free ((char *)tab);
}


static struct hashtree_item *
nfa_set_hash_item_alloc (void * key, struct hashtree_rules * rules)
{
  struct hashtree_item * it;
  it = (struct hashtree_item *)rx_nfa_cache_soft_malloc (sizeof (*it));
  if (!it)
    return 0;
  it->key = key;
  it->binding = 0;
  return it;
}


static void
nfa_set_free_hash_item (struct hashtree_item * item, struct hashtree_rules * rules)
{
  rx_nfa_cache_free ((char *)item);
}


static struct hashtree_rules nfa_set_hash_rules =
{
  nfa_set_equal,
  nfa_set_hash_alloc,
  nfa_set_free_hash,
  nfa_set_hash_item_alloc,
  nfa_set_free_hash_item
};


/* static struct rx_nfa_state_set * nfa_set_cons (struct rx_nfa * rx,
 *						  struct hashtree * memo,
 *						  struct rx_nfa_state * state,
 *						  struct rx_nfa_state_set * set);
 * 
 * Construct a new state set by adding a new state to the head of the list.
 * For equal `set' and `state', return equal (`==') results.
 */
static struct rx_nfa_state_set * 
nfa_set_cons (struct rx_nfa * rx,
	      struct hashtree * memo,
	      struct rx_nfa_state * state,
	      struct rx_nfa_state_set * set)
{
  struct rx_nfa_state_set template;
  struct hashtree_item * node;
  unsigned long hash_value;

  template.car = state;
  template.cdr = set;
  hash_value = (  (unsigned long)set
		^ (((unsigned long)state) << 19)
		^ (((unsigned long)state) >> (8 * sizeof (unsigned long) - 19)));

  node = hashtree_store (memo,
			hash_value, 
			&template,
			&nfa_set_hash_rules);
  
  if (!node)
    return 0;

  if (node->key == &template)
    {
      struct rx_nfa_state_set * l;

      l = (struct rx_nfa_state_set *) rx_nfa_cache_malloc (sizeof (*l));
      if (!l)
	{
	  node->key = 0;
	  hashtree_delete (node, &nfa_set_hash_rules);
	  return 0;
	}
      *l = template;
      node->key = (void *) l;
    }

  return (struct rx_nfa_state_set *)node->key;
}


/* static struct rx_nfa_state_set * nfa_set_adjoin (struct rx_nfa * rx,
 *                                                  struct hashtree * memo,
 *                                                  struct rx_nfa_state * state,
 *                                                  struct rx_nfa_state_set * set);
 * 
 * Construct a state set by adding a state to the appropriate position
 * in the (sorted) list.
 *
 * For equal `set' and `state', return equal (`==') results.
 */
static struct rx_nfa_state_set * 
nfa_set_adjoin (struct rx_nfa * rx,
		struct hashtree * memo,
		struct rx_nfa_state * state,
		struct rx_nfa_state_set * set)
{
  if (!set || (state->id < set->car->id))
    return nfa_set_cons (rx, memo, state, set);
  if (state->id == set->car->id)
    return set;
  else
    {
      struct rx_nfa_state_set * newcdr;
      newcdr = nfa_set_adjoin (rx, memo, state, set->cdr);
      if (!newcdr)
	return 0;
      if (newcdr != set->cdr)
	set = nfa_set_cons (rx, memo, set->car, newcdr);
      return set;
    }
}


/* static struct rx_nfa_state_set * nfa_set_union (struct rx_nfa * rx,
 *						   struct hashtree * memo,
 *						   struct rx_nfa_state_set * a,
 *						   struct rx_nfa_state_set * b);
 * 
 * Construct a state set containing the members of `a' and `b'.
 *
 * For equal sets of members, return equal (`==') results.
 */
static struct rx_nfa_state_set * 
nfa_set_union (struct rx_nfa * rx,
	       struct hashtree * memo,
	       struct rx_nfa_state_set * a,
	       struct rx_nfa_state_set * b)
{
  if (!a)
    return b;
  if (!b)
    return a;

  if (a->car->id < b->car->id)
    {
      struct rx_nfa_state_set * u;
      u = nfa_set_union (rx, memo, a->cdr, b);
      if (!u)
	return 0;
      return nfa_set_cons (rx, memo, a->car, u);
    }
  else if (b->car->id < a->car->id)
    {
      struct rx_nfa_state_set * u;
      u = nfa_set_union (rx, memo, b->cdr, a);
      if (!u)
	return 0;
      return nfa_set_cons (rx, memo, b->car, u);
    }
  else
    return nfa_set_union (rx, memo, a, b->cdr);
}


/************************************************************************
 *(h2 "Computing Epsilon Closures")
 * 
 * Given an NFA and a state `S' from that NFA, there is a set of NFA
 * states which is the set of all states that can be reached from `S'
 * by following 0 or more epsilon edges.  This set plays an important
 * role in the construction of a deterministic automata from a
 * non-deterministic automata.  The function in this section computes
 * that set.
 * 
 */


/* static void eclose_node (struct rx_nfa * rx,
 *			    struct rx_nfa_state * outnode,
 *			    struct rx_nfa_state * node);
 * 
 * Compute the epsilon closure of one node of the `nfa' graph.
 * Do this by traversing epsilon edges while avoiding cycles.
 */
static int
eclose_node (struct rx_nfa * rx,
	     struct rx_nfa_state * outnode,
	     struct rx_nfa_state * node)
{
  struct rx_nfa_edge *e;

  if (node->closure_computed)
    {
      outnode->closure = nfa_set_union (rx, &rx->set_list_memo, outnode->closure, node->closure);
      if (!outnode->closure)
	return -1;
    }
  
  if (node->mark)
    return 0;

  node->mark = 1;

  outnode->closure = nfa_set_adjoin (rx,
				     &rx->set_list_memo,
				     node,
				     outnode->closure);
  if (!outnode->closure)
    {
      node->mark = 0;
      return -1;
    }

  e = node->edges;
  while (e)
    {
      if (e->type == ne_epsilon)
	{
	  if (eclose_node (rx, outnode, e->dest))
	    {
	      node->mark = 0;
	      return -1;
	    }
	}
      e = e->next;
    }

  node->mark = 0;
  return 0;
}


/*(c rx_state_closure)
 * struct rx_nfa_state_set * rx_state_closure (struct rx_nfa * rx,
 *					       struct rx_nfa_state * n);
 * 
 * Return the state set which is the epsilon-edge closure of NFA state
 * `n' in NFA `rx'.  This function records its result so that
 * subsequent calls with the same arguments run faster.  The first
 * call for particular arguments can be slow, depending on the size
 * and structure of the NFA graph.
 *
 * If an allocation failure occurs, this function panics and exits.
 */
struct rx_nfa_state_set *
rx_state_closure (struct rx_nfa * rx,
		  struct rx_nfa_state * n)
{
  if (!n->closure_computed)
    {
      if (eclose_node (rx, n, n))
	return 0;
      n->closure_computed = 1;
      return n->closure;
    }
  return n->closure;
}

