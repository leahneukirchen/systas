/* nfa.h - decls for manipulating regexps as NFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX__NFA_H
#define INCLUDE__RX__NFA_H

struct rx_nfa;
struct rx_nfa_state;
struct rx_superset;

#include "hackerlab/hash/hashtree.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/rx/bits-tree-rules.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/super.h"


/************************************************************************
 *(h1 "The NFA Data Types")
 * 
 * 
 * 
 */

/*(c #s"struct rx_nfa" :category type)
 * 
 * This structure type is used to hold a non-deterministic
 * finite automata.
 * 
 insert*/
struct rx_nfa
{
  unsigned long rx_id;
  /*	Each NFA has a unique ID assigned by `rx_nfa_xalloc'. */

  int local_cset_size;
  /* 	Either 256 (8-bit ascii) or 1<<21 (Unicode). */

  struct bits_tree_rule * bitset_rule;

  struct rx_nfa_state * nfa_states;
  /* 	The list of states in this NFA, linked by the field `next'.
    	This is filled in by `rx_nfa_state'. */

  int nfa_state_id;
  /*   	NFA states are assigned sequential ids.  This is the id of the
     	next state.*/

  struct rx_nfa_state * start_nfa_state;
  /* 	The starting state of the NFA.  This field must be filled in
 	by a call to `rx_set_start_state' once the start state has
 	been created. */

  struct hashtree set_list_memo;
  /*	A hash table used to allocate NFA state sets with the property
	that equal sets are `=='. */
};
/*end-insert
 */


/*(c #s"struct rx_nfa_state" :category type)
 * 
 * This structure type holds one state of an NFA.
 *
 insert*/
struct rx_nfa_state
{
  unsigned int id;
  /*    NFA states are assigned sequential ids.*/

  unsigned long state_label;
  /* 	Each state has a label.  If the label is non-0, the state is a
	final state.  In the DFA, a superstate is given a state label
	which is the smallest magnitude of the non-0 state labels of
	the NFA states it contains.  If all of the NFA states have a
	state label 0, then so does the DFA state. */

  struct rx_nfa_edge *edges;
  /*  	A list of NFA edges originating in this state.
	The edges are linked by the field `next'. */

  t_uchar is_start;
  /*	Set to `1' by `rx_set_start_state' and is otherwise 0.*/

  t_uchar has_cset_edges;
  /*	Set to `1' by `rx_nfa_edge' and is otherwise 0.  This is used
	during DFA matching to recognize dead-end states.*/

  t_uchar closure_computed;
  /*   	Set to `1' by `rx_state_closure' and is otherwise 0.*/

  struct rx_nfa_state_set * closure;
  /*	This is set to the state's epsilon closure by 
	`rx_state_closure' and is otherwise 0.*/

  struct rx_superset * superstate_set;
  /* 	This field is set by `rx_nfa_state_to_superstate' to cache
    	the superstate set of the eclosure of this NFA state.

	This field does not have a reference count to the set.
	Instead, if the set is freed, this field is set to 0.

	The superstate set points back to this state in the field
	`nfa_state'. */

  t_uchar mark;
  /*   	This is used by various graph traversal algorithms. */

  struct rx_nfa_state *next;
  /*	This link is to the next state of the same NFA in the list
 	of all states starting at `nfa->nfa_states'. */
};
/*end-insert
 */


/*(c #s"struct rx_nfa_edge" :other-terms "enum rx_nfa_etype" :category type)
 * enum rx_nfa_etype;	
 * struct rx_nfa_edge;
 *
 * This structure type holds one edge of an NFA.
 *
 insert*/

/* enum rx_nfa_etype;
 * 
 * There are two types of NFA edges: character sets and "epsilon".  An
 * epsilon edge represents a transition that can be taken immediately
 * whenever the source state is reached.  A character set transition
 * can be taken only by matching character in the set.
 */
enum rx_nfa_etype
{
  ne_cset,
  ne_epsilon
};

/* struct rx_nfa_edge;
 *
 * One NFA edge.
 */
struct rx_nfa_edge
{
  enum rx_nfa_etype type;	/* Which type of edge? */
  struct rx_nfa_state *dest;	/* What is the destination state? */

  bits cset;
  /*	If the edge is a character set edge (`ne_cset'), 
	this is the set of characters it matches. */

  struct rx_nfa_edge *next;
  /*	Next edge for the same source node. */
};
/*end-insert
 */

/*(c #s"struct rx_nfa_state_set" :category type)
 * 
 * This structure type holds a set of NFA states, represented as a
 * list.  It is used to represent the epsilon closure of an NFA node.
 * Any two NFA sets with the same elements are equal in the sense of
 * `==' if they were returned by `rx_state_closure'.
 * 
 insert*/
struct rx_nfa_state_set
{
  struct rx_nfa_state * car;
  struct rx_nfa_state_set * cdr;
};
/*end-insert
 */



/************************************************************************
 *(h1 "Shared NFA Data Structures")
 * 
 * 
 * We have a structure type which represents a cached NFA:
 */

/*(c #s"struct rx_unfa" :category type)
 * struct rx_unfa;
 * 
 * This structure holds a regexp expression tree and an NFA for that
 * expression.  There is at most one of these structures for each 
 * expression.
 *
 insert*/
struct rx_unfa
{
  int refs;			/* A reference count. */
  struct rx_exp_node * exp;	/* A queue of regexps with the same NFA. */
  struct rx_nfa * nfa;		/* The NFA. */
};
/*end-insert
 */

 
/* automatically generated __STDC__ prototypes */
extern int rx_build_nfa (struct rx_nfa *rx,
			 struct rx_exp_node *rexp,
			 struct rx_nfa_state **start,
			 struct rx_nfa_state **end);
extern void rx__nfa_cache_statistics (size_t * threshold,
				      size_t * failure_pt,
				      size_t * in_use,
				      size_t * high_water_mark,
				      int * hits,
				      int * misses,
				      int * saves);
extern struct rx_unfa * rx_unfa (struct rx_exp_node * exp,
				 int cset_size);
extern void rx_save_unfa (struct rx_unfa * unfa);
extern void rx_free_unfa (struct rx_unfa * unfa);
extern int rx__really_free_unfa (void);
extern struct rx_nfa * rx_nfa_xalloc (int cset_size);
extern void rx_free_nfa (struct rx_nfa * rx);
extern struct rx_nfa_state * rx_nfa_state (struct rx_nfa *rx);
extern struct rx_nfa_edge * rx_nfa_edge (struct rx_nfa *rx,
					 enum rx_nfa_etype type,
					 struct rx_nfa_state *start,
					 struct rx_nfa_state *dest);
extern struct rx_nfa_edge * rx_nfa_cset_edge (struct rx_nfa *rx,
					      enum rx_nfa_etype type,
					      bits cset,
					      struct rx_nfa_state *start,
					      struct rx_nfa_state *dest);
extern void rx_set_start_state (struct rx_nfa * rx, struct rx_nfa_state * n);
extern void rx_set_state_label (struct rx_nfa * rx,
				struct rx_nfa_state * n,
				int label);
extern struct rx_nfa_state_set * rx_state_closure (struct rx_nfa * rx,
						   struct rx_nfa_state * n);
#endif  /* INCLUDE__RX__NFA_H */
