/* super.h - decls for lazilly constructed DFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#ifndef INCLUDE__RX__SUPER_H
#define INCLUDE__RX__SUPER_H

/* #define RX_LARGE_TABLES */

struct rx_superset;
struct rx_super_edge;
struct rx_superstate;

#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/bitsets/bitset.h"
#include "hackerlab/hash/hashtree.h"
#include "hackerlab/rx/nfa.h"



/************************************************************************
 *(h1 "Superstate Data Structures")
 * 
 * 
 * One state of the DFA is called a "superstate" because it
 * represents the composition of one or more NFA states.  The
 * functions in this chapter build superstates and edges between
 * them.
 *
 * A DFA can be prohibitively large -- too large to keep in memory all
 * at once.  So these functions help you to build only the superstates
 * you actually need.  While you keep those states locked, they remain
 * in memory;  when unlocked, they can be discarded and recycled.
 *
 * As you fill the superstate cache with DFA states you visit often,
 * you'll want to follow transitions between those DFA states as
 * quickly as possible.  For that reason, the data structures in this
 * chapter are slightly complicated: to heavily optimize the case when
 * you are tracing through the DFA graph entirely out of the
 * superstate cache.  In the optimized case, you can advance a DFA
 * through one state transition with only 10-20 machine instructions.
 * That number of instructions, multiplied by the length of the input
 * string, is the entire overhead of a DFA comparison running out
 * of the cache!
 */

/************************************************************************
 *(h2 "DFA State Transition Tables")
 * 
 * A regular expression DFA is represented by a collection of
 * transition tables, one per DFA state.  If the character set has `N'
 * characters, the transition tables have `N' entries.
 *
 * Each entry in a transition table is one instruction for a virtual
 * machine.
 */

/*(c #s"struct rx_inx" :category type)
 * struct rx_inx;
 *
 * This structure holds one instruction for a virtual machine that
 * interprets the DFA.  For example, a structure of this type is used
 * to represent one entry in the transition table of a DFA state.
 *
 * This structure type is padded to a power-of-2 bytes because in some
 * critical code, we dispatch by indexing an array of these
 * structures.  If that indexing can be accomplished by just a shift
 * of the index, we're much happier than if it requires a full-fledged
 * multiplication.
 *
 * An instruction includes an opcode and two operands.  The opcode is
 * stored in the field `inx' and the operands are stored in the fields
 * `data' and `data_2'.  The field `data' is only used by one
 * instruction (`rx_next_char').  For all other instructions, `data'
 * is set to 0.  This is used to speed up dispatching: the field
 * `data' is fetched and tested before the field `inx'.  If data is
 * not 0, there is no need to fetch the field `inx' at all -- we
 * already know it contains `rx_next_char' and we already have the
 * primary operand to that instruction in a register.
 *
 * In the case of a `rx_next_char' instruction, the primary operand is
 * a pointer to the transition table of the destination DFA state.
 * The secondary operand is the state label of the destination state.
 * Thus, the state transition part of a DFA matching loop is something
 * like:
 *
 *	while (1)
 *	  {
 *	    // Presume that the current transition table entry 
 *	    // for the next input character (`input_char') contains
 *	    // the virtual instruction `rx_next_char'.
 *	    // 
 *	    // Fetch the operands for that instruction:
 *	    //
 * 	    next_state_table 
 *	      = (struct rx_inx *)current_state_table[input_char].data;
 *	    next_state_label
 *	      = (long)current_state_table[input_char].data_2;
 * 
 * 	    // Now check, is it really a `rx_next_char' instruction?
 *	    //
 *	    if (!next_state_table)
 *	      {
 *		// Primary operand was 0 -- not really `rx_next_char'.
 *		//
 * 	        ... handle a cache miss or illegal input character
 *		... patch up the values of `next_state_table' and
 *		... `next_state_label'
 *	      }
 *
 * 	    // Advance to the next DFA state:
 *	    //
 *	    current_state_table = next_state_table;
 *	    current_state_label = next_state_label;
 *
 * 	    // Test whether or not it is a final state:
 *	    //
 *	    if (current_state_label)
 *	      {
 *	        // we have reached a final state in the DFA
 * 		... perhaps return or perhaps continue to look for
 *		... a longer match.
 *	      }
 *
 * 	    // Process further input.
 *	    //
 *	    input_char = read_next_char ();
 *         }
 *
 * 
 * Here are the details of the strucure type:
 *
 insert*/
struct rx_inx 
{
  void * data;
  /*   	The primary operand of this instruction.  For efficient
	indexing, this must be the first field of the structure.*/
	
  void * data_2;
  /*   	The secondary operand of this instruction.*/
	
  void * inx;
  /*   	The opcode for this instruction.  This field is declared
    	`void *' to force it to occupy exactly one word.  In fact it
    	holds a value of type `enum rx_opcode'.*/

  struct rx_inx * next_same_edge;
  /* 	A list of instruction frames, anchored at e->inx_list,
	of instructions built for the same edge. */
};
/*end-insert
 */


/*(c #s"enum rx_opcode" :category type)
 * enum rx_opcode;
 * 
 * This type represents opcodes for a regular expression DFA virtual
 * machine.
 * 
 insert*/
enum rx_opcode
{
  rx_cache_miss = 0,		/* must be 0 */
  /* 	An `rx_cache_miss' instruction means that a transition
	has been reached whose edge or destination state is not
	currently in the DFA cache.

	If the edge is missing from the cache, then `data' and
	`data_2' are 0.  If the edge is in the cache, but the
	destination state is either not in the cache or is unknown to
	the edge, then `data' is 0 and `data_2' points to the edge.*/

	
  rx_next_char = rx_cache_miss + 1,
  /*	An `rx_next_char' instruction means that a transition has been
	reached to a DFA state that is known to be in the DFA state
	cache.

	`data' points to the transition table of the destination state
	(the `transitions' field of a `struct rx_superstate').

	`data_2' is set to the state label of the destination state.

	If the `data' field of a `struct rx_inx' is not 0, then the
     	`inx' field is guaranteed to be `rx_next_char'.  If the
     	insruction is `rx_next_char', a DFA interpreter is almost
     	certain to need the value in `data'.  A DFA interpreter
     	should optimize this instruction above all others.  This
     	suggests that interpreters should dispatch first on the value
     	in `data' (on whether or not it is 0), and then (if `data' is
     	0) on the instruction in field `inx'.*/

	
  rx_backtrack = rx_next_char + 1,
  /*	An `rx_backtrack' instruction means that there is no
	transition defined for an input character.  If the DFA is
	being used for regular expression comparison, this means that
	the input does not match.*/

  rx_huge_char = rx_backtrack + 1,
  /* 	An `rx_huge_char' instruction means that the the character
 	is a UTF-16 high surrogate. */
	

  rx_bogus_utf8 = rx_backtrack + 1,
  /*	Second, third or fourth byte of a UTF-8 character.
	*/

  rx_2byte_utf8 = rx_bogus_utf8 + 1,
  /* 	First byte of a 2-byte UTF-8 character.
	*/

  rx_3byte_utf8 = rx_2byte_utf8 + 1,
  /* 	First byte of a 3-byte UTF-8 character.
	*/

  rx_4byte_utf8 = rx_3byte_utf8 + 1,
  /* 	First byte of a 4-byte UTF-8 character.
	*/

  rx_num_instructions = rx_4byte_utf8 + 1,
};
/*end-insert
 */

/************************************************************************
 *(h2 "DFA States")
 * 
 */

/*(c #s"struct rx_superstate" :category type)
 * struct rx_superstate;
 * 
 * This structure holds one state in a regular expression DFA.
 * 
 insert*/
struct rx_super_transition;	/* no such structure */
typedef struct rx_super_transition ** rx_transition_table;

struct rx_superstate
{
  struct rx_superset * members;
  /* 	A list of NFA states: the NFA state set corresponding to this
	DFA state.  No two DFA states have the same set of members. */

  struct rx_super_edge * outgoing_edges;
  /* 	A list of outgoing DFA edges, linked by `next_same_present'.
	At any time, some edges for this state may be missing from the
	list (because they are missing from the DFA cache).  To build
	a complete DFA, it is necessary to traverse the complete
	transition table of every state, keeping locks on each state
	encountered so that none are flushed from the DFA cache.  (Be 
	advised: DFA graphs can be quite large.)*/

	
  struct rx_super_edge * incoming_edges;
  /* 	A queue of incoming DFA edges, linked by `next_same_dest' and
	`prev_same_dest'.  Again, those edges not in the DFA cache
	are missing from this list.  Also missing from this list are
	edges that are in the cache, but that do not yet know their 
	destination state.*/

	
  /* Superstate Locks and Superstate Cache Management
   *
   * Every superstate has a lock count -- a kind of reference
   * count.  While your code holds a pointer to a superstate, that 
   * superstate should have a non-0 lock count unless you are
   * certain that none of the functions that allocate storage in
   * the DFA cache will be called.  You can use `rx_lock_superstate' 
   * and `rx_unlock_superstate' to manipulate this reference count.
   *
   * The gory details of memory management for DFA states is
   * interesting and relevant to programmers who want to obtain the
   * best performance from this library.  Many of those details are
   * explained here.
   *
   * Upon creation, superstates are given a lock count of 0 and are
   * added to the back of a queue of live superstates.  When a
   * cache-hit yields an already existing superstate, it is moved
   * back to back of that queue.  Thus, the states most recently
   * returned from the DFA cache tend to be at the back of the queue
   * of live states.
   *
   * While a state has a lock count of 0, it is a candidate to be
   * made semifree and then truly free.  This can happen any time the
   * old state reaches the _front_ of the queue of live states and
   * new states or edges are allocated in the DFA cache (such as
   * during a call to `rx_handle_cache_miss').  When this occurs,
   * incoming transitions to the state are modified to cause cache
   * misses, the state is moved from the queue of live states to the
   * back of the queue of semifree states, and the state is a
   * candidate to become truly free.  To convert a state which is
   * semifree back to a live state, use `rx_refresh_this_superstate'.
   *
   * When a state becomes semifree, there are two possibilities of
   * what will happen next.  One possibility is that Rx will reclaim
   * the state's storage.  This will happen if the state reaches the
   * _front_ of the queue of semifree states at a time when Rx detects
   * that the DFA cache is over-full.
   *
   * The other possibility is that a semifree state will be
   * referenced by a DFA transition which results in a cache miss.
   * The cache miss handler reverts the semifree state to a live
   * state.  Restoring the state this way is less expensive than
   * rebuilding the state from scratch and gives us the information
   * that the state is useful enough to keep in the cache.  In
   * effect, we use the cache misses that restore semifree states to
   * sort the two queues of DFA states, moving popular states to the
   * back of those queues, and letting unpopular states drift to the
   * front of the live queue, then through the semifree queue, and
   * finally into being recycled.
   * 
   * There is a (soft) upper bound on the amount of memory Rx will use
   * for caching superstates.  While that upper bound is exceeded, Rx
   * tries to free some superstates which have a lock count of 0,
   * starting with the head of the queue of semifree states.
   */
  int locks;			/* A reference count. */
  int is_semifree;		/* Is this state live or semifree? */

  struct rx_superstate * next_recyclable;
  struct rx_superstate * prev_recyclable;
  /*	While a DFA state is live, these links place the state on 
	a queue of all live DFA states.

	While the state is semifree, these links place the state on
	queue of all semifree DFA states.*/


  
  /* At run-time, a matcher follows transitions from one superstate
   * to the next.  At times, a destination state may be missing from
   * the superstate cache or from a particular instruction frame.  In
   * those cases, a destination superset (set of NFA states) is
   * computed, and that is used as a key to search the cache of DFA
   * states.  If a cache entry is found (and valid) then the
   * superstate is missing only from the instruction frame, which is
   * then filled in, and the transition resumed normally.  If no
   * cache entry is found, a new superstate is constructed for that
   * superset.
   *
   * The following field (`rx_id') is used when validating cache
   * entries in the superstate cache keyed on supersets.  If a cache
   * search for a particular DFA state yields this state, the result
   * is only valid if this field agrees with the `rx_id' field of the
   * NFA.
   *
   * This field is necessary because of the possibility of the
   * following (unlikely but not impossible) sequence of events:
   *
   *	The caller allocates an NFA (call it "NFA A").
   *
   *	The caller builds DFA states based on NFA A.
   *
   *	The caller frees NFA A.  The DFA states continue to exist
   *	until flushed from the DFA cache from lack of use.
   *
   *	The caller allocates a new NFA ("NFA B") which happens
   *	to use the same region of the heap as NFA A.
   *	By coincidence, some of the NFA states sets for NFA A 
   *	and NFA B hash to the same value.  Consequently, when
   *	looking for a DFA state built for NFA B, the cache probe
   *	might yield a stale DFA state built for NFA A.
   *
   *	But luckilly, NFA A and NFA B are guaranteed to have 
   *	different values for the field `rx_id'.  By comparing
   *	the `rx_id' field of NFA B to the superstate yielded by
   *	the cache probe, we can know to invalidate that cache 
   * 	entry.
   */
  int rx_id;

  /* Superstates of a given NFA are numbered sequentially.  The
   * sequence number counter is the global variable
   * `rx_superstate_counter' which programs are free to modify.
   *
   * Sequence numbers are useful for applications which manipulate the
   * DFA graph in ways other than simply following transition tables.
   */
  int seq;

  /* The following field is used by `rx_mkstatic', which builds a
   * complete DFA by examining superstates.
   */
  long table_id;

  int storage_unit_size;	/* see nfa.h */
  int has_huge_table;		/* yes for Unicode */

#ifdef RX_LARGE_TABLES
  struct rx_inx *** huge_char_transitions;
#else
  struct rx_inx **** huge_char_transitions;
#endif

  /* struct rx_inx transitions[1]; */
  struct rx_super_transition * transitions[1];
};
/*end-insert
 */


#define rx_transitions_to_suprestate(T)		((struct rx_superstate *) \
						 ((char *)(T) \
						  - ((unsigned long) \
						     ((struct rx_superstate *)0)->transitions)))


#define rx_transition8(T,N)			(((struct rx_inx *)(T)) + (N))
#define rx_subset_transitions8(T,N)		(((struct rx_inx *)(T)) + bitset_subset_offset(N))

#ifdef RX_LARGE_TABLES

#  define rx_page1_index16(N)			((N) >> 8)
#  define rx_page2_index16(N)			((N) & 0xff)
#  define rx_page2_subset_index16(N)		bitset_subset_offset (rx_page2_index16(N))
#  define rx_page1_16(T)			((struct rx_inx **)(T))
#  define rx_page2_16(T,N)			rx_page1_16 (T)[rx_page1_index16(N)]

#  define rx_transition16(T,N)			(rx_page2_16 ((T), (N)) + rx_page2_index16 (N))
#  define rx_subset_transitions16(T,N)		(rx_page2_16 ((T), (N)) + rx_page2_subset_index16 (N))


#  define rx_page1_size21			32
#  define rx_page2_size21			256
#  define rx_page1_index21(N)			((N) >> 16)
#  define rx_page2_index21(N)			(((N) >> 8) & 0xff)
#  define rx_page3_index21(N)			((N) & 0xff)
#  define rx_page3_subset_index21(N)		bitset_subset_offset (rx_page3_index21 (N))
#  define rx_page2_21(T,N)			((T)[rx_page1_index21 (N)])
#  define rx_page3_21(T,N)			rx_page2_21 ((T), (N))[rx_page2_index21 (N)]

#  define rx_transition21(T,N)			(rx_page3_21 ((T), (N)) + rx_page3_index21 (N))
#  define rx_subset_transition21(T,N)		(rx_page3_21 ((T), (N)) + rx_page3_subset_index21 (N))


#else /* !defined(RX_LARGE_TABLES) */


#  if (bits_per_subset == 32)

#    define rx_page2_size			(8)
#    define rx_page3_size			bits_per_subset
#    define rx_page1_index16(N)			((N) >> 8)
#    define rx_page2_index16(N)			(((N) >> 5) & 0x7)
#    define rx_page3_index16(N)			((N) & 0x1f)

#    define rx_page1_size21			256
#    define rx_page2_size21			32
#    define rx_page3_size21			(8)
#    define rx_page4_size21			bits_per_subset
#    define rx_page1_index21(N)			((N) >> 13)
#    define rx_page2_index21(N)			(((N) >> 8) & 0x1f)
#    define rx_page3_index21(N)			(((N) >> 5) & 0x7)
#    define rx_page4_index21(N)			((N) & 0x1f)


#  elif (bits_per_subset == 64)

#    define rx_page2_size			(4)
#    define rx_page3_size			bits_per_subset
#    define rx_page1_index16(N)			((N) >> 8)
#    define rx_page2_index16(N)			(((N) >> 6) & 0x3)
#    define rx_page3_index16(N)			((N) & 0x3f)

#    define rx_page1_size21			256
#    define rx_page2_size21			32
#    define rx_page3_size21			(8)
#    define rx_page4_size21			bits_per_subset
#    define rx_page1_index21(N)			((N) >> 13)
#    define rx_page2_index21(N)			(((N) >> 8) & 0x1f)
#    define rx_page3_index21(N)			(((N) >> 6) & 0x3)
#    define rx_page4_index21(N)			((N) & 0x3f)

#  else

#    error "odd bits_per_subset in hackerlab/rx/super.h"

#  endif

#  define rx_page1_16(T)			((struct rx_inx ***)(T))
#  define rx_page2_16(T,N)			rx_page1_16 (T)[rx_page1_index16(N)]
#  define rx_page3_16(T,N)			rx_page2_16 (T,N)[rx_page2_index16(N)]
#  define rx_page3_subset_index16(N)		bitset_subset_offset (rx_page3_index16(N))

#  define rx_transition16(T,N)			(rx_page3_16 ((T), (N)) + rx_page3_index16 (N))
#  define rx_subset_transitions16(T,N)		(rx_page3_16 ((T), (N)) + rx_page3_subset_index16 (N))


#  define rx_page1_21(T)			(T)
#  define rx_page2_21(T,N)			rx_page1_21(T)[rx_page1_index21(N)]
#  define rx_page3_21(T,N)			rx_page2_21((T),(N))[rx_page2_index21(N)]
#  define rx_page4_21(T,N)			rx_page3_21((T),(N))[rx_page3_index21(N)]
#  define rx_page4_subset_index21(N)		bitset_subset_offset (rx_page4_index21 (N))

#  define rx_transition21(T,N)			(rx_page4_21 ((T), (N)) + rx_page4_index21 (N))
#  define rx_subset_transition21(T,N)		(rx_page4_21 ((T), (N)) + rx_page4_subset_index21 (N))

#endif







/*(c rx_lock_superstate :category macro)
 * #define rx_lock_superstate(RX_NFA, SUPERSTATE)
 * 
 * Increment the reference count of a DFA state.
 */
#define rx_lock_superstate(R,S)  ((S)->locks++)

/*(c rx_unlock_superstate :category macro)
 * #define rx_unlock_superstate(RX_NFA, SUPERSTATE)
 * 
 * Decrement the reference count of a DFA state.
 */
#define rx_unlock_superstate(R,S) (--(S)->locks)
	
/*(c rx_superstate_transition_table :category macro)
 * #define rx_superstate_transition_table(RX_NFA, SUPERSTATE)
 * 
 * As a function:
 *
 *	extern struct rx_inx * 
 *	rx_superstate_transition_table (struct rx_nfa * rx,
 *					struct rx_superstate * state);
 *
 * Return the DFA state transition table that corresponds to a
 * particular superstate
 */
#define rx_superstate_transition_table(RX_NFA, SUPERSTATE) \
	&((SUPERSTATE)->transitions)

/*(c rx_transition_table_superstate :category macro)
 * #define rx_transition_table_superstate(RX_NFA, TABLE)
 * 
 * As a function:
 *
 *	extern struct rx_superstate * 
 *	rx_superstate_transition_table (struct rx_nfa * rx,
 *					struct rx_inx * table);
 *
 * Return the superstate that corresponds to a particular
 * DFA state transition table.
 */
#define rx_transition_table_superstate(RX_NFA, TABLE) \
	((struct rx_superstate *)\
	 ( ((char *)(TABLE))  - (char *)(&((struct rx_superstate *)0)->transitions)))



/*(c rx_superstate_counter :category variable)
 * extern int rx_superstate_counter;
 *
 * As DFA states are allocated, they are assigned sequential
 * numbers.  The number of the next DFA state is stored in this
 * variable.
 */
extern int rx_superstate_counter;



/************************************************************************
 *(h2 "DFA Edges")
 * 
 */

/*(c #s"struct rx_super_edge" :category type)
 * 
 * This structure holds one edge of a regular expression DFA.
 *
 * All DFA edges are character set edges -- they are labeled by
 * a non-empty set of characters.  No two of the character sets
 * of edges originating in a common DFA state intersect.
 *
 insert*/
struct rx_super_edge
{
  bits cset;
  /*	The character set of this edge.*/

  struct rx_superstate * present;
  /*   	The DFA source state of this edge.  If the edge exists, this
	state is guaranteed to be in the cache.*/

  struct rx_superstate * future;
  /*   	The DFA destination state of this edge.  This state may or may
	not be in the DFA cache, and if it is in the cache, this edge
	may or may not "know" about it.  If the state is in the cache
	and this edge "knows" about it, then this field points to the
	state.  Otherwise, this field contains 0.*/
	

  struct rx_inx * inx_list;
  /* 	A list of instruction frames, linked by next_same_edge,
	built for this edge. */


  struct rx_super_edge * next_same_present;
  /*   	This points to the next edge in a list of DFA edges sharing a
	common starting state.   The list starts at
	`present->outgoing_edges'.*/

  struct rx_super_edge * next_same_dest;
  struct rx_super_edge * prev_same_dest;
  /*  	These link this edge into a queue of edges sharing a
	common DFA ending state.  These fields are only valid if
	`future' is not 0.  In that case, this edge is stored on
	the queue `future->incoming_edges'.*/

  int is_backtrack;
};
/*end-insert
 */




/************************************************************************
 *(h2 "Sets of NFA States")
 * 
 * Every state of a DFA corresponds to a non-empty set of NFA states.
 * No two DFA states share the same set of NFA states.
 * 
 */

/*(c #s"struct rx_superset" :category type)
 * struct rx_superset;
 * 
 * This structure holds a set of NFA states and is used to represent
 * the set of states that comprise a single DFA state.  In xref:"The
 * NFA Data Types" we encountered another representation for sets of
 * NFA states, used for computing epsilon closures of NFA states.
 * Unfortunately, the requirements of computing epsilon closures and
 * of computing DFA states are too different, so two representations
 * are needed.
 * 
 * For equal `car' and `cdr' (in the sense of `=='), there is at most
 * one of these structures.
 *
 * The empty set is *not* represented by `(struct rx_superset *)0'
 * but instead by a superset structure with `car' and `cdr' equal to
 * 0.  Thus, the last element of every `struct rx_superste' list
 * (linked by `cdr' pointers) is a structure with `car' and `cdr' 0.
 * (This simplifies hash-consing sets of these structures).
 * 
 insert*/
struct rx_superset
{
  struct rx_nfa_state * car;
  /*	This points to the first NFA state in the set.  The
	list of states is kept sorted by id number, from
	least to greatest.

	Note that if the NFA has been freed but the superset has not,
	then this pointer is invalid.  This condition can be detected
	by comparing the `id' field of this structure with the `rx_id'
	field of the NFA over which this set was supposedly built.  If
	the two numbers agree, then the `car' pointer is valid.  If
	the two numbers disagree, then this set was actually built for
	an NFA that no longer exists: it is a stale entry in the cache
	of supersets.*/

  struct rx_superset * cdr;
  /*	This points to the rest of this set.*/

  unsigned long state_label;
  /*  	If all of the state labels of the NFA states in this set are
	0, then this field is 0.  Otherwise, this field is the NFA
	state label with the smallest magnitude.*/

  int has_cset_edges;
  /*   	This is the logical-or (`||') of the same field of the `cdr',
     	and the same-named field of the `car'.  If it is 0, then this
     	superset corresponds to a dead-end DFA state (no outgoing
     	edges).*/

  /* Cache management data.
   */
  int refs;				/* A reference count. */
  int id;				/* See `car', above. */
  struct hashtree_item hash_item;	/* For hash-consing lists */
	

  struct rx_nfa_state * nfa_state;
  /*	If this superset is discovered to be the eclosure of a 
	single NFA state by `rx_nfa_state_to_superstate', this
	field points to that NFA state.

	The state points back to this set in the field
	`superstate_set'.

	If the NFA is subsequently freed, this field is reset to 0.*/
	
  struct rx_superstate * superstate[2];
  /*   	If the DFA states for this superset exists (in the cache of DFA
	states), then this field points to those DFA state.  

	superstate[0] for 1-byte storage unit size,
	superstate[1] for 2-byte storage unit size.
	*/

};
/*end-insert
 */


extern alloc_limits rx__dfa_alloc_limits;


/* automatically generated __STDC__ prototypes */
extern void rx__init_dfa_alloc_limits (void);
extern size_t rx_dfa_cache_failure_pt (void);
extern void rx__dfa_cache_statistics (size_t * threshold,
				      size_t * failure_pt,
				      size_t * in_use,
				      size_t * high_water_mark,
				      int * hits,
				      int * misses,
				      int * total_hits,
				      int * total_misses);
extern void rx_set_dfa_cache_failure_pt (size_t n);
extern int rx__really_free_superstate (void);
extern struct rx_superstate * rx_nfa_state_to_superstate (struct rx_nfa * rx,
							  struct rx_nfa_state * nfa_state,
							  int storage_unit_size);
extern void rx_clear_superstate_table_ids (struct rx_nfa * nfa);
extern struct rx_inx * rx_handle_cache_miss (struct rx_nfa * rx,
					     struct rx_superstate * super,
					     unsigned int chr,
					     void * data_2) ;
#endif  /* INCLUDE__RX__SUPER_H */
