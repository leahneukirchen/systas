/* dfa.c - functions that manipulate regexps as DFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/rx/escape.h"
#include "hackerlab/rx/dfa.h"


/************************************************************************
 *(h0 "DFA String Comparisons" 
 *    :includes ("rx/dfa.h"))
 * 
 * 
 * A common use for regular expressions is to compile them to DFA and
 * compare them to strings using a loop that advances through DFA
 * states.
 *
 * In Rx, the DFA data structure has been heavily optimized for such
 * loops.  The functions in this chapter implement the most common
 * kinds of DFA loop, taking full advantage of the Rx optimizations.
 */
/*(menu)
 */


/*(include-documentation "dfa.h")
 */

/************************************************************************
 *(h1 "DFA Allocation Functions")
 * 
 */

/*(c rx_dfa_alloc)
 * struct rx_dfa * rx_dfa_alloc (alloc_limits limits);
 * 
 * Allocate a new DFA, initially not pointing to any NFA or DFA state.
 *
 */
struct rx_dfa *
rx_dfa_alloc (alloc_limits limits)
{
  struct rx_dfa * a;
  a = (struct rx_dfa *)lim_malloc (limits, sizeof (*a));
  mem_set0 ((t_uchar *)a, sizeof (*a));
  return a;
}


/*(c rx_dfa_free)
 * void rx_free_dfa (alloc_limits limits, struct rx_dfa * dfa);
 * 
 * Release all storage associated with `dfa'.
 */
void
rx_dfa_free (alloc_limits limits, struct rx_dfa * dfa)
{
  if (dfa->rx)
    rx_clear_dfa_state (dfa);
  lim_free (limits, dfa);
}



/************************************************************************
 *(h1 "Initializing a DFA")
 * 
 */


/*(c rx_init_dfa_from_nfa)
 * void rx_init_dfa_from_nfa (struct rx_dfa * frame, struct rx_nfa * rx);
 * 
 * Make `rx' the NFA of DFA machine `frame'.  
 *
 * The DFA machine must not already have a current DFA state (must be
 * in state 0).  See xref:"rx_clear_dfa_state".
 *
 * This function sets the DFA used by `frame', but does not set the
 * current DFA state of `frame'.
 */
void
rx_init_dfa_from_nfa (struct rx_dfa * frame, struct rx_nfa * rx)
{
  frame->rx = rx;
  frame->state = 0;
  frame->final_tag = 0;
}


/*(c rx_init_dfa_from_dfa)
 * void rx_init_dfa_from_dfa (struct rx_dfa * dest, struct rx_dfa * src);
 * 
 * Make the NFA and DFA state of `src' the NFA and DFA state of
 * `dest'.  After a call to this function (which is inexpensive), you
 * have two DFA machines in equivalent states.
 *
 * The DFA machine `dest' must not already have a current DFA state
 * (`state' must be in state 0).  See xref:"rx_clear_dfa_state".
 */
void
rx_init_dfa_from_dfa (struct rx_dfa * dest, struct rx_dfa * src)
{
  dest->rx = src->rx;
  dest->state = src->state;
  dest->final_tag = src->final_tag;
  if (dest->state)
    rx_lock_superstate (dest->rx, dest->state);
}


/*(c rx_clear_dfa_state)
 * void rx_clear_dfa_state (struct rx_dfa * frame);
 * 
 * Clear the DFA state of DFA machine `frame'.
 */
void
rx_clear_dfa_state (struct rx_dfa * frame)
{
  if (frame->state)
    {
      rx_unlock_superstate (frame->rx, frame->state);
      frame->state = 0;
      frame->final_tag = 0;
    }
}

/* rx_dfa_goto_start_superstate
 *
 * Return (or initialize) a DFA to its start state.
 */

/*(c rx_dfa_goto_start_superstate)
 * void rx_dfa_goto_start_superstate (struct rx_dfa * frame,
 *                                    int storage_unit_size);
 * 
 * Return state machine `frame' to its starting state.
 *
 * The starting state is a DFA state built from the epsilon closure of
 * the starting state of the NFA.  See xref:"rx_set_start_state".
 */
int
rx_dfa_goto_start_superstate (struct rx_dfa * frame,
			      int storage_unit_size)
{
  struct rx_superstate * start_state;
  start_state = rx_nfa_state_to_superstate (frame->rx, frame->rx->start_nfa_state, storage_unit_size);
  if (!start_state)
    return -1;
  if (frame->state)
    rx_unlock_superstate (frame->rx, frame->state);
  frame->state = start_state;
  frame->final_tag = start_state->members->state_label;
  rx_lock_superstate (frame->rx, frame->state);
  return 0;
}



/************************************************************************
 *(h1 "DFA Comparison Functions")
 * 
 * The functions in this section compare an input string to a regular
 * expression by advancing through DFA states according to the
 * characters in the input string.
 */

/*(c rx_dfa_can_continue)
 * int rx_dfa_can_continue (struct rx_dfa * frame);
 * 
 * Return a non-zero value if there exist characters for which the
 * current state of DFA machine `frame' has transitions defined.  If
 * this function returns 0, then the machine is in a dead-end state.
 */
int
rx_dfa_can_continue (struct rx_dfa * frame)
{
  return (   frame->state
	  && frame->state->members->has_cset_edges);
}


int
rx_dfa_tag (struct rx_dfa * frame)
{
  return frame->final_tag;
}

/*(c rx_dfa_fits)
 * int rx_dfa_fits (int * label,
 *                  struct rx_dfa * frame,
 *                  const t_uchar * burst,
 *                  size_t len);
 * 
 * 
 * Compare a DFA to string: is the entire string matched by the DFA?
 * Return -1 if an error occurs, 0 otherwise.
 * 
 * The final state label reached is returned in `*label': 0 if 
 * if the string does not match, non-0 if it does.
 *
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-running Matches".
 */
int
rx_dfa_fits (int * label,
	     struct rx_dfa * frame,
	     const t_uchar * burst,
	     size_t len)
{
  int adv;
  adv = rx_dfa_advance (frame, burst, len);
  if (adv < 0)
    return -1;
  else if (!adv)
    {
      *label = 0;
      return 0;
    }
  else
    {
      *label = frame->final_tag;
      return 0;
    }
}




/*(c rx_dfa_advance)
 * int rx_dfa_advance (struct rx_dfa * frame,
 *		       const t_uchar * burst,
 *		       size_t len);
 * 
 * 
 * Advance a DFA, reading characters from the input string.  Stop at
 * the end of the string, returning 1 or when a character is
 * encountered for which no transition is defined, returning 0.
 * -1 == ESPACE.
 *
 * This is similar to `rx_dfa_fits', except that in this case, we
 * don't care about the state label of the final state.
 * 
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-running Matches".
 */
int
rx_dfa_advance (struct rx_dfa * frame,
		const t_uchar * burst,
		size_t len)
{
  rx_transition_table inx_table;

  if (!len)
    return 1;

  inx_table = frame->state->transitions;
  rx_unlock_superstate (frame->rx, frame->state);
  frame->state = 0;

  while (len--)
    {
      struct rx_inx * inx;
      rx_transition_table next_table;

      if (rx_poll)
	(*rx_poll)();

      inx = rx_transition8 (inx_table, *burst);
      next_table = (rx_transition_table)inx->data;
      while (!next_table)
	{
	  struct rx_superstate * state;
	  state = rx_transitions_to_suprestate (inx_table);

	  switch ((long)inx->inx)
	    {
	    case rx_backtrack:
	      /* RX_BACKTRACK means that we've reached the empty
	       * superstate, indicating that match can't succeed
	       * from this point.
	       */
	      frame->state = 0;
	      frame->final_tag = 0;
	      return 0;
	    
	    case rx_cache_miss:
	      /* Because the superstate NFA is lazily constructed,
	       * and in fact may erode from underneath us, we sometimes
	       * have to construct the next instruction from the hard way.
	       * This invokes one step in the lazy-conversion.
	       */
	      inx = rx_handle_cache_miss (frame->rx, state, *burst, inx->data_2);
	      if (!inx)
		{
		  frame->state = 0;
		  frame->final_tag = 0;
		  return -1;
		}
	      next_table = (rx_transition_table)inx->data;
	      continue;
		

	      /* No other instructions are legal here.
	       */
	    default:
	      panic ("unrecognized instruction in rx_dfa_advance");
	  }
	}
      inx_table = next_table;
      ++burst;
    }
  
  frame->state = rx_transitions_to_suprestate (inx_table);
  frame->final_tag = frame->state->members->state_label;
  rx_lock_superstate (frame->rx, frame->state);
  return 1;
}


/*(c rx_dfa_advance_to_final)
 * size_t rx_dfa_advance_to_final (struct rx_dfa * frame,
 *				   const t_uchar * burst,
 *				   size_t len);
 * 
 * Advance a DFA, reading characters from a string.
 * 
 * Stop at the end of the string, a character with no transition, or
 * when a superstate is encountered with a non-0 label.  Return the
 * number of characters read from the string.
 * 
 * 0 == backtrack, 1 == found final state or ran out of characters, -1 == ESPACE
 * 
 * This function stops on a transition *into* a state with a non-0
 * state label.  It doesn't matter if the machine is initially in a
 * state with a non-0 label: the machine will consume the first input
 * character regardless.  That means that if your regular expression
 * can match the empty string, you must detect this condition before
 * calling `rx_dfa_advance_to_final' by checking `dfa->final_tag'
 * after setting the start state of the DFA.
 * 
 * If the match stopped in a final state, `dfa->final_tag' contains
 * the non-0 state label of the final state, otherwise, it contains 0.
 * If the match stopped on an illegal character, `dfa->state' is 0,
 * otherwise it is non-0.
 * 
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-running Matches".
 */
int
rx_dfa_advance_to_final (size_t * amt,
			 struct rx_dfa * frame,
			 const t_uchar * burst,
			 size_t len)
{
  size_t initial_len;
  rx_transition_table inx_table;
  
  if (!len)
    {
      *amt = 0;
      return 1;
    }

  initial_len = len;
  inx_table = frame->state->transitions;
  rx_unlock_superstate (frame->rx, frame->state);
  frame->state = 0;

  while (len--)
    {
      struct rx_inx * inx;
      rx_transition_table next_table;

      if (rx_poll)
	(*rx_poll)();

      inx = rx_transition8 (inx_table, *burst);
      next_table = (rx_transition_table)inx->data;

      while (!next_table)
	{
	  struct rx_superstate * state;

	  state = rx_transitions_to_suprestate (inx_table);
	  
	  switch ((long)inx->inx)
	    {
	    case rx_backtrack:
	      /* RX_BACKTRACK means that we've reached the empty
	       * superstate, indicating that match can't succeed
	       * from this point.
	       */
	      frame->state = 0;
	      frame->final_tag = 0;
	      *amt = (initial_len - len) - 1;
	      return 0;

	    case rx_cache_miss:
	      /* Because the superstate NFA is lazily constructed,
	       * and in fact may erode from underneath us, we sometimes
	       * have to construct the next instruction from the hard way.
	       * This invokes one step in the lazy-conversion.
	       */
	      inx = rx_handle_cache_miss (frame->rx, state, *burst, inx->data_2);
	      if (!inx)
		{
		  frame->state = 0;
		  frame->final_tag = 0;
		  return -1;
		}
	      next_table = (rx_transition_table)inx->data;
	      continue;

	      /* No other instructions are legal here.
	       */
	    default:
	      while (1)
		panic ("unrecognized instruction in rx_dfa_advance_to_final");
	  }
	}

      if (inx->data_2)
	{
	  frame->state = rx_transitions_to_suprestate (next_table);
	  rx_lock_superstate (frame->rx, frame->state);
	  frame->final_tag = (int)inx->data_2;
	  *amt = (initial_len - len);
	  return 1;
	}
      inx_table = next_table;
      ++burst;
    }

  /* Consumed all of the characters. */
  frame->state = rx_transitions_to_suprestate (inx_table);
  rx_lock_superstate (frame->rx, frame->state);
  frame->final_tag = 0;
  *amt = initial_len;
  return 1;
}

