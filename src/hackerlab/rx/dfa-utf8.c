/* dfa-utf8.c - utf8 functions that manipulate regexps as DFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/rx/escape.h"
#include "hackerlab/rx/super.h"
#include "hackerlab/rx/dfa-utf8.h"


/************************************************************************
 *(h0 "DFA String Comparisons for 8-bit Character Storage Units" 
 *    :includes ("rx/dfa.h"
 *		 "rx/dfa_utf8.h"))
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

/************************************************************************
 *(h1 "DFA Comparison Functions for 16-bit Character Storage Units")
 * 
 * The functions in this section compare an input string to a regular
 * expression by advancing through DFA states according to the
 * characters in the input string.
 */


/*(c rx_dfa_utf8_fits)
 * int rx_dfa_utf8_fits (struct rx_dfa * frame,
 *		      const t_uint16 * burst,
 *		      size_t len);
 * 
 * 
 * Compare a DFA to string: is the entire string matched by the DFA?
 * Return a non-zero value (the state-label of the final DFA state) if
 * the string matches, 0 otherwise.
 *
 * This function works by advancing the DFA through all of the
 * characters in the input string and checking the state label of the
 * last state reached.  If that label is not 0, then the string
 * matches.  If that label is 0, or if an illegal input character is
 * reached before the end of the input string, the string does not
 * match.
 * 
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-running Matches".
 */
int
rx_dfa_utf8_fits (int * label,
		  struct rx_dfa * frame,
		  const t_uchar * burst,
		  size_t len)
{
  int adv;
  adv = rx_dfa_utf8_advance (frame, burst, len);
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




/*(c rx_dfa_utf8_advance)
 * int rx_dfa_advance (struct rx_dfa * frame,
 *		       const t_uchar * burst,
 *		       size_t len);
 * 
 * 
 * Advance a DFA, reading characters from the input string.  Stop at
 * the end of the string, returning 1 or when a character is
 * encountered for which no transition is defined, returning 0.
 *
 * This is similar to `rx_dfa_fits', except that in this case, we
 * don't care about the state label of the final state.
 * 
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-running Matches".
 */
int
rx_dfa_utf8_advance (struct rx_dfa * frame,
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
	  t_uchar byte0;
	  t_uchar byte1;
	  t_uchar byte2;
	  t_uchar byte3;
	  t_unicode c;

	  state = rx_transitions_to_suprestate (inx_table);

	  switch ((long)inx->inx)
	    {
	    case rx_bogus_utf8:
	      goto handle_as_backtrack;

	    case rx_2byte_utf8:
	      if (!len)
		goto handle_as_backtrack;
	      byte0 = *burst;
	      byte1 = burst[1];
	      ++burst;
	      --len;
	      if ((byte1 & 0xc0) != 0x80)
		goto handle_as_backtrack;
	      c = ((byte0 & 0x1f) << 6) | (byte1 & 0x3f);
	      goto handle_huge_char;
	      
	    case rx_3byte_utf8:
	      if (len < 2)
		goto handle_as_backtrack;
	      byte0 = *burst;
	      byte1 = burst[1];
	      byte2 = burst[2];
	      burst += 2;
	      len -= 2;
	      if (   ((byte0 == 0xe0)
		      ? ((byte1 < 0xa0) || (byte1 > 0xbf))
		      : ((byte1 & 0xc0) != 0x80))
		  || ((byte2 & 0xc0) != 0x80))
		goto handle_as_backtrack;
	      c = ((byte0 & 0xf) << 12) | ((byte1 & 0x3f) << 6) | (byte2 & 0x3f);
	      goto handle_huge_char;
	      
	    case rx_4byte_utf8:
	      if (len < 3)
		goto handle_as_backtrack;
	      
	      byte0 = *burst;
	      byte1 = burst[1];
	      byte2 = burst[2];
	      byte3 = burst[3];
	      burst += 3;
	      len -= 3;
	      
	      if (  ((byte0 == 0xf0)
		     ? ((byte1 < 0x90) || (byte1 > 0xbf))
		     : ((byte0 == 0xf4)
			? ((byte1 < 0x80) || (byte1 > 0x8f))
			: ((byte1 & 0xc0) != 0x80)))
		  || ((byte2 & 0xc0) != 0x80)
		  || ((byte3 & 0xc0) != 0x80))
		goto handle_as_backtrack;
	      c = ((byte0 & 0x7) << 18) | ((byte1 & 0x3f) << 12) | ((byte2 & 0x3f) << 6) | (byte3 & 0x3f);
	      goto handle_huge_char;
	      
	    handle_huge_char:
	      {
		inx = rx_transition21 (state->huge_char_transitions, c);
		next_table = (rx_transition_table)inx->data;

		while (!next_table)
		  {
		    switch ((enum rx_opcode)inx->inx)
		      {
		      default:
		      case rx_huge_char:
			goto handle_by_panic;
		      case rx_backtrack:
			goto handle_as_backtrack;
		      case rx_cache_miss:
			inx = rx_handle_cache_miss (frame->rx, state, c, inx->data_2);
			if (!inx)
			  {
			    frame->state = 0;
			    frame->final_tag = 0;
			    return -1;
			  }
			next_table = (rx_transition_table)inx->data;
			continue;
		      }
		  }
		continue;
	      }
	      
	      
	    case rx_backtrack:
	    handle_as_backtrack:
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
	    handle_by_panic:
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


/*(c rx_dfa_utf8_advance_to_final)
 * size_t rx_dfa_utf8_advance_to_final (struct rx_dfa * frame,
 *				     	const t_uchar * burst,
 *				     	size_t len);
 * 
 * Advance a DFA, reading characters from a string.
 *
 * Stop at the end of the string, a character with no transition, or
 * when a superstate is encountered with a non-0 label.  Return the
 * number of characters read from the string.
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
rx_dfa_utf8_advance_to_final (size_t * amt,
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
	  t_uchar byte0;
	  t_uchar byte1;
	  t_uchar byte2;
	  t_uchar byte3;
	  t_unicode c;

	  state = rx_transitions_to_suprestate (inx_table);
	  
	  switch ((enum rx_opcode)inx->inx)
	    {
	    case rx_bogus_utf8:
	      goto handle_as_backtrack;

	    case rx_2byte_utf8:
	      if (!len)
		goto handle_as_backtrack;
	      byte0 = *burst;
	      byte1 = burst[1];
	      ++burst;
	      --len;
	      if ((byte1 & 0xc0) != 0x80)
		goto handle_as_backtrack;
	      c = ((byte0 & 0x1f) << 6) | (byte1 & 0x3f);
	      goto handle_huge_char;
	      
	    case rx_3byte_utf8:
	      if (len < 2)
		goto handle_as_backtrack;
	      byte0 = *burst;
	      byte1 = burst[1];
	      byte2 = burst[2];
	      burst += 2;
	      len -= 2;
	      if (   ((byte0 == 0xe0)
		      ? ((byte1 < 0xa0) || (byte1 > 0xbf))
		      : ((byte1 & 0xc0) != 0x80))
		  || ((byte2 & 0xc0) != 0x80))
		goto handle_as_backtrack;
	      c = ((byte0 & 0xf) << 12) | ((byte1 & 0x3f) << 6) | (byte2 & 0x3f);
	      goto handle_huge_char;
	      
	    case rx_4byte_utf8:
	      if (len < 3)
		goto handle_as_backtrack;
	      
	      byte0 = *burst;
	      byte1 = burst[1];
	      byte2 = burst[2];
	      byte3 = burst[3];
	      burst += 3;
	      len -= 3;

	      if (  ((byte0 == 0xf0)
		     ? ((byte1 < 0x90) || (byte1 > 0xbf))
		     : ((byte0 == 0xf4)
			? ((byte1 < 0x80) || (byte1 > 0x8f))
			: ((byte1 & 0xc0) != 0x80)))
		  || ((byte2 & 0xc0) != 0x80)
		  || ((byte3 & 0xc0) != 0x80))
		goto handle_as_backtrack;
	      c = ((byte0 & 0xf) << 18) | ((byte1 & 0x3f) << 12) | ((byte2 & 0x3f) << 6) | (byte3 & 0x3f);
	      goto handle_huge_char;
	      
	    handle_huge_char:
	      {
		inx = rx_transition21 (state->huge_char_transitions, c);
		next_table = (rx_transition_table)inx->data;

		while (!next_table)
		  {
		    switch ((enum rx_opcode)inx->inx)
		      {
		      default:
		      case rx_huge_char:
			goto handle_by_panic;
		      case rx_backtrack:
			goto handle_as_backtrack;
		      case rx_cache_miss:
			inx = rx_handle_cache_miss (frame->rx, state, c, inx->data_2);
			if (!inx)
			  {
			    frame->state = 0;
			    frame->final_tag = 0;
			    return -1;
			  }
			next_table = (rx_transition_table)inx->data;
			continue;
		      }
		  }
		continue;
	      }
	      

	    case rx_backtrack:
	    handle_as_backtrack:
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
	    handle_by_panic:
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

