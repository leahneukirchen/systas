/* dfa.h - decls for manipulating regexps as DFAs
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX__DFA_H
#define INCLUDE__RX__DFA_H

/************************************************************************
 *(h1 "DFA Data Structures")
 * 
 */

#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx/dfa-cache.h"
#include "hackerlab/rx/super.h"

/*(c #s"struct rx_dfa" :category type)
 * 
 * This structure type holds pointers to an NFA and one state of the
 * DFA built from that NFA.  In addition, it contains a copy of the
 * state label from that DFA state.
 *
 * In effect, this structure represents a virtual machine whose
 * semantics are defined by the NFA, whose internal state is
 * represented by the DFA state, and whose output is represented by
 * the state label and by a flag that says whether or not the machine
 * is able to accept additional characters from its current state.
 *
 * Rx provides functions which can advance these machines through
 * successive states by following transitions indicated by the
 * characters of an input string.
 * 
 insert*/
struct rx_dfa
{
  struct rx_nfa * rx;			/* The NFA */
  struct rx_superstate * state;		/* The DFA state */
  int final_tag;			/* The DFA state label */

  /* When a `struct rx_dfa' is first allocated, all of those fields
   * are 0.  When the `state' field is not 0, it holds a lock on the
   * superstate it points to.
   */
};
/*end-insert
 */



/* automatically generated __STDC__ prototypes */
extern struct rx_dfa * rx_dfa_alloc (alloc_limits limits);
extern void rx_dfa_free (alloc_limits limits, struct rx_dfa * dfa);
extern void rx_init_dfa_from_nfa (struct rx_dfa * frame, struct rx_nfa * rx);
extern void rx_init_dfa_from_dfa (struct rx_dfa * dest, struct rx_dfa * src);
extern void rx_clear_dfa_state (struct rx_dfa * frame);
extern int rx_dfa_goto_start_superstate (struct rx_dfa * frame,
					 int storage_unit_size);
extern int rx_dfa_can_continue (struct rx_dfa * frame);
extern int rx_dfa_tag (struct rx_dfa * frame);
extern int rx_dfa_fits (int * label,
			struct rx_dfa * frame,
			const t_uchar * burst,
			size_t len);
extern int rx_dfa_advance (struct rx_dfa * frame,
			   const t_uchar * burst,
			   size_t len);
extern int rx_dfa_advance_to_final (size_t * amt,
				    struct rx_dfa * frame,
				    const t_uchar * burst,
				    size_t len);
#endif  /* INCLUDE__RX__DFA_H */
