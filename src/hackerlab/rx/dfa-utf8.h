/* tag: Tom Lord Tue Dec  4 14:41:38 2001 (dfa-utf8.h)
 */
/* dfa-utf8.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__RX__DFA_UTF8_H
#define INCLUDE__RX__DFA_UTF8_H


#include "hackerlab/machine/types.h"
#include "hackerlab/rx/dfa.h"


/* automatically generated __STDC__ prototypes */
extern int rx_dfa_utf8_fits (int * label,
			     struct rx_dfa * frame,
			     const t_uchar * burst,
			     size_t len);
extern int rx_dfa_utf8_advance (struct rx_dfa * frame,
				const t_uchar * burst,
				size_t len);
extern int rx_dfa_utf8_advance_to_final (size_t * amt,
					 struct rx_dfa * frame,
					 const t_uchar * burst,
					 size_t len);
#endif  /* INCLUDE__RX__DFA_UTF8_H */
