/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (dfa-cache.h)
 */
/* dfa-cache.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__RX__DFA_CACHE_H
#define INCLUDE__RX__DFA_CACHE_H


#include "hackerlab/machine/types.h"


/* automatically generated __STDC__ prototypes */
extern void rx_set_dfa_cache_threshold (size_t n);
extern size_t rx_dfa_cache_threshold (void);
extern size_t rx_dfa_cache_in_use (void);
extern size_t rx_dfa_cache_high_water_mark (void);
extern void rx_dfa_cache_statistics (size_t * threshold,
				     size_t * ign,
				     size_t * in_use,
				     size_t * high_water_mark,
				     int * hits,
				     int * misses,
				     int * total_hits,
				     int * total_misses);
extern size_t rx_flush_dfa_cache (void);
#endif  /* INCLUDE__RX__DFA_CACHE_H */
