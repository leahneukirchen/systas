/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (nfa-cache.h)
 */
/* nfa-cache.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__RX__NFA_CACHE_H
#define INCLUDE__RX__NFA_CACHE_H


#include "hackerlab/mem/alloc-limits.h"


/* automatically generated __STDC__ prototypes */
extern void rx_free_some_nfa_memory (void * ign, size_t needed);
extern alloc_limits rx_nfa_cache_limits (void);
extern void rx_set_nfa_cache_threshold (size_t n);
extern size_t rx_nfa_cache_threshold (void);
extern void rx_set_nfa_cache_failure_pt (size_t n);
extern size_t rx_nfa_cache_failure_pt (void);
extern size_t rx_nfa_cache_in_use (void);
extern size_t rx_nfa_cache_high_water_mark (void);
extern void rx_nfa_cache_statistics (size_t * threshold,
				     size_t * ign,
				     size_t * in_use,
				     size_t * high_water_mark,
				     int * hits,
				     int * misses,
				     int * ign2);
extern size_t rx_flush_nfa_cache (void);
extern void * rx_nfa_cache_malloc (size_t size);
extern void * rx_nfa_cache_soft_malloc (size_t size);
extern void * rx_nfa_cache_realloc (void * prev, size_t size);
extern void rx_nfa_cache_free (void * mem);
#endif  /* INCLUDE__RX__NFA_CACHE_H */
