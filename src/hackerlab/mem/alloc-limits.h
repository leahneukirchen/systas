/* tag: Tom Lord Tue Dec  4 14:41:27 2001 (alloc-limits.h)
 */
/* alloc-limits.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__MEM__ALLOC_LIMITS_H
#define INCLUDE__MEM__ALLOC_LIMITS_H



#include "hackerlab/machine/types.h"



struct alloc_limits;
typedef struct alloc_limits * alloc_limits;

typedef void (*lim_free_memory_fn)(void * closure, size_t needed);

extern struct alloc_limits lim_use_malloc_limits;
#define lim_use_malloc (&lim_use_malloc_limits)

#define lim_use_must_malloc ((alloc_limits)0)

extern struct alloc_limits lim_no_allocations_limits;
#define lim_no_allocations (&lim_no_allocations_limits)

#define LIM_END	((size_t)-1)


/* automatically generated __STDC__ prototypes */
extern alloc_limits make_alloc_limits (t_uchar * name,
				       size_t threshold,
				       size_t failure_pt,
				       int panic_on_failure,
				       lim_free_memory_fn free_memory,
				       void * closure);
extern void free_alloc_limits (alloc_limits it);
extern size_t lim_set_threshold (alloc_limits it, size_t threshold);
extern size_t lim_threshold (alloc_limits it);
extern size_t lim_set_failure_pt (alloc_limits it, size_t failure_pt);
extern size_t lim_failure_pt (alloc_limits it);
extern int lim_is_panic_on_failure (alloc_limits it);
extern int lim_set_panic_on_failure (alloc_limits it, int value);
extern size_t lim_in_use (alloc_limits it);
extern size_t lim_high_water_mark (alloc_limits it);
extern void * lim_malloc (alloc_limits limits, size_t amt);
extern void * lim_soft_malloc (alloc_limits limits, size_t amt);
extern void * lim_realloc (alloc_limits limits, void * prev, size_t amt);
extern void * lim_soft_realloc (alloc_limits limits,
				void * prev,
				size_t amt);
extern void lim_free (alloc_limits limits, void * ptr);
extern int lim_prepare (alloc_limits limits, size_t amt);
extern void * lim_malloc_contiguous (alloc_limits limits, size_t base_size, ...);
#endif  /* INCLUDE__MEM__ALLOC_LIMITS_H */
