/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (nfa-cache.c)
 */
/* nfa-cache.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx/nfa-cache.h"


/************************************************************************
 *(h1 "Tuning the NFA Cache Size"
 *    :includes ("hackerlab/rx/nfa-cache.h"))
 * 
 * |NFA cache|
 * |cache (NFA)|
 * |NFA|
 * |non-deterministic finite automata|
 * When Rx compiles a regexp or regular expression, it builds a tree
 * structure that describes the syntax of the expression.  Later, some
 * or all of the tree is converted to a graph, representing a
 * non-deterministic finite automata (NFA).
 * 
 * Rx maintains a cache so that whenever two expressions have equivalent
 * tree structure, they are likely to share a single NFA.  This cache
 * speeds up the processing of regexps by avoiding redundant NFA construction.
 * 
 * Note that the NFA cache can preserve NFA beyond the lifetime of a
 * single compiled expression.  If an expression is compiled, then
 * matched, then freed, then recompiled, the recompiled expression
 * will sometimes re-use the NFA cached from the first compile.
 * 
 * |deterministic finite automata|
 * |DFA|
 * During matching, NFA are incrementally converted to deterministic
 * automata (DFA).  Another cache is kept of DFA fragments (see
 * xref:"Tuning the DFA Cache Size").  Here again, the NFA cache speeds up
 * processing: when a single NFA is re-used, the DFA cache is made
 * more effective.
 * 
 * This chapter presents functions which are used to monitor and tune
 * the performance of the NFA cache.  Be sure to also read
 * xref:"The Impact of NFA and DFA Cache Sizes".
 */


/************************************************************************
 *(h2 "The NFA Cache Replacement Strategy")
 * 
 * |cache replacement strategy (NFA)|
 * |NFA cache replacement strategy|
 * NFA cache entries are approximately sorted from most to least
 * recently used.  When cache space is exhausted, the least recently
 * used entries are discarded.
 * 
 */




#ifndef RX_DEFAULT_NFA_CACHE_SIZE
#define RX_DEFAULT_NFA_CACHE_SIZE (1 << 20)
#endif

static alloc_limits nfa_alloc_limits;



void
rx_free_some_nfa_memory (void * ign, size_t needed)
{
  while (   (lim_in_use (nfa_alloc_limits) + needed > lim_threshold (nfa_alloc_limits))
	 && (0 <= rx__really_free_unfa ()))
    ;
}


static void
init_nfa_alloc_limits (void)
{
  static int init = 1;

  if (init)
    {
      nfa_alloc_limits = make_alloc_limits ("Rx NFA cache",
					    RX_DEFAULT_NFA_CACHE_SIZE,
					    0,
					    0,
					    rx_free_some_nfa_memory,
					    0);
      init = 0;
    }
}

alloc_limits
rx_nfa_cache_limits (void)
{
  init_nfa_alloc_limits ();
  return nfa_alloc_limits;
}


/************************************************************************
 *(h2 "The Advisory NFA Cache Limit")
 * 
 * |NFA cache limit|
 * |cache limit (NFA)|
 * |advisory limit|
 * |NFA cache threshold|
 * |cache threshold (NFA)|
 * The size of the NFA cache is regulated by an advisory limit called
 * the "cache threshold".  The threshold is a size (expressed in bytes)
 * which represents an ideal limit on the amount of memory used by the
 * NFA cache.
 * 
 * If an allocation within the NFA cache would cause the total amount
 * of memory used by the cache to exceed the threshold, Rx attempts to
 * discard sufficient cache entries to avoid exceeding the threshold.
 * This is not always possible.  When necessary for correct operation,
 * Rx will exceed the cache threshold: usually by a small amount;
 * rarely by a large amount.  (That is why the threshold is called an
 * ^advisory^ limit.)
 * 
 * The default threshold is `1MB'.
 */


/*(c rx_set_nfa_cache_threshold)
 * void rx_set_nfa_cache_threshold (size_t n);
 * 
 * Set the advisory NFA cache limit to `n'.
 */
void
rx_set_nfa_cache_threshold (size_t n)
{
  init_nfa_alloc_limits ();
  lim_set_threshold (nfa_alloc_limits, n);
}


/*(c rx_nfa_cache_threshold)
 * size_t rx_nfa_cache_threshold (void);
 * 
 * Return the current NFA cache limit.
 */
size_t
rx_nfa_cache_threshold (void)
{
  init_nfa_alloc_limits ();
  return lim_threshold (nfa_alloc_limits);
}


/* Not yet documented. */

void
rx_set_nfa_cache_failure_pt (size_t n)
{
  init_nfa_alloc_limits ();
  lim_set_failure_pt (nfa_alloc_limits, n);
}


size_t
rx_nfa_cache_failure_pt (void)
{
  init_nfa_alloc_limits ();
  return lim_failure_pt (nfa_alloc_limits);
}


/************************************************************************
 *(h2 "NFA Cache Statistics")
 * 
 * |NFA cache statistics|
 * |cache statistics (NFA)|
 * These functions report statistics about the NFA cache.
 * 
 */


/*(c rx_nfa_cache_in_use)
 * size_t rx_nfa_cache_in_use (void);
 * 
 * Return the amount of memory currently in use by the NFA cache.
 */
size_t
rx_nfa_cache_in_use (void)
{
  init_nfa_alloc_limits ();
  return lim_in_use (nfa_alloc_limits);
}


/*(c rx_nfa_cache_high_water_mark)
 * size_t rx_nfa_cache_high_water_mark (void);
 * 
 * Return the largest amount of memory ever used at one time by the
 * NFA cache.
 */
size_t
rx_nfa_cache_high_water_mark (void)
{
  init_nfa_alloc_limits ();
  return lim_high_water_mark (nfa_alloc_limits);
}



/*(c rx_nfa_cache_statistics)
 * void rx_nfa_cache_statistics (size_t * threshold,
 *                               size_t * ign,
 *                               size_t * in_use,
 *                               size_t * high_water_mark,
 *                               int * hits,
 *                               int * misses,
 *                               int * ign2);
 * 
 * Return statistics about the effectiveness of the NFA cache.
 * 
 * All parameters are used to return values.  Any parameter may be 0.
 *
 * `threshold' returns the NFA cache threshold.
 *
 * `ign' is reserved for future use and should be ignored.
 *
 * `in_use' returns the number of bytes currently used by the NFA cache.
 *
 * `high_water_mark' returns the largest number of bytes ever used by the
 *  NFA cache.
 *
 * `hits' returns the number of cache hits that have occured within
 * the NFA cache.
 * 
 * `misses' returns the number of cache misses that have occured within 
 * the NFA cache.
 * 
 * `ign2' is reserved for future use and should be ignored.
 */
void
rx_nfa_cache_statistics (size_t * threshold,
			 size_t * ign,
			 size_t * in_use,
			 size_t * high_water_mark,
			 int * hits,
			 int * misses,
			 int * ign2)
{
  rx__nfa_cache_statistics (threshold,
			    ign,
			    in_use,
			    high_water_mark,
			    hits,
			    misses,
			    ign2);
}



/************************************************************************
 *(h2 "Flushing the NFA Cache")
 * 
 * |cache flushing (NFA)|
 * |NFA cache flushing|
 */


/*(c rx_flush_nfa_cache)
 * size_t rx_flush_nfa_cache (void);
 * 
 * Attempt to flush all entries from the NFA cache.  If there exist
 * compiled regexps (that have not been freed), it may not be possible 
 * to entirely empty the NFA cache.
 * 
 * Return the number of bytes still allocated to the NFA cache after
 * the flush.
 */
size_t
rx_flush_nfa_cache (void)
{
  while (0 <= rx__really_free_unfa ())
    ;
  return rx_nfa_cache_in_use ();
}




void *
rx_nfa_cache_malloc (size_t size)
{
  void * answer;

  init_nfa_alloc_limits ();
  answer = lim_malloc (nfa_alloc_limits, size);
  return answer;
}


void *
rx_nfa_cache_soft_malloc (size_t size)
{
  void * answer;

  init_nfa_alloc_limits ();
  answer = lim_soft_malloc (nfa_alloc_limits, size);
  return answer;
}


void *
rx_nfa_cache_realloc (void * prev, size_t size)
{
  void * answer;

  init_nfa_alloc_limits ();
  answer = lim_realloc (nfa_alloc_limits, prev, size);
  return answer;
}


/* static void rx_cache_free (int size, char * mem);
 * 
 * Free memory for the NFA state cache.
 */
void
rx_nfa_cache_free (void * mem)
{
  lim_free (nfa_alloc_limits, mem);
}


