/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (dfa-cache.c)
 */
/* dfa-cache.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/rx/super.h"
#include "hackerlab/rx/dfa-cache.h"



/************************************************************************
 *(h1 "Tuning the DFA Cache Size"
 *    :includes ("hackerlab/rx/dfa-cache.h"))
 * 
 * |DFA cache|
 * |cache (DFA)|
 * |deterministic finite automata|
 * When Rx compiles a regexp or regular expression, it builds a tree
 * structure that describes the syntax of the expression.  Later, some
 * or all of the tree is converted to a graph, representing a
 * non-deterministic finite automata (NFA).  During matching, NFA are
 * incrementally converted to deterministic finite automata (DFA).
 * 
 * Rx maintains a cache of DFA fragments.  When part of a DFA is
 * needed, the cache is used to avoid redundant construction.  Because
 * DFA can be quite large, DFA fragments are sometimes flushed from
 * the cache to make room.
 * 
 * Note that the DFA cache can preserve DFA fragments beyond the
 * lifetime of a single compiled expression.  If an expression is
 * compiled, then matched, then freed, then recompiled, the recompiled
 * expression will sometimes re-use DFA fragments cached from the
 * first compile.
 * 
 * This chapter presents functions which are used to monitor and tune
 * the performance of the DFA cache.  Be sure to also read
 * xref:"The Impact of NFA and DFA Cache Sizes".
 */


/************************************************************************
 *(h2 "The DFA Cache Replacement Strategy")
 * 
 * |cache replacement strategy (DFA)|
 * |DFA cache replacement strategy|
 * DFA cache entries are approximately sorted from most to least
 * recently used.  When cache space is exhausted, the least recently
 * used entries are discarded.
 * 
 */

/************************************************************************
 *(h2 "The advisory DFA cache Limit")
 * 
 * |DFA cache limit|
 * |cache limit (DFA)|
 * |advisory limit|
 * |DFA cache threshold|
 * |cache threshold (DFA)|
 * The size of the DFA cache is regulated by an advisory limit called
 * the "cache threshold".  The threshold is a size (expressed in bytes)
 * which represents an ideal limit on the amount of memory used by the
 * DFA cache.
 * 
 * If an allocation within the DFA cache would cause the total amount
 * of memory used by the cache to exceed the threshold, Rx attempts to
 * discard sufficient cache entries to avoid exceeding the threshold.
 * This is not always possible.  When necessary for correct operation,
 * Rx will exceed the cache threshold: usually by a small amount;
 * sometimes by a large amount.  (That is why the threshold is called an
 * ^advisory^ limit.)
 * 
 * The default threshold is `1MB'.
 */

/*(c rx_set_dfa_cache_threshold)
 * void rx_set_dfa_cache_threshold (size_t n);
 * 
 * Set the advisory DFA cache limit to `n'.
 */
void
rx_set_dfa_cache_threshold (size_t n)
{
  rx__init_dfa_alloc_limits ();
  lim_set_threshold (rx__dfa_alloc_limits, n);
}


/*(c rx_dfa_cache_threshold)
 * size_t rx_dfa_cache_threshold (void);
 * 
 * Return the current DFA cache limit.
 */
size_t
rx_dfa_cache_threshold (void)
{
  rx__init_dfa_alloc_limits ();
  return lim_threshold (rx__dfa_alloc_limits);
}



/************************************************************************
 *(h2 "DFA Cache Statistics")
 * 
 * |DFA cache statistics|
 * |cache statistics (DFA)|
 * These functions report statistics about the DFA cache.
 * 
 */

/*(c rx_dfa_cache_in_use)
 * size_t rx_dfa_cache_in_use (void);
 * 
 * Return the amount of memory currently in use by the DFA cache.
 */
size_t
rx_dfa_cache_in_use (void)
{
  rx__init_dfa_alloc_limits ();
  return lim_in_use (rx__dfa_alloc_limits);
}


/*(c rx_dfa_cache_high_water_mark)
 * size_t rx_dfa_cache_high_water_mark (void);
 * 
 * Return the largest amount of memory ever used at one time by the
 * DFA cache.
 */
size_t
rx_dfa_cache_high_water_mark (void)
{
  rx__init_dfa_alloc_limits ();
  return lim_high_water_mark (rx__dfa_alloc_limits);
}


/*(c rx_dfa_cache_statistics)
 * void rx_dfa_cache_statistics (size_t * threshold,
 *                               size_t * ign,
 *                               size_t * in_use,
 *                               size_t * high_water_mark,
 *                               int * hits,
 *                               int * misses,
 *                               int * total_hits,
 *                               int * total_misses);
 * 
 * Return statistics about the effectiveness of the DFA cache.
 * 
 * All parameters are used to return values.  Any parameter may be 0.
 *
 * `threshold' returns the DFA cache threshold.
 *
 * `ign' is reserved for future use and should be ignored.
 *
 * `in_use' returns the number of bytes currently used by the DFA cache.
 *
 * `high_water_mark' returns the largest number of bytes ever used by the
 *  DFA cache.
 *
 * `hits' returns an indication of the number of cache hits that have
 * occured within the DFA cache.  (See below.)
 * 
 * `misses' returns an indication the number of cache misses that have
 * occured within the DFA cache. (See below.)
 * 
 * Note: The values returned in `hits' and `misses' are scaled to give
 * greater weight to recent cache activity, and reduced weight to older
 * cache activity.  It is the ratio of `hits' to `misses', not their
 * absolute values, that is interesting.
 * 
 * `total_hits' returns the exact number of cache hits that have occured
 * within the DFA cache over the lifetime of the process.
 * 
 * `total_misses' returns the exact number of cache misses that have occured
 * within the DFA cache over the lifetime of the process.
 *
 */
void
rx_dfa_cache_statistics (size_t * threshold,
			 size_t * ign,
			 size_t * in_use,
			 size_t * high_water_mark,
			 int * hits,
			 int * misses,
			 int * total_hits,
			 int * total_misses)
{
  rx__dfa_cache_statistics (threshold,
			    ign,
			    in_use,
			    high_water_mark,
			    hits,
			    misses,
			    total_hits,
			    total_misses);
}



/************************************************************************
 *(h2 "Flushing the DFA Cache")
 * 
 * |cache flushing (DFA)|
 * |DFA cache flushing|
 */


/*(c rx_flush_dfa_cache)
 * size_t rx_flush_dfa_cache ();
 * 
 * Attempt to flush all entries from the DFA cache.  If there exist
 * locked DFA states, it may not be possible to entirely empty the DFA
 * cache.  (It is not possible to create locked DFA states using only
 * the portion of the interface to Rx that is currently documented.)
 * 
 * Return the number of bytes still allocated to the DFA cache after
 * the flush.
 */
size_t
rx_flush_dfa_cache (void)
{
  while (!rx__really_free_superstate ())
    ;

  return lim_in_use (rx__dfa_alloc_limits);
}

