/* tag: Tom Lord Tue Dec  4 14:41:27 2001 (alloc-limits.c)
 */
/* alloc-limits.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/machine/alignment.h"
#include "hackerlab/os/malloc.h"
#include "hackerlab/os/stdarg.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/alloc-limits.h"


/************************************************************************
 *(h1 "Allocation With Limitations"
 *  	:includes ("hackerlab/mem/alloc-limits.h"))
 * 
 * |allocation limits|
 * |limited allocation|
 * 
 * In some situations it is desriable for a subsystem of a program
 * to limit its use of memory, independently of the rest of the program.
 * The `alloc_limits' functions are for such situations.
 * 
 * There are two steps to using the `alloc_limits' functions.  First,
 * you create an `alloc_limits' object which specifies how much memory
 * your subsystem should, ideally, use, and how much it is permitted to use.
 * See xref:"make_alloc_limits".  Second, use functions like `lim_malloc' 
 * and `lim_free' instead of functions like `malloc' and `free' in the
 * subsystem to which the limits apply.
 * 
 * 
 */
/*(menu)
 */



#define SIZEOF_HEADER		((sizeof (size_t) > MACHINE_ALIGNMENT) ? sizeof (size_t) : MACHINE_ALIGNMENT)
#define MEM_TO_HEADER(B)	((size_t *)((char *)(B) - SIZEOF_HEADER))
#define HEADER_TO_MEM(H)	((void *)((char *)(H) + SIZEOF_HEADER))


struct alloc_limits
{
  t_uchar * name;

  size_t threshold;		/* where does allocation trigger gc? */
  size_t failure_pt;		/* where does allocation fail? */

  /* threshold == 0		never trigger gc
   * failure_pt == 0		never fail
   */

  int panic_on_failure;

  size_t in_use;
  size_t high_water_mark;
  
  lim_free_memory_fn free_memory;
  void * closure;
};

struct alloc_limits lim_use_malloc_limits = 
{
  "alloc_limits for generic malloc",
  0, 0, 0, 0, 0, 0, 0
};

struct alloc_limits lim_no_allocations_limits = 
{
  "alloc_limits which prohibit allocation",
  0, 1, 0, 1, 1, 0, 0
};


/************************************************************************
 *(h2 "Specifying Allocation Limits")
 * 
 * An object of the opaque type `alloc_limits' records the rules which
 * limit memory use in a particular subsystem.
 * 
 */


/*(c make_alloc_limits)
 * alloc_limits make_alloc_limits (t_uchar * name,
 *                                 size_t threshold,
 *                                 size_t failure_pt,
 *                                 int panic_on_failure,
 *                                 lim_free_memory_fn free_memory,
 *                                 void * closure);
 * 
 * Create a new `alloc_limits' object.
 * 
 * `name' is the name of the subsystem to which these allocation limits
 * apply.  The name is useful for debugging purposes.
 * 
 * `threshold' specifies an ideal limit on the amount of memory used
 * by your subsystem.  If a subsystem attempts to allocate more than 
 * `threshold' bytes, the `free_memory' function for that subsystem
 * is invoked (see below).  `threshold' may be 0, in which case the
 * `free_memory' function is never invoked.
 * 
 * `failure_pt' specifies an absolute limit on the amount of memory
 * used by your subsystem.  Allocations beyond `failure_pt' fail
 * (return 0).
 * 
 * `panic_on_failure', if non-0, means that if the failure point is
 * reached, or allocation fails, the program will exit with a
 * message to the standard error stream and a non-0 status.
 * If 0, allocation failures return 0.
 * 
 * |$lim_free_memory_fn|
 * 
 * `free_memory' is a function pointer:
 * 
 * 	typedef void (*lim_free_memory_fn)(void * closure, 
 * 					   size_t needed);
 * 
 * It is called immediately before an allocation that would exceed
 * `threshold' (if `threshold' is not 0).  `closure' is as passed
 * to `make_alloc_limits' (see below).  `needed' is the number of 
 * bytes by which the proposed allocation causes the total amount of
 * memory used by the subsystem to exceed `threshold'.  It is completely
 * safe to call `lim_free' from your `free_memory' function.  It
 * is possible to call `lim_malloc' or `lim_realloc', but if your
 * program does this, `free_memory' must be reentrant.
 * 
 * `closure' is an opaque value passed to `free_memory'.
 * 
 * If a new `alloc_limits' object can not be allocated, this function
 * calls `panic' and does not return.
 * 
 * As an alternative to calling `make_alloc_limits' in some situations,
 * three pre-defined allocation limits are declared in `alloc-limits.h':
 * 
 *	extern alloc_limits lim_use_malloc;
 *	extern alloc_limits lim_use_must_malloc;
 *	extern alloc_limits lim_no_allocations;
 * 
 * Allocations performed with `lim_use_malloc' are unlimited and are
 * effectively like allocations performed with `malloc' or `realloc'.
 * Failed allocations return 0.
 * 
 * Allocations performed with `lim_use_must_malloc' are unlimited and
 * are effectively like allocations performed with `must_malloc' or
 * `must_realloc'.  Failed allocations cause the process to exit with
 * a non-0 status.
 * 
 * Allocations performed with `lim_no_allocations' always fail and return
 * 0.  `lim_free' has no effect when passed `lim_no_allocations'.
 */
alloc_limits
make_alloc_limits (t_uchar * name,
		   size_t threshold,
		   size_t failure_pt,
		   int panic_on_failure,
		   lim_free_memory_fn free_memory,
		   void * closure)
{
  alloc_limits answer;

  answer = (alloc_limits)must_malloc (sizeof (struct alloc_limits));

  answer->name = name;
  answer->threshold = threshold;
  answer->failure_pt = failure_pt;
  answer->panic_on_failure = panic_on_failure;
  answer->in_use = 0;
  answer->high_water_mark = 0;
  answer->free_memory = free_memory;
  answer->closure = closure;

  return answer;
}

/*(c free_alloc_limits)
 * void free_alloc_limits (alloc_limits limits);
 * 
 * Free an allocation limits object.
 */
void
free_alloc_limits (alloc_limits limits)
{
  must_free ((void *)limits);
}



/*(c lim_set_threshold)
 * size_t lim_set_threshold (alloc_limits it, size_t threshold);
 * 
 * Modify the `threshold' of an `alloc_limits' object.  Return
 * the old `threshold'.
 * 
 * This function does not immediately call `free_memory', even if
 * the total amount of memory allocated exceeds the new `threshold'.
 */
size_t
lim_set_threshold (alloc_limits it, size_t threshold)
{
  size_t old;
  old = it->threshold;
  it->threshold = threshold;
  return old;
}


/*(c lim_threshold)
 * size_t lim_threshold (alloc_limits limits);
 * 
 * Return the current `threshold' of allocation limits `limits'.
 */
size_t
lim_threshold (alloc_limits limits)
{
  return limits->threshold;
}


/*(c lim_set_failure_pt)
 * size_t lim_set_failure_pt (alloc_limits limits, size_t failure_pt);
 * 
 * Modify the `failure_pt' of an `alloc_limits' object.  Return
 * the old `failure_pt'.
 * 
 */
size_t
lim_set_failure_pt (alloc_limits limits, size_t failure_pt)
{
  size_t old;
  old = limits->failure_pt;
  limits->failure_pt = failure_pt;
  return old;
}


/*(c lim_failure_pt)
 * size_t lim_failure_pt (alloc_limits limits);
 * 
 * Return the `failure_pt' of allocation limits `limits'.
 */
size_t
lim_failure_pt (alloc_limits limits)
{
  return limits->failure_pt;
}


/*(c lim_is_panic_on_failure)
 * int lim_is_panic_on_failure (alloc_limits limits);
 * 
 * Return the value of the `panic_on_failure' flag of
 * allocation limits `limits'.
 */
int
lim_is_panic_on_failure (alloc_limits limits)
{
  return limits->panic_on_failure;
}


/*(c lim_set_panic_on_failure)
 * int lim_set_panic_on_failure (alloc_limits limits, int value);
 * 
 * Set the `panic_on_failure' flag of allocation limits `limits'.
 * 
 * Return the old value.
 */
int
lim_set_panic_on_failure (alloc_limits limits, int value)
{
  int was;

  was = limits->panic_on_failure;
  limits->panic_on_failure = value;
  return was;
}

/*(c lim_in_use)
 * size_t lim_in_use (alloc_limits limits);
 * 
 * Return the amount (in bytes) of memory allocation charged
 * to `limits' and not yet freed.
 */
size_t
lim_in_use (alloc_limits limits)
{
  return limits->in_use;
}


/*(c lim_high_water_mark)
 * size_t lim_high_water_mark (alloc_limits limits);
 * 
 * Return the largest amount (in bytes) of outstanding memory 
 * allocation charged to `limits' during the lifetime of the process.
 */
size_t
lim_high_water_mark (alloc_limits limits)
{
  return limits->high_water_mark;
}




/************************************************************************
 *(h2 "Allocating and Freeing")
 * 
 * These functions allocate and free memory, similarly to 
 * `malloc', `realloc', and `free', but with allocation limits.
 * 
 */


/*(c lim_malloc)
 * void * lim_malloc (alloc_limits limits, size_t amt);
 * 
 * Allocate `amt' bytes of memory.
 * 
 * If the allocation would exceed the threshold of `limits',
 * invoke the `free_memory' function first.
 * 
 * If the allocation would exceed the `failure_pt' of `limits', or if
 * the underlying `malloc' fails, return 0 (if the `panic_on_failure'
 * flag of `limits' is 0) or exit the process with a non-0 status (if
 * `panic_on_failure' is non-0).
 * 
 * Adjust the `in_use' and `high_water_mark' of `limits'.
 * 
 * If `limits' is 0, this function works like xref:"must_malloc".
 * 
 */
void *
lim_malloc (alloc_limits limits, size_t amt)
{
  if (!limits)
    return must_malloc (amt);

  if (lim_prepare (limits, amt))
    {
      if (limits->panic_on_failure)
	{
	  panic_msg ("allocation failure");
	  panic (limits->name);
	}
      else
	return 0;
    }

  return lim_soft_malloc (limits, amt);
}


/*(c lim_soft_malloc)
 * void * lim_soft_malloc (alloc_limits limits, size_t amt);
 * 
 * Allocate `amt' bytes of memory.  Return the newly allocated memory.
 * 
 * If the allocation would exceed the `failure_pt' of `limits',
 * or if the underlying `malloc' fails, return 0 (if the
 * `panic_on_failure' flag of `limits' is 0) or exit the
 * process with a non-0 status.
 * 
 * Adjust the `in_use' and `high_water_mark' of `limits'.
 * 
 * This function *does not* invoke the `free_memory' function of
 * `limits', even if the allocation would exceed the threshold of
 * `limits'.
 * 
 * If `limits' is 0, this function works like xref:"must_malloc".
 * 
 */
void *
lim_soft_malloc (alloc_limits limits, size_t amt)
{
  void * answer;

  if (!limits)
    return must_malloc (amt);

  if (limits->failure_pt && (limits->in_use + amt >= limits->failure_pt))
    {
      if (limits->panic_on_failure)
	{
	  panic_msg ("allocation failure");
	  panic (limits->name);
	}
      else
	return 0;
    }

  answer = (void *)malloc (amt + SIZEOF_HEADER);

  if (!answer)
    {
      if (limits->panic_on_failure)
	{
	  panic_msg ("allocation failure");
	  panic (limits->name);
	}
      else
	return 0;
    }

  *(size_t *)answer = amt;
  limits->in_use += amt;
  if (limits->in_use > limits->high_water_mark)
    limits->high_water_mark = limits->in_use;
  return HEADER_TO_MEM (answer);
}


/*(c lim_realloc)
 * void * lim_realloc (alloc_limits limits, void * prev, size_t amt);
 * 
 * Reallocate `prev' as `amt' bytes of memory.  Copy (up to) `amt' bytes
 * of data from `prev' to the newly allocated memory.
 * 
 * If the allocation would exceed the threshold of `limits',
 * invoke the `free_memory' function first.
 * 
 * If the allocation would exceed the `failure_pt' of `limits',
 * or if the underlying `malloc' fails, return 0 (if the
 * `panic_on_failure' flag of `limits' is 0) or exit the
 * process with a non-0 status.
 * 
 * Adjust the `in_use' and `high_water_mark' of `limits'.
 * 
 * If `prev' is 0, this function behaves like `lim_malloc'.
 * 
 * 
 * If `limits' is 0, this function works like xref:"must_realloc".
 * 
 */
void *
lim_realloc (alloc_limits limits, void * prev, size_t amt)
{
  void * base;
  size_t prev_amt;

  if (!limits)
    return must_realloc (prev, amt);

  if (!prev)
    return lim_malloc (limits, amt);

  base = MEM_TO_HEADER (prev);
  prev_amt = *(size_t *)base;

  if (amt > prev_amt)
    {
      if (lim_prepare (limits, amt - prev_amt))
	{
	  if (limits->panic_on_failure)
	    {
	      panic_msg ("allocation failure");
	      panic (limits->name);
	    }
	  else
	    return 0;
	}
    }

  return lim_soft_realloc (limits, prev, amt);
}


/*(c lim_soft_realloc)
 * void * lim_soft_realloc (alloc_limits limits,
 *                          void * prev,
 *                          size_t amt);
 * 
 * 
 * Reallocate `prev' as `amt' bytes of memory.  Copy (up to) `amt' bytes
 * of data from `prev' to the newly allocated memory.
 * 
 * If the allocation would exceed the `failure_pt' of `limits', or if
 * the underlying `malloc' fails, return 0 (if the
 * `panic_on_failure' flag of `limits' is 0) or exit the
 * process with a non-0 status.
 * 
 * Adjust the `in_use' and `high_water_mark' of `limits'.
 * 
 * This function *does not* invoke the `free_memory' function of
 * `limits', even if the allocation would exceed the threshold of
 * `limits'.
 * 
 * If `limits' is 0, this function works like xref:"must_realloc".
 * 
 */
void *
lim_soft_realloc (alloc_limits limits,
		  void * prev,
		  size_t amt)
{
  void * base;
  size_t prev_amt;
  void * answer;

  if (!limits)
    return must_realloc (prev, amt);

  if (!prev)
    return lim_soft_malloc (limits, amt);

  base = MEM_TO_HEADER (prev);
  prev_amt = *(size_t *)base;

  if (amt > prev_amt)
    {
      if (limits->failure_pt && (limits->in_use + amt - prev_amt >= limits->failure_pt))
	{
	  if (limits->panic_on_failure)
	    {
	      panic_msg ("allocation failure");
	      panic (limits->name);
	    }
	  else
	    return 0;
	}
    }
  
  answer = (void *)realloc (base, amt + SIZEOF_HEADER);

  if (!answer)
    {
      if (limits->panic_on_failure)
	{
	  panic_msg ("allocation failure");
	  panic (limits->name);
	}
      else
	return 0;
    }

  *(size_t *)answer = amt;
  limits->in_use -= prev_amt;
  limits->in_use += amt;
  if (limits->in_use > limits->high_water_mark)
    limits->high_water_mark = limits->in_use;
  return HEADER_TO_MEM (answer);
}


/*(c lim_free)
 * void lim_free (alloc_limits limits, void * ptr);
 * 
 * Free `ptr'.  Adjust the `in_use' and `high_water_mark' values
 * of `limits'.
 * 
 * If `limits' is 0, this function works like xref:"must_free".
 * 
 */
void
lim_free (alloc_limits limits, void * ptr)
{
  void * base;
  size_t amt;

  if (limits == lim_no_allocations)
    return;
  
  if (!ptr)
    return;

  if (!limits)
    {
      must_free (ptr);
      return;
    }
  base = MEM_TO_HEADER (ptr);
  amt = *(size_t *)base;
  limits->in_use -= amt;
  free (base);
}



/************************************************************************
 *(h2 "Reserving Limited Memory")
 * 
 * 
 * 
 */


/*(c lim_prepare)
 * int lim_prepare (alloc_limits limits, size_t amt);
 * 
 * Prepare for an allocation of `amt' bytes.
 * 
 * If such an allocation would exceed the threshold of `limits',
 * invoke the `free_memory' function first.
 * 
 * If the allocation would exceed the `failure_pt' of `limits',
 * return -1.  Otherwise, return 0.
 * 
 * 
 * If `limits' is 0, this function simply returns 0.
 * 
 */
int
lim_prepare (alloc_limits limits, size_t amt)
{
  if (!limits)
    return 0;

  if (limits->threshold && (limits->in_use + amt >= limits->threshold) && limits->free_memory)
    limits->free_memory (limits->closure, amt);

  if (limits->failure_pt && (limits->in_use + amt >= limits->failure_pt))
    return -1;

  return 0;
}



void *
lim_malloc_contiguous (alloc_limits limits, size_t base_size, ...)
{
  va_list ap;
  size_t total_size;
  size_t part_offset;
  size_t part_size;
  t_uchar * answer;

  total_size = base_size;

  va_start (ap, base_size);
  while (1)
    {
      part_offset = va_arg (ap, size_t);
      if (part_offset == (size_t)-1)
	break;
      part_size = va_arg (ap, size_t);
      total_size += part_size;
    }
  va_end (ap);

  answer = lim_malloc (limits, total_size);
  if (!answer)
    return 0;

  va_start (ap, base_size);
  total_size = base_size;
  while (1)
    {
      part_offset = va_arg (ap, size_t);
      if (part_offset == (size_t)-1)
	break;
      part_size = va_arg (ap, size_t);
      *(t_uchar **)(answer + part_offset) = answer + total_size;
      total_size += part_size;
    }
  return (void *)answer;
}
