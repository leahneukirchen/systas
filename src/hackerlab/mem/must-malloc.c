/* must-malloc.c - malloc or die
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/malloc.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/must-malloc.h"


/************************************************************************
 *(h1 "Allocate or Die"
 *    :includes ("hackerlab/mem/must-malloc.h"))
 *
 * The functions in this section allocate memory.  If an allocation
 * fails, they call `panic' rather than returning.  Thus, these
 * functions may be used without checking for errors.
 */



/*(c must_calloc)
 * void * must_calloc (size_t amt);
 * 
 * Return a newly allocated block large enough to hold `amt' bytes.
 * If no such block can be allocated, exit by calling `panic'.
 * Fill the newly allocated memory with 0 bytes.
 * 
 */
void *
must_calloc (size_t amt)
{
  void * a;
  a = must_malloc (amt);
  mem_set0 (a, amt);
  return a;
}


/*(c must_malloc)
 * void * must_malloc (size_t amt);
 * 
 * Return a newly malloced block large enough to hold `amt' bytes.  If
 * no such block can be allocated, exit by calling `panic'.
 */
void *
must_malloc (size_t amt)
{
  void * a;
  a = (void *)malloc (amt);
  if (amt && !a)
    panic ("out of memory");
  return a;
}


/*(c must_realloc)
 * void * must_realloc (void * ptr, size_t amt);
 * 
 * Return a malloced block large enough to hold `amt' bytes.  If `ptr'
 * is not 0, it should point to a previously malloced block.  The
 * contents of the new block are initialized from the contents of
 * `ptr'.  If the new block is not the same as `ptr', `ptr' is freed.
 *
 * If no block can be allocated, exit by calling `panic'.
 */
void *
must_realloc (void * ptr, size_t amt)
{
  void * a;
  a = (void *)realloc (ptr, amt);
  if (amt && !a)
    panic ("out of memory");
  return a;
}


/*(c must_free)
 * void must_free (void * ptr);
 * 
 * Free a block allocated by `must_calloc', `must_malloc', or
 * `must_realloc'.
 */
void
must_free (void * ptr)
{
  if (ptr)
    free (ptr);
}


