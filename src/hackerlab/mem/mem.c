/* mem.c - array-of-byte functions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/mem/mem.h"


/************************************************************************
 *(h1 "Array-of-Byte Functions"
 * 	:includes ("hackerlab/mem/mem.h"))
 *
 * These functions operate on regions of memory as arrays of unsigned
 * bytes with no further interpretation.
 */


/*(c mem_set)
 * void mem_set (t_uchar * mem, unsigned int c, size_t size);
 * 
 * Store `size' bytes of value `c' beginning at `mem'.
 */
void
mem_set (t_uchar * mem, unsigned int c, size_t size)
{
  while (size--)
    *(mem++) = c;
}


/*(c mem_set0)
 * void mem_set0 (t_uchar * mem, size_t n);
 * 
 * Store `n' 0 bytes beginning at `mem'.
 */
void
mem_set0 (t_uchar * mem, size_t n)
{
  while (n--)
    *(mem++) = 0;
}


/*(c mem_move)
 * void mem_move (t_uchar * to, const t_uchar * from, size_t amt);
 * 
 * Copy `amt' bytes from `from' to `to'.  The source and destination
 * regions may overlap.
 */
void
mem_move (t_uchar * to, const t_uchar * from, size_t amt)
{
  if (from == to)
    return;
  if (from > to)
    {
      while (amt--)
	*to++ = *from++;
    }
  else
    {
      to += amt - 1;
      from += amt - 1;
      while (amt--)
	*to-- = *from--;
    }
}


/*(c mem_cmp)
 * size_t mem_cmp (const t_uchar * m1, const t_uchar * m2, size_t amt);
 * 
 * Compare `amt' bytes starting at `m1' and `m2'.  If a difference is
 * found, immediately return -1 if the differing byte in `m1' is less
 * than the byte in `m2', 1 otherwise.  If no difference is found,
 * return 0.
 */
size_t
mem_cmp (const t_uchar * m1, const t_uchar * m2, size_t amt)
{
  while (amt--)
    if (*m1++ != *m2++)
      {
	--m1;
	--m2;
	return (*m1 < *m2) ? -1 : 1;
      }
  return 0;
}


/*(c mem_occurrences)
 * size_t mem_occurrences (const t_uchar * mem,
 *                         unsigned int c,
 *                         size_t size);
 * 
 * Search `size' bytes of data for occurences of byte `c' beginning at
 * `mem'.  Return the number of occurences found.
 */
size_t
mem_occurrences (const t_uchar * mem,
		 unsigned int c,
		 size_t size)
{
  size_t x;
  x = 0;
  while (size--)
    if (*mem++ == c)
      ++x;
  return x;
}
