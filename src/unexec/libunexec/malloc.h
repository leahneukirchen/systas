/* malloc.h:
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__LIBUNEXEC__MALLOC_H
#define INCLUDE__LIBUNEXEC__MALLOC_H

extern void * _malloc (size_t size);
extern void _free (void * ptr);
extern void * _realloc (void * ptr, size_t size);


/* automatically generated __STDC__ prototypes */
extern void * malloc (size_t size);
extern void free (void * ptr);
extern void cfree (void * ptr);
extern void * realloc (void * ptr, size_t size);
extern void * calloc (size_t nmemb, size_t size);
extern void * memalign (size_t alignment, size_t size);
extern void * valloc (size_t size);
#endif  /* INCLUDE__LIBUNEXEC__MALLOC_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (malloc.h)
 */
