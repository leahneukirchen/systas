/* tag: Tom Lord Tue Dec  4 14:41:29 2001 (malloc.h)
 */
/* malloc.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__OS__MALLOC_H
#define INCLUDE__OS__MALLOC_H


/* An alternative to <stdlib.h> for declaring `malloc' and
 * friends.
 * 
 * Most hackerlab code should not use `malloc' directly.
 * Instead, use "hackerlab/mem/alloc-limits.h"
 */

#include "hackerlab/machine/types.h"

extern void * calloc (size_t nmemb, size_t size);
extern void free (void * ptr);
extern void * malloc (size_t size);
extern void * realloc (void * ptr, size_t size);


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__OS__MALLOC_H */
