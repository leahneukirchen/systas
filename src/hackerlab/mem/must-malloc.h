/* must-malloc.h - decls for malloc or die
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__MEM__MUST_MALLOC_H
#define INCLUDE__MEM__MUST_MALLOC_H


#include "hackerlab/machine/types.h"



/* automatically generated __STDC__ prototypes */
extern void * must_calloc (size_t amt);
extern void * must_malloc (size_t amt);
extern void * must_realloc (void * ptr, size_t amt);
extern void must_free (void * ptr);
#endif  /* INCLUDE__MEM__MUST_MALLOC_H */
