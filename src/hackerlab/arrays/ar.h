/* ar.h - decls for variable size arrays
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__ARRAYS__AR_H
#define INCLUDE__ARRAYS__AR_H

#include "hackerlab/machine/types.h"
#include "hackerlab/mem/alloc-limits.h"


/* automatically generated __STDC__ prototypes */
extern int ar_size (void * base, alloc_limits limits, size_t elt_size);
extern void * ar_ref (void ** base,
		      alloc_limits limits,
		      int n,
		      int szof);
extern void ar_setsize (void ** base,
		      alloc_limits limits,
		      int n,
		      size_t szof);
extern void ar_compact (void ** base,
		      alloc_limits limits,
		      size_t szof);
extern void ar_free (void ** base, alloc_limits limits);
extern void * ar_push (void ** base,
		     alloc_limits limits,
		     size_t szof);
extern void * ar_pop (void ** base,
		    alloc_limits limits,
		    size_t szof);
extern void * ar_copy (void * base,
		     alloc_limits limits,
		     size_t szof);
#endif  /* INCLUDE__ARRAYS__AR_H */
