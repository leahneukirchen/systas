/* tag: Tom Lord Tue Dec  4 14:41:52 2001 (pow2-array-compact.h)
 */
/* pow2-array-compact.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__ARRAYS__POW2_ARRAY_COMPACT_H
#define INCLUDE__ARRAYS__POW2_ARRAY_COMPACT_H


#include "pow2-array.h"


/* automatically generated __STDC__ prototypes */
extern void pow2_array_compact (pow2_array array,
				t_ulong (*hash_elts) (void * elts, size_t n_elts),
				int (*compare_elts) (void * a, void * b, size_t n_elts),
				void (*free_elts) (void * elts, size_t n_elts));
#endif  /* INCLUDE__ARRAYS__POW2_ARRAY_COMPACT_H */
