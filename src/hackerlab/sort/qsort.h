/* qsort.h:
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__SORT__QSORT_H
#define INCLUDE__SORT__QSORT_H


#include "hackerlab/os/sys/types.h"

typedef int (*quicksort_cmp) (void * a, void * b, void * closure);



/* automatically generated __STDC__ prototypes */
extern void quicksort (void * base,
		       size_t n_elts,
		       size_t sizeof_elt,
		       quicksort_cmp cmp,
		       void * closure);
#endif  /* INCLUDE__SORT__QSORT_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (qsort.h)
 */
