/* hash-utils.h - decls for computing hash values
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__HASH__HASH_UTILS_H
#define INCLUDE__HASH__HASH_UTILS_H


#include "hackerlab/machine/types.h"


/* automatically generated __STDC__ prototypes */
extern unsigned long hash_ul (unsigned long n);
extern unsigned long hash_pointers (void * elts, size_t n_elts);
extern unsigned long hash_mem (t_uchar * elts, size_t n_elts);
#endif  /* INCLUDE__HASH__HASH_UTILS_H */
