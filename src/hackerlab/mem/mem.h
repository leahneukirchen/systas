/* mem.h - array-of-byte decls
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__MEM__MEM_H
#define INCLUDE__MEM__MEM_H


#include "hackerlab/machine/types.h"


/* automatically generated __STDC__ prototypes */
extern void mem_set (t_uchar * mem, unsigned int c, size_t size);
extern void mem_set0 (t_uchar * mem, size_t n);
extern void mem_move (t_uchar * to, const t_uchar * from, size_t amt);
extern size_t mem_cmp (const t_uchar * m1, const t_uchar * m2, size_t amt);
extern size_t mem_occurrences (const t_uchar * mem, unsigned int c, size_t size);
#endif  /* INCLUDE__MEM__MEM_H */
