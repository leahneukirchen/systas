/* reserv.h - decls for reserved and pseudo file descriptors
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__RESERV_H
#define INCLUDE__VU__RESERV_H


#include "hackerlab/vu/vu.h"


/* automatically generated __STDC__ prototypes */
extern int reserv (int * errn, int flags);
extern int reserv_pseudo (int * errn, int flags);
extern int reserv_pseudo_ge_n (int * errn, int n, int flags);
extern void unreserv_pseudo (int fd);
#endif  /* INCLUDE__VU__RESERV_H */
