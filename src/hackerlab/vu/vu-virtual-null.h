/* vu-virtual-null.h - decls for the virtual-null file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__VU_VIRTUAL_NULL_H
#define INCLUDE__VU__VU_VIRTUAL_NULL_H


#include "hackerlab/vu/vu.h"



/****************************************************************
 * vu_virtual_null_vtable
 *
 * An in-process emulation of /dev/null.
 *
 */
extern struct vu_fs_discipline vu_virtual_null_vtable;


/* automatically generated __STDC__ prototypes */
extern int vu_make_virtual_null_fd (int * errn, int flags);
extern int vu_make_virtual_null_fd_ge_n (int * errn, int n, int flags);
#endif  /* INCLUDE__VU__VU_VIRTUAL_NULL_H */
