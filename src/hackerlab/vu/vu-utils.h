/* vu-utils.h - decls for vu helper functions
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__VU_UTILS_H
#define INCLUDE__VU__VU_UTILS_H


#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/vu-utils-vfdbuf.h"



/* automatically generated __STDC__ prototypes */
extern int vu_file_to_string (int * errn,
			      t_uchar ** buf,
			      size_t * len,
			      int fd);
extern int vu_move_fd (int * errn, int fd, int newfd);
extern int vu_file_is_directory (int * errn, t_uchar * name);
#endif  /* INCLUDE__VU__VU_UTILS_H */
