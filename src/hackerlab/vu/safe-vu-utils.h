/* tag: Tom Lord Tue Dec  4 14:41:41 2001 (safe-vu-utils.h)
 */
/* safe-vu-utils.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__VU__SAFE_VU_UTILS_H
#define INCLUDE__VU__SAFE_VU_UTILS_H


#include "hackerlab/vu/vu-utils.h"
#include "hackerlab/vu/safe-vu-utils-vfdbuf.h"


/* automatically generated __STDC__ prototypes */
extern int safe_move_fd (int fd, int newfd);
extern void safe_file_to_string (t_uchar ** buf, size_t * len, int fd);
extern int safe_file_is_directory (t_uchar * name);
#endif  /* INCLUDE__VU__SAFE_VU_UTILS_H */
