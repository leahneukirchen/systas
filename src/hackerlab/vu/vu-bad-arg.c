/* vu-bad-arg.c - the EINVAL file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "hackerlab/vu/vu-bad-arg.h"


/************************************************************************
 *(h0 "A VU Handler that Always Returns an Error")
 * 
 */




/*(c vu_bad_arg)
 * int vu_bad_arg (int * errn, ...);
 * 
 * Return -1 and set `*errn' to `EINVAL'.
 * 
 * This function is useful when implementing VU handler vtables in
 * which not every VU function is supported.
 */
int
vu_bad_arg (int * errn, ...)
{
  *errn = EINVAL;
  return -1;
}


#define VU_FS_NAME_BAD_ARG(name, prefix, middle, suffix, ret, proto) ((ret(*)proto)vu_bad_arg),

struct vu_fs_discipline _vu_bad_arg_functions \
  = { VU_MAP_FS_NAMES (_BAD_ARG, , , , ) };

