/* unexec-unsupported.c: unexec that never works
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "unexec/libunexec/unexec.h"



int
unexec (int * errn, char * new_name, char * old_name)
{
  *errn = EINVAL;
  return -1;
}


/* tag: Tom Lord Thu May  9 23:31:03 2002 (unexec-unsupported.c)
 */
