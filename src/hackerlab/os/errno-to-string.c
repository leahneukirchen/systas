/* tag: Tom Lord Tue Dec  4 14:41:32 2001 (errno-to-string.c)
 */
/* errno-to-string.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <string.h>
#include "hackerlab/os/errno-to-string.h"



t_uchar *
errno_to_string (int errn)
{
  return strerror (errn);
}
