/* tag: Tom Lord Tue Dec  4 14:41:41 2001 (safe-vu-utils-vfdbuf.c)
 */
/* safe-vu-utils-vfdbuf.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/safe-vu-utils-vfdbuf.h"




void
safe_next_line (t_uchar ** line, long * len, int fd)
{
  int errn;
  int status;

 retry:
  status = vfdbuf_next_line (&errn, line, len, fd);
  if (status < 0)
    {
      if (errn == EAGAIN)
	goto retry;
      printfmt (&errn, 2, "Error during call to `vfdbuf_next_line'\n");
      printfmt (&errn, 2, "  error %d: %s\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}
