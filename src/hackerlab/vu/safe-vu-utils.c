/* tag: Tom Lord Tue Dec  4 14:41:40 2001 (safe-vu-utils.c)
 */
/* safe-vu-utils.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/safe-vu-utils.h"




/*(c safe_move_fd)
 * int safe_move_fd (int fd, int newfd);
 * 
 * See xref:"move_fd".
 */
int
safe_move_fd (int fd, int newfd)
{
  int errn;
  int answer;

  answer = vu_move_fd (&errn, fd, newfd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_move_fd' to move descriptor %d to descriptor %d (%s)\n", fd, newfd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}



void
safe_file_to_string (t_uchar ** buf, size_t * len, int fd)
{
  int errn;
  int answer;

  answer = vu_file_to_string (&errn, buf, len, fd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_file_to_string' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
}


int
safe_file_is_directory (t_uchar * name)
{
  int errn;
  int answer;

  answer = vu_file_is_directory (&errn, name);

  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_file_is_directory' (%s)\n", errno_to_string (errn));
      printfmt (&errn, 2, "  file %s\n", name);
      panic ("I/O error");
    }
  return answer;
}
