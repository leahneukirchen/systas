/* tag: Tom Lord Tue Dec  4 14:41:40 2001 (safe-printfmt.c)
 */
/* safe-printfmt.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/safe-printfmt.h"



/*(c safe_printfmt)
 * void safe_printfmt (int fd, char * fmt, ...);
 * 
 * See xref:"printfmt".
 */
void
safe_printfmt (int fd, char * fmt, ...)
{
  int errn;
  int answer;
  va_list ap;

  va_start (ap, fmt);
  answer = printfmt_va_list (&errn, fd, fmt, ap);
  va_end (ap);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error in `printfmt' (%s)\n", errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_printfmt_va_list (int fd, char * fmt, va_list ap)
{
  int errn;
  int answer;

  answer = printfmt_va_list (&errn, fd, fmt, ap);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error in `printfmt_va_list' (%s)\n", errno_to_string (errn));
      panic ("I/O error");
    }
}

