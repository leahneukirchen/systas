/* printfmt.c - formatted printing
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/vu/printfmt-va.h"
#include "hackerlab/vu/printfmt.h"

/************************************************************************
 *(h0 "Formatted Printing With printfmt")
 *
 * The function `printfmt' provides the functionality of the standard
 * function `printf' within the context of the *VU* file system
 * interface.
 */




/*(c printfmt)
 * int printfmt (int * errn, int fd, char * fmt, ...);
 * 
 * Print formatted output on descriptor `fd'.
 *
 * [See the man page for `printf(3)' for a description for format
 *  strings.]
 *
 * Extensions:
 *
 *	The formatting flag '&' means to read the padding character
 *	from the next unused argument.  ' ' is the default padding
 *	character.
 * 
 * \WARNING: The implementation of this function is incomplete.
 * Floating point values are not handled yet, for example./
 * 
 * \WARNING: Compilers do not type check the arguments to this
 * function, so it is easy to make mistakes.  A type-safe
 * interface for formatted printing is needed for a future
 * release./
 */
int
printfmt (int * errn, int fd, char * fmt, ...)
{
  int answer;
  va_list ap;

  va_start (ap, fmt);
  answer = printfmt_va_list (errn, fd, (t_uchar *)fmt, ap);
  va_end (ap);
  return answer;
}


