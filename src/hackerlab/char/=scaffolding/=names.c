/* tag: Tom Lord Tue Dec  4 14:41:26 2001 (=names.c)
 */
/* =names.c - generate a list of character names
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



/* This is handy for generating part of "chrname.c"
 *
 * cc -o =names =names.c ; ./=names ; rm =names
 *
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

int
main (int argc, char ** argv)
{
  int x;

  x = 0;
  while (x < 256)
    {

      if (x == '\\')
	fputs ("  \"\\\\\\\\\",", stdout);
      else if (x == '"')
	fputs ("  \"\\\\\\\"\",", stdout);
      else if (isprint (x))
	printf ("  \"%c\",", x);
      else
	{
	  switch (x)
	    {
	    default:
	      printf ("  \"\\\\0%o\",", x);
	      break;
	    case '\n':
	      fputs ("  \"\\\\n\",", stdout);
	      break;
	    case '\t':
	      fputs ("  \"\\\\t\",", stdout);
	      break;
	    case '\f':
	      fputs ("  \"\\\\f\",", stdout);
	      break;
	    }
	}
      ++x;
      if (!(x % 8))
	putchar ('\n');
    }
  putchar ('\n');
  return 0;
}
