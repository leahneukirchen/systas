/* tag: Tom Lord Tue Dec  4 14:41:36 2001 (=shuffle2-8.c)
 */
/* =shuffle2-8.c - more scaffolding for hash routines
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



/* This is handy for generating the initializer for
 * "static int shuffled_bytes[] = "  in "hashtree.c".
 *
 * cc -o =shuffle2-8 ./=shuffle2-8.c ; ./=shuffle2-8 ; rm =shuffle2-8
 *
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

int ar [8] = 
{
  0, 1, 2, 3, 4, 5, 6, 7
};

int
main (int argc, char ** argv)
{
  int x;

  srandom (time (0));

  for (x = sizeof (ar) / sizeof (ar[0]); x; --x)
    {
      int i;

      i = random () % x;
      printf ("%d\n", ar[i]);
      ar[i] = ar[x - 1];
    }
  return 0;
}
