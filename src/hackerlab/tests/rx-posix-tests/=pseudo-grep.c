/* tag: Tom Lord Tue Dec  4 14:41:11 2001 (=pseudo-grep.c)
 */
/* =pseudo-grep.c - a pseudo-grep program for testing
 *
 ****************************************************************
 * Copyright (C) 2001 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#ifdef RX
#include "hackerlab/rx-posix/regex.h"
#else
#include <regex.h>
#endif

int
main (int argc, char * argv[])
{
  char str[1024];
  regex_t re;
  regmatch_t pos[1];

  if (argc != 2)
    {
      fprintf (stderr, "usage: pseudo-grep regexp\n");
      exit (1);
    }

  if (regcomp (&re, argv[1], REG_EXTENDED))
    {
      fprintf (stderr, "regcomp error\n");
      exit (1);
    }

  while (fgets (str, sizeof (str), stdin))
    {
      int stat;
      int len;
      
      len = strlen (str);
      if (len && (str[len - 1] == '\n'))
	str[len - 1] = 0;

      stat = regexec (&re, str, 1, pos, 0);
      if (stat == REG_NOMATCH)
	continue;
      if (stat)
	{
	  fprintf (stderr, "regexec error\n");
	  exit (1);
	}
      printf ("%.*s{{{%.*s}}}%.*s\n",
	      (int)(pos[0].rm_so), str, (int)(pos[0].rm_eo - pos[0].rm_so), str + pos[0].rm_so, (int)(strlen (str) - pos[0].rm_eo), str + pos[0].rm_eo);
    }
  if (!feof (stdin))
    {
      fprintf (stderr, "fgets error\n");
      exit (1);
    }
  return 0;
}
