/* regex-utils.c: 
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/rx-posix/regex-utils.h"



int
regexec_comp (regex_t * preg,
	      char * pattern,
	      int cflags,
	      char * string,
	      size_t nmatch,
	      regmatch_t pmatch[],
	      int eflags)
{
  return regnexec_ncomp (preg, pattern, str_length (pattern), cflags, string, str_length (string), nmatch, pmatch, eflags);
}

int
regnexec_comp (regex_t * preg,
	      char * pattern,
	      int cflags,
	      char * string,
	      size_t string_len,
	      size_t nmatch,
	      regmatch_t pmatch[],
	      int eflags)
{
  return regnexec_ncomp (preg, pattern, str_length (pattern), cflags, string, string_len, nmatch, pmatch, eflags);
}

int
regexec_ncomp (regex_t * preg,
	       char * pattern,
	       size_t pattern_len,
	       int cflags,
	       char * string,
	       size_t nmatch,
	       regmatch_t pmatch[],
	       int eflags)
{
  return regnexec_ncomp (preg, pattern, pattern_len, cflags, string, str_length (string), nmatch, pmatch, eflags);
}


int
regnexec_ncomp (regex_t * preg,
		char * pattern,
		size_t pattern_len,
		int cflags,
		char * string,
		size_t string_len,
		size_t nmatch,
		regmatch_t pmatch[],
		int eflags)
{
  if (
}




/* tag: Tom Lord Sun Mar 10 22:01:18 2002 (regex-utils.c)
 */
