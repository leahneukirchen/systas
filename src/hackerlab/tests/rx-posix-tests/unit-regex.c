/* unit-regex.c - A test of posix declarations.
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



/****************************************************************
 * This program does no useful computation.  
 * 
 * 
 * It does contain `#include' directives and declarations that can be
 * used to (partially) verify posix conformance.  If this program
 * compiles correctly, the regexp declarations required by Posix
 * appear to be in order.
 * 
 */





/****************************************************************
 * A native implementation would use:
 * 
 * 	#include <sys/types.h>
 * 	#include <limits.h>
 * 	#include <regex.h>
 * 
 * to obtain all of the regexp declarations plus RE_DUP_MAX.
 * 
 * As a non-native implementation, Rx uses:
 * 
 */

#include <sys/types.h>
#include <limits.h>
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/rx-posix/limits.h"

extern int regcomp (regex_t *preg, const char *pattern, int cflags);
extern int regexec (const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern size_t regerror (int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
extern void regfree (regex_t *preg);

int
main (int argc, char * argv[])
{
  regex_t a_regex;
  regmatch_t a_regmatch;
  int cflags;
  int eflags;
  int ret_val;
  int dup_max;

  /* Required fields:
   */
  a_regex.re_nsub = 1;
  a_regmatch.rm_so = 1;
  a_regmatch.rm_eo = 1;


  /* flags:
   */
  cflags = (REG_EXTENDED | REG_ICASE | REG_NOSUB | REG_NEWLINE);
  eflags = (REG_NOTBOL | REG_NOTEOL);

  /* Return values:
   */
  ret_val = REG_NOMATCH;
  ret_val = REG_BADPAT;
  ret_val = REG_ECOLLATE;
  ret_val = REG_ECTYPE;
  ret_val = REG_EESCAPE;
  ret_val = REG_ESUBREG;
  ret_val = REG_EBRACK;
  ret_val = REG_EPAREN;
  ret_val = REG_EBRACE;
  ret_val = REG_BADBR;
  ret_val = REG_ERANGE;
  ret_val = REG_ESPACE;
  ret_val = REG_BADRPT;

  dup_max = RE_DUP_MAX;

  exit (0);
}


