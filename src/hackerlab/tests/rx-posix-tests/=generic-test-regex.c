/* tag: Tom Lord Tue Dec  4 14:41:09 2001 (=generic-test-regex.c)
 */
/* =generic-test-regex.c - Correctness tests for Rx
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include <sys/types.h>
#include <regex.h>
#include <stdio.h>

struct regmatch_answer
{
  regoff_t rm_so;
  regoff_t rm_eo;
};

struct rx_test
{
  unsigned char * name;
  unsigned char * pattern;
  int cflags;
  int compile_error;

  unsigned char * string;
  int eflags;

  int is_match;
  size_t n_match;
  struct regmatch_answer pmatch[11];
};

#include "posix-test-cases.h"



static int exit_status = 0;

static void
test_failed (struct rx_test * test, int warning_only)
{
  fprintf (stderr, "%s: %s\n pattern: %s\n string: %s\n", (warning_only ? "WARNING" : "FAILED"), test->name, test->pattern, test->string);
  exit_status = 1;
}

static int n_comps = 0;
static int n_execs = 0;

static void
run_a_test (struct rx_test * test)
{
  regex_t preg;
  int comp_result;
  int exec_result;
  regmatch_t pmatch[10];

  comp_result = regcomp (&preg, test->pattern, test->cflags);
  ++n_comps;

  if (comp_result != test->compile_error)
    {
      char errbuf[1000];
      int warning;

      warning = ((!comp_result) == (!test->compile_error));
      regerror (comp_result, &preg, errbuf, sizeof (errbuf));
      test_failed (test, warning);
      fprintf (stderr, " Expected regcomp to return %d, actually returned %d (%s)\n", test->compile_error, comp_result, errbuf);
      if (!warning)
	{
	  if (!comp_result)
	    regfree (&preg);
	  return;
	}
    }

  if (comp_result)
    return;

  exec_result = regexec (&preg, test->string, test->n_match, pmatch, test->eflags);
  ++n_execs;

  if (!exec_result != test->is_match)
    {
      test_failed (test, 0);
      fprintf (stderr, " Expected regexec to return %d, actually returned %d\n", !test->is_match, exec_result);
      regfree (&preg);
      return;
    }

  {
    int x;

    for (x = 0; x < test->n_match; ++x)
      {
	if (   (pmatch[x].rm_so != test->pmatch[x].rm_so)
	    || (pmatch[x].rm_eo != test->pmatch[x].rm_eo))
	  {
	    test_failed (test, 0);
	    fprintf (stderr, " Expected pmatch[%d] to be { rm_so == %d, rm_eo == %d } but got {%d, %d}.\n",
		     x,
		     (int)test->pmatch[x].rm_so,
		     (int)test->pmatch[x].rm_eo,
		     (int)pmatch[x].rm_so,
		     (int)pmatch[x].rm_eo);
	    regfree (&preg);
	    return;
	  }
      }
  }
  regfree (&preg);
  return;
}




int
main (int argc, char * argv[])
{
  int x;

  for (x = 0; rx_tests[x].pattern; ++x)
    {
      printf ("%d %s\n", x, rx_tests[x].name);
      run_a_test (&rx_tests[x]);
    }
  exit (exit_status);
}
