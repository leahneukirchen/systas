/* test-rx.c - Correctness tests for Rx
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/cmd/main.h"
#include "hackerlab/tests/rx-posix-tests/test-rx.h"
#include "hackerlab/tests/rx-posix-tests/test-decls.h"



static t_uchar * program_name = "test-rx";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_random, "r", "random", 0, \
      "Run the tests in random order.") \
  OP (opt_iterations, "i", "iterations=n", 1, \
      "Iterate N times.") \
  OP (opt_repeat, 0, "repeat=n", 1, \
      "Repeat each test N times per iteration.") \
  OP (opt_number, "n", "number=n", 1, \
      "Run only test #N.") \
  OP (opt_verbose, "v", "verbose", 0, \
      "Run tests verbosely.") \
  OP (opt_quiet, "q", "quiet", 0, \
      "Produce no (ordinary) output.") \
  OP (opt_dfa_cache_threshold, "D", "dfa-cache-threshold=N", 1, \
      "Set the DFA cache GC threshold.") \
  OP (opt_nfa_cache_threshold, "N", "nfa-cache-threshold=N", 1, \
      "Set the NFA cache GC threshold.")


enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



static int exit_status = 0;

static void
test_failed (struct rx_test * test)
{
  safe_printfmt (2, "FAILED: %s\n pattern: %s\n string: %s\n", test->name, test->pattern, test->string);
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

      regerror (comp_result, &preg, errbuf, sizeof (errbuf));
      test_failed (test);
      safe_printfmt (2, " Expected regcomp to return %d, actually returned %d (%s)\n", test->compile_error, comp_result, errbuf);
      if (!comp_result)
	regfree (&preg);
      return;
    }

  if (comp_result)
    return;

  exec_result = regexec (&preg, test->string, test->n_match, pmatch, test->eflags);
  ++n_execs;

  if (!exec_result != test->is_match)
    {
      test_failed (test);
      safe_printfmt (2, " Expected regexec to return %d, actually returned %d\n", !test->is_match, exec_result);
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
	    test_failed (test);
	    safe_printfmt (2, " Expected pmatch[%d] to be { rm_so == %d, rm_eo == %d } but got {%d, %d}.\n",
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
  int errn;
  int x;
  int noisy;
  int quiet;
  int repeats;
  int iterations;
  int random_order;
  int number;
  unsigned long cache_size;
  int o;
  struct opt_parsed * option;

  noisy = 0;
  quiet = 0;
  iterations = 1;
  repeats = 1;
  random_order = 0;
  number = -1;
  option = 0;

  while (1)
    {
      o = opt_standard (lim_use_must_malloc, &option, opts, &argc, argv, program_name, usage, version_string, 0, opt_help_msg, opt_none, opt_version);
      if (o == opt_none)
	break;
      switch (o)
	{
	default:
	  safe_printfmt (2, "unhandled option `%s'\n", option->opt_string);
	  panic ("internal error parsing arguments");

	usage_error:
	  opt_usage (2, argv[0], program_name, usage, 1);
	  panic_exit ();

	bogus_arg:
	  safe_printfmt (2, "ill-formed argument for `%s' (`%s')\n", option->opt_string, option->arg_string);
	  goto usage_error;

	case opt_random:
	  random_order = 1;
	  break;

	case opt_iterations:
	  if (cvt_decimal_to_uint (&errn, &iterations, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_repeat:
	  if (cvt_decimal_to_uint (&errn, &repeats, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_number:
	  if (cvt_decimal_to_int (&errn, &number, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;

	  if (number >= ((sizeof (rx_tests) / sizeof (rx_tests[0])) - 1))
	    {
	      safe_printfmt (2, "test number out of range\n");
	      exit (1);
	    }
	  break;

	case opt_dfa_cache_threshold:
	  if (cvt_decimal_to_ulong (&errn, &cache_size, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;

	  rx_set_dfa_cache_threshold ((size_t)cache_size);
	  break;

	case opt_nfa_cache_threshold:
	  if (cvt_decimal_to_ulong (&errn, &cache_size, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;

	  rx_set_nfa_cache_threshold ((size_t)cache_size);
	  break;

	case opt_verbose:
	  noisy = 1;
	  break;

	case opt_quiet:
	  quiet = 1;
	  break;
	}
    }

  if (noisy && quiet)
    {
      safe_printfmt (2, "Can not be both verbose and quiet\n");
      goto usage_error;
    }


  while (iterations--)
    {
      int r;

      if (number >= 0)
	{
	  if (noisy)
	    safe_printfmt (1, "%d %s\n", number, rx_tests[number].name);
	  for (r = 0; r < repeats; ++r)
	    {
	      if (noisy && (repeats > 1))
		safe_printfmt (1, "...%d", r);
	      run_a_test (&rx_tests[number]);
	    }
	  if (noisy && (repeats > 1))
	    safe_printfmt (1, "\n");
	}
      else
	{
	  for (x = 0; rx_tests[x].pattern; ++x)
	    {
	      int y;
	      if (random_order)
		y = random () % ((sizeof (rx_tests) / sizeof (rx_tests[0])) - 1);
	      else
		y = x;
	      if (noisy)
		safe_printfmt (1, "%d %s\n", y, rx_tests[y].name);
	      for (r = 0; r < repeats; ++r)
		{
		  if (noisy && (repeats > 1))
		    safe_printfmt (1, "...%d", r);
		  run_a_test (&rx_tests[y]);
		}
	      if (noisy && (repeats > 1))
		safe_printfmt (1, "\n");
	    }
	}
    }


  if (!quiet)
    {
      size_t threshold;
      size_t failure_pt;
      size_t in_use;
      size_t high_water_mark;
      int dfa_hits;
      int dfa_misses;
      int dfa_total_hits;
      int dfa_total_misses;

      rx_dfa_cache_statistics (&threshold, &failure_pt, &in_use, &high_water_mark, &dfa_hits, &dfa_misses, &dfa_total_hits, &dfa_total_misses);
      safe_printfmt (1, "dfa cache stats:\n   threshold %lu; failure_pt %lu\n   in_use %lu; high_water_mark %lu\n   hits %d; misses %d; total_hits %d; total_misses %d\n",
		     (unsigned long)threshold,
		     (unsigned long)failure_pt,
		     (unsigned long)in_use,
		     (unsigned long)high_water_mark,
		     dfa_hits, dfa_misses, dfa_total_hits, dfa_total_misses);
    }

  if (!quiet)
    {
      size_t threshold;
      size_t failure_pt;
      size_t in_use;
      size_t high_water_mark;
      int nfa_hits;
      int nfa_misses;
      int nfa_saves;

      rx_nfa_cache_statistics (&threshold, &failure_pt, &in_use, &high_water_mark, &nfa_hits, &nfa_misses, &nfa_saves);
      safe_printfmt (1, "nfa cache stats:\n   threshold %lu; failure_pt %lu\n   in_use %lu; high_water_mark %lu\n   hits %d; misses %d; saves %d\n",
		     (unsigned long)threshold,
		     (unsigned long)failure_pt,
		     (unsigned long)in_use,
		     (unsigned long)high_water_mark,
		     nfa_hits, nfa_misses, nfa_saves);
    }

  {
    size_t dfa_bytes;
    size_t nfa_bytes;

    dfa_bytes = rx_flush_dfa_cache ();
    nfa_bytes = rx_flush_nfa_cache ();

    if (!quiet || dfa_bytes || nfa_bytes)
      {
	safe_printfmt (1, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);
	safe_printfmt (1, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);
      }

    if (dfa_bytes || nfa_bytes)
      exit_status = 1;
  }

  if (!quiet)
    {
      safe_printfmt (1, "%d regcomps, %d regexecs\n", n_comps, n_execs);
      safe_printfmt (1, "Posix and Rx extension validation tests %s\n", exit_status ? "failed" : "passed");
    }
  exit (exit_status);
}
