/* unit-alloc-limits.c - test alloc-limits.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/bugs/test-coverage.h"
#include "hackerlab/char/str.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-alloc-limits";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_iterations, "i", "iterations=n", 1, \
      "Perform `n' test operations.") \
  OP (opt_reseed, "r", "reseed", 0, \
      "Reseed the random number generator from the time of day.") \
  OP2(opt_reseed, 0, 0, 0, \
      "Print the new seed value.") \
  OP (opt_seed, "s", "seed=n", 1, \
      "Seed the random number generator with the number `n'.") \
  OP (opt_threshold, "t", "threshold=n", 1, \
      "Set the GC threshold.") \
  OP (opt_failure_pt, "f", "failure-point=n", 1, \
      "Set the allocation failure point.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



static unsigned int
rnd (int bound)
{
  return ((unsigned int)random ()) % bound;
}



static t_uchar ** allocated = 0;
static size_t * allocated_size = 0;

static t_uchar * aside = 0;		/* memory being realloced -- should not be GCed */
static size_t aside_size = 0;

static size_t threshold = 4096;
static size_t failure_pt = 8192;

static size_t amt_used = 0;

static alloc_limits limits;

#define TEST_COVERAGE_LIST(MACRO) \
  TEST_COVERAGE_ ## MACRO (alloc_gc); \
  TEST_COVERAGE_ ## MACRO (alloc_no_gc); \
  TEST_COVERAGE_ ## MACRO (alloc_fail); \
  TEST_COVERAGE_ ## MACRO (alloc_no_fail); \
  TEST_COVERAGE_ ## MACRO (realloc_gc); \
  TEST_COVERAGE_ ## MACRO (realloc_no_gc); \
  TEST_COVERAGE_ ## MACRO (realloc_fail); \
  TEST_COVERAGE_ ## MACRO (realloc_no_fail)

TEST_COVERAGE_LIST(DECL);



static void
check_test_engine (void)
{
  int x;
  size_t accounted;

  accounted = 0;
  for (x = ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - 1; x >= 0; --x)
    accounted += allocated_size[x];

  accounted += aside_size;

  if (accounted != amt_used)
    panic ("test engine error");
  
}



static void
free_memory (int pos)
{
  check_test_engine ();

  if ((pos < 0) || (pos >= ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated))))
    panic ("bad pos in free_memory");

  lim_free (limits, allocated[pos]);
  amt_used -= allocated_size[pos];
  allocated[pos] = allocated[ar_size((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - 1];
  allocated_size[pos] = allocated_size[ar_size((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - 1];
  ar_pop ((void **)&allocated, lim_use_must_malloc, sizeof (void *));
  ar_pop ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t));
  check_test_engine ();
}



static size_t want;
static int free_memory_fn_expected;
static int free_memory_fn_called;
static int can_free_memory;

static void
free_memory_fn (void * closure, size_t amt)
{
  if (!free_memory_fn_expected)
    panic ("unexpected call to free_memory_fn");

  free_memory_fn_called = 1;

  if (closure != (void *)0xac1dc0de)
    panic ("bad closure in free_memory_fn");

  if (want != amt)
    panic ("bad amt passed to free_memory_fn");

  if (can_free_memory)
    while (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) && (lim_in_use (limits) + amt >= lim_threshold (limits)))
      free_memory (0);
}




static void
alloc_test (int force_gc, int force_fail)
{
  void * answer;

  if (force_gc)
    {
      if (amt_used < threshold)
	want = threshold - amt_used + 1;
      else
	want = 1;
    }
  else
    {
      want = rnd (threshold - sizeof (size_t));
      if (want == 0)
	want = 1;
    }

  if (force_fail)
    can_free_memory = 0;
  else
    can_free_memory = rnd (2);

  free_memory_fn_expected = ((want + amt_used) >= threshold);
  free_memory_fn_called = 0;

  if (free_memory_fn_expected)
    TEST_COVERED (alloc_gc);
  else
    TEST_COVERED (alloc_no_gc);

  answer = lim_malloc (limits, want);

  if (free_memory_fn_expected != free_memory_fn_expected)
    panic ("free_memory_fn_expected != free_memory_fn_called");

  if (!answer)
    {
      TEST_COVERED (alloc_fail);
      if (!lim_failure_pt (limits) || (want + lim_in_use (limits) < lim_failure_pt (limits)))
	panic ("allocation below failure point failed");
      return;
    }
  else
    {
      TEST_COVERED (alloc_no_fail);
      if (lim_failure_pt (limits) && (lim_in_use (limits) > lim_failure_pt (limits)))
	panic ("allocation succeeded above failure point");
    }

  *(void **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (void *)) = answer;
  *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = want;
  amt_used += want;
}


static void
realloc_test (int force_gc, int force_fail)
{
  int pos;
  size_t will_be;
  void * answer;

  while (!ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)))
    alloc_test (0, 0);
  
  pos = rnd (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)));

  aside = allocated[pos];
  aside_size = allocated_size[pos];

  allocated[pos] = allocated[ar_size((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - 1];
  allocated_size[pos] = allocated_size[ar_size((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - 1];
  ar_pop ((void **)&allocated, lim_use_must_malloc, sizeof (void *));
  ar_pop ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t));


  if (force_gc)
    {
      if (amt_used < threshold)
	will_be = aside_size + (threshold - amt_used) + 1;
      else
	will_be = aside_size;
    }
  else
    {
      will_be = rnd (threshold);
      if (will_be == 0)
	will_be = 1;
    }

  if (force_fail)
    can_free_memory = 0;
  else
    can_free_memory = rnd (2);
  
  free_memory_fn_expected = ((will_be - aside_size + amt_used) >= threshold);
  free_memory_fn_called = 0;

  if (free_memory_fn_expected)
    TEST_COVERED (realloc_gc);
  else
    TEST_COVERED (realloc_no_gc);

  want = will_be - aside_size;
  answer = lim_realloc (limits, aside, will_be);

  if (free_memory_fn_expected != free_memory_fn_expected)
    panic ("free_memory_fn_expected != free_memory_fn_called");

  if (!answer)
    {
      TEST_COVERED (realloc_fail);
      if (!lim_failure_pt (limits) || (will_be < aside_size) || (want + lim_in_use (limits) < lim_failure_pt (limits)))
	panic ("allocation below failure point failed");

      *(void **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (void *)) = aside;
      *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = aside_size;
      aside = 0;
      aside_size = 0;
      return;
    }
  else
    {
      TEST_COVERED (realloc_no_fail);
      if (lim_failure_pt (limits) && (lim_in_use (limits) > lim_failure_pt (limits)))
	panic ("allocation succeeded above failure point");
    }

  *(void **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (void *)) = answer;
  *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = will_be;
  amt_used += (will_be - aside_size);
  aside = 0;
  aside_size = 0;
}


static void
free_test ()
{
  while (!amt_used)
    alloc_test (0, 0);
  
  free_memory (0);
}




static void
init_test (void)
{
  limits = make_alloc_limits ("test limits",
			      threshold,
			      failure_pt,
			      0,
			      free_memory_fn,
			      (void *)0xac1dc0de);
}




static void
run_tests (int iter)
{
  enum test_cases
    {
      test_alloc,
      test_realloc,
      test_free,
      n_tests
    };


  init_test ();

  while (iter--)
    {
      check_test_engine ();

      switch (rnd (n_tests))
	{
	default:
	  panic ("bogus random number");

	case test_alloc:
	  alloc_test (0, 0);
	  break;
	  
	case test_realloc:
	  realloc_test (0, 0);
	  break;
	  
	case test_free:
	  free_test ();
	  break;
	}
    }
}



int
main (int argc, char * argv[])
{
  int errn;
  int o;
  struct opt_parsed * option;
  unsigned int iter;

  option = 0;
  iter = 10000;
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

	case opt_iterations:
	  if (cvt_decimal_to_uint (&errn, &iter, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_reseed:
	  {
	    unsigned long t;

	    t = time(0);
	    printfmt (&errn, 1, "RNG seed == %lu\n", t);
	    srandom (t);
	  }
	  break;

	case opt_seed:
	  {
	    unsigned long t;

	    if (cvt_decimal_to_ulong (&errn, &t, option->arg_string, str_length (option->arg_string)))
	      goto bogus_arg;
	    printfmt (&errn, 1, "RNG seed == %lu\n", t);
	    srandom (t);
	    break;
	  }


	case opt_threshold:
	  {
	    unsigned long t;

	    if (cvt_decimal_to_ulong (&errn, &t, option->arg_string, str_length (option->arg_string)))
	      goto bogus_arg;
	    threshold = (size_t) t;
	    break;
	  }

	case opt_failure_pt:
	  {
	    unsigned long t;

	    if (cvt_decimal_to_ulong (&errn, &t, option->arg_string, str_length (option->arg_string)))
	      goto bogus_arg;
	    failure_pt = (size_t) t;
	    break;
	  }

	}
    }
  run_tests (iter);
  alloc_test (1, 0);
  alloc_test (1, 1);
  realloc_test (1, 0);
  realloc_test (1, 1);
  TEST_COVERAGE_LIST(CHECK);
  safe_printfmt (1, "%d iterations completed\n", iter);
  exit (0);
}


