/* unit-must-malloc.c - test must-malloc.c
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
#include "hackerlab/mem/mem.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-must-malloc";
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
      "Seed the random number generator with the number `n'.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



enum test_case
{
  malloc_test,
  strsav_test,
  strnsav_test,
  strcat_test,
#define allocation_test_max strcat_test

  realloc_test,
  free_test
#define test_max free_test
};


static unsigned int
rnd (int bound)
{
  return ((unsigned int)random ()) % bound;
}

static void
malloc_stress_test (int iter)
{
  t_uchar ** allocated;
  size_t * allocated_size;
  int pagesize;
  int small_allocation_range;
  int allocation_range;
  size_t amt_used;

  allocated = 0;
  allocated_size = 0;
  pagesize = getpagesize ();
  small_allocation_range = pagesize;
  allocation_range = 4 * pagesize;
  amt_used = 0;

  while (iter--)
    {
      int max;
      enum test_case test;

      if (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)))
	max = test_max;
      else
	max = allocation_test_max;

      if (amt_used >= 4194304)
	test = free_test;
      else
	test = rnd (max + 1);
      switch (test)
	{
	case malloc_test:
	  {
	    size_t amt;
	    t_uchar * mem;
	    
	    amt = (rnd (2)
		   ? rnd (small_allocation_range + 1)
		   : (small_allocation_range + rnd (allocation_range + 1)));
	    
	    mem = must_malloc (amt);
	    amt_used += amt;
	    *(t_uchar **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (t_uchar *)) = mem;
	    *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = amt;
	  }
	  break;


	case strsav_test:
	  {
	    t_uchar * mem;
	    

	    mem = str_save (lim_use_must_malloc, "hello world");
	    invariant (str_cmp ("hello world", mem) == 0);
	    amt_used += 12;
	    *(t_uchar **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (t_uchar *)) = mem;
	    *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = 12;
	  }
	  break;

	case strnsav_test:
	  {
	    t_uchar * mem;
	    

	    mem = str_save_n (lim_use_must_malloc, "hello world", 11);
	    invariant (str_cmp ("hello world", mem) == 0);
	    amt_used += 12;
	    *(t_uchar **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (t_uchar *)) = mem;
	    *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = 12;
	  }
	  break;

	case strcat_test:
	  {
	    t_uchar * mem;
	    

	    mem = str_alloc_cat (lim_use_must_malloc, "Hello ", "world!");
	    invariant (str_cmp ("Hello world!", mem) == 0);
	    amt_used += 13;
	    *(t_uchar **)ar_push ((void **)&allocated, lim_use_must_malloc, sizeof (t_uchar *)) = mem;
	    *(size_t *)ar_push ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t)) = 13;
	  }
	  break;

	case realloc_test:
	  {
	    int which;
	    size_t amt;
	    size_t old_amt;

	    which = rnd (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)));
	    old_amt = allocated_size[which];

	    amt = (rnd (2)
		   ? rnd (small_allocation_range + 1)
		   : (small_allocation_range + rnd (allocation_range + 1)));

	    amt_used -= old_amt;
	    amt_used += amt;
	    allocated[which] = must_realloc (allocated[which], amt);
	    allocated_size[which] = amt;
	  }
	  break;


	case free_test:
	  {
	    int which;

	    which = rnd (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)));
	    must_free (allocated[which]);
	    amt_used -= allocated_size[which];
	    mem_move ((t_uchar *)&allocated[which],
		      (t_uchar *)&allocated[which + 1],
		      sizeof (t_uchar *) * (ar_size ((void *)allocated, lim_use_must_malloc, sizeof (*allocated)) - (which + 1)));
	    ar_pop ((void **)&allocated, lim_use_must_malloc, sizeof (t_uchar *));
	    mem_move ((t_uchar *)&allocated_size[which],
		      (t_uchar *)&allocated_size[which + 1],
		      sizeof (size_t) * (ar_size ((void *)allocated_size, lim_use_must_malloc, sizeof (*allocated_size)) - (which + 1)));
	    ar_pop ((void **)&allocated_size, lim_use_must_malloc, sizeof (size_t));
	  }
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
	  }
	}
    }
  malloc_stress_test (iter);
  safe_printfmt (1, "%d iterations completed\n", iter);
  exit (0);
}
