/* unit-ar.c - test ar.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/machine/alignment.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-ar";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_iterations, "i", "iterations n", 1,  \
      "Perform <n> tests.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



#define HASH_INDEX(X) ((((X) >> 8) & 0xff) ^ ((X) & 0xff))

static void
check_array (size_t size, unsigned long * ar)
{
  int x;
  invariant (0 == ((unsigned long)ar & (MACHINE_ALIGNMENT - 1)));
  invariant (size == ar_size ((void *)ar, lim_use_must_malloc, sizeof (*ar)));
  for (x = 0; x < size; ++x)
    invariant (ar[x] == HASH_INDEX (x));
}



int
main (int argc, char * argv[])
{
  int errn;
  int o;
  struct opt_parsed * option;
  unsigned int iterations;

  option = 0;
  iterations = 10000;

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
	  if (cvt_decimal_to_uint (&errn, &iterations, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;
	}
    }


  {
#  define MAX_ARRAYS 1024
    unsigned long * arrays[1024];
    size_t sizes[1024];
    int n_arrays;
    enum ar_op
      {
	op_new = 0,
	op_ar_size,
	op_ar_setsize,
	op_ar_compact,
	op_ar_free,
	op_push,
	op_pop,
	op_copy,
	n_ops = op_copy + 1
      };
    int counts[n_ops];
    int x;

    for (x = 0; x < n_ops; ++x)
      counts[x] = 0;

    n_arrays = 0;
    while (iterations--)
      {
	int choose;
	enum ar_op op;
	if (!n_arrays)
	  {
	    op = op_new;
	    choose = 0;
	  }
	else
	  {
	    op = (enum ar_op)(random () % n_ops);
	    choose = (random () % n_arrays);
	  }

	counts[op]++;
	switch (op)
	  {
	    {
	    case op_new:
	      if (n_arrays == MAX_ARRAYS)
		break;
	      else
		{
		  size_t size;
		  int x;
		  
		  size = 1 + random () % 1024;
		  arrays[n_arrays] = 0;
		  ar_setsize ((void **)&arrays[n_arrays], lim_use_must_malloc, size, sizeof (unsigned long));
		  for (x = 0; x < size; ++x)
		    arrays[n_arrays][x] = HASH_INDEX(x);
		  sizes[n_arrays] = size;
		  ++n_arrays;
		}


	    case op_copy:
	      if (n_arrays == MAX_ARRAYS)
		break;
	      else
		{
		  check_array (sizes[choose], arrays[choose]);
		  arrays[n_arrays] = (unsigned long *)ar_copy ((void *)arrays[choose], lim_use_must_malloc, sizeof (unsigned long));
		  sizes[n_arrays] = sizes[choose];
		  check_array (sizes[n_arrays], arrays[n_arrays]);
		  ++n_arrays;
		}
	      
	    case op_ar_size:
	      check_array (sizes[choose], arrays[choose]);
	      invariant (sizes[choose] == ar_size ((void *)arrays[choose], lim_use_must_malloc, sizeof (*arrays[choose])));
	      break;

	    case op_ar_compact:
	      check_array (sizes[choose], arrays[choose]);
	      ar_compact ((void **)&arrays[choose], lim_use_must_malloc, sizeof (unsigned long));
	      check_array (sizes[choose], arrays[choose]);
	      break;

	    case op_push:
	      check_array (sizes[choose], arrays[choose]);
	      *(unsigned long *)ar_push ((void **)&arrays[choose], lim_use_must_malloc, sizeof (unsigned long)) = HASH_INDEX(sizes[choose]);
	      ++sizes[choose];
	      check_array (sizes[choose], arrays[choose]);
	      break;

	    case op_pop:
	      {
		unsigned long val;

		check_array (sizes[choose], arrays[choose]);
		if (!sizes[choose])
		  break;
		val = arrays[choose][sizes[choose] - 1];
		invariant (val == *(unsigned long *)ar_pop ((void **)&arrays[choose], lim_use_must_malloc, sizeof (unsigned long)));
		--sizes[choose];
		check_array (sizes[choose], arrays[choose]);
		break;
	      }

	    case op_ar_setsize:
	      {
		size_t size;
		size_t old_size;
		size_t smaller;
		size_t x;

		check_array (sizes[choose], arrays[choose]);
		old_size = sizes[choose];
		size = random () % 1024;
		smaller = (old_size < size ? old_size : size);
		ar_setsize ((void **)&arrays[choose], lim_use_must_malloc, size, sizeof (unsigned long));
		sizes[choose] = size;
		for (x = 0; x < smaller; ++x)
		  invariant (arrays[choose][x] == HASH_INDEX (x));
		for (x = smaller; x < size; ++x)
		  {
		    arrays[choose][x] = HASH_INDEX (x);
		  }
		check_array (sizes[choose], arrays[choose]);
		break;
	      }

	    case op_ar_free:
	      {
		unsigned long * ar;

		check_array (sizes[choose], arrays[choose]);
		ar = arrays[choose];
		arrays[choose] = arrays[n_arrays - 1];
		sizes[choose ] = sizes[n_arrays - 1];
		--n_arrays;
		ar_free ((void **)&ar, lim_use_must_malloc);
		invariant (ar == 0);
		break;
	      }

	    default:
	      while (1)
		panic ("unknown operation");
	    }
	  }
      }
    {
      int total;

      total = 0;

      safe_printfmt (1, "op_new: %d tests\n", counts[op_new]);
      total += counts[op_new];
      safe_printfmt (1, "op_ar_size: %d tests\n", counts[op_ar_size]);
      total += counts[op_ar_size];
      safe_printfmt (1, "op_ar_setsize: %d tests\n", counts[op_ar_setsize]);
      total += counts[op_ar_setsize];
      safe_printfmt (1, "op_ar_compact: %d tests\n", counts[op_ar_compact]);
      total += counts[op_ar_compact];
      safe_printfmt (1, "op_ar_free: %d tests\n", counts[op_ar_free]);
      total += counts[op_ar_free];
      safe_printfmt (1, "op_push: %d tests\n", counts[op_push]);
      total += counts[op_push];
      safe_printfmt (1, "op_pop: %d tests\n", counts[op_pop]);
      total += counts[op_pop];
      safe_printfmt (1, "op_copy: %d tests\n", counts[op_copy]);
      total += counts[op_copy];
      safe_printfmt (1, "TOTAL tests: %d\n", total);
    }
    
  }
  return 0;
}



