/* unit-bits.c - test bits.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/char/str.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-bits";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_iterations, "i", "iterations n", 1, \
      "Run all tests `n' times.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};




#if 1
struct bits_tree_rule rules[] = {{16, 256*16, 12, 0xfff}, {16, 256, 0, 0}, {0, 256, 0, 0}};
#else
struct bits_tree_rule rules[] = {{256, 256, 8, 0xff}, {0, 256, 0, 0}};
#endif

static bits
make_test_set (bitset members)
{
  bits answer;
  int x;

  answer = bits_alloc (0, rules);
  for (x = 0; x < 65536; ++x)
    if (bitset_is_member (members, x))
      bits_adjoin (answer, x);
  bits_compact (answer);
  return answer;
}

static int test_no = 0;

static int
compare_test_result (bits test_answer, bitset answer)
{
  int x;

  for (x = 0; x < 65536; ++x)
    if (bitset_is_member (answer, x) != bits_is_member (test_answer, x))
      panic ("test failed");
  bits_compact (test_answer);
  for (x = 0; x < 65536; ++x)
    if (bitset_is_member (answer, x) != bits_is_member (test_answer, x))
      panic ("test failed");
  return 1;
}

static void
free_test_set (bits b)
{
  bits_free (b);
}



static bitset
make_empty_bitset (void)
{
  return bitset_alloc (lim_use_must_malloc, 65536);
}


static bitset
make_full_bitset (void)
{
  bitset b;

  b = bitset_alloc (lim_use_must_malloc, 65536);
  bitset_fill (65536, b);
  return b;
}


static bitset
make_random_bitset (void)
{
  bitset b;
  int x;
  
  b = bitset_alloc (lim_use_must_malloc, 65536);
  for (x = 0; x < 65536; ++x)
    if (random () & 1)
      bitset_adjoin (b, x);

  return b;
}

static bitset (*contents_fns[])(void) = 
{
  make_empty_bitset,
  make_full_bitset,
  make_random_bitset,
  0
};



static int
make_zero_index (void)
{
  return 0;
}

static int
make_max_index (void)
{
  return 65535;
}

static int
make_random_index (void)
{
  return random () % 65536;
}

static int (*index_fns[])(void) = 
{
  make_zero_index,
  make_max_index,
  make_random_index,
  0
};




static void
make_full_range (int * from, int * to)
{
  *from = 0;
  *to = 65536;
}


static void
make_empty_range (int * from, int * to)
{
  *from = random () % 65537;
  if (!*from)
    *to = 0;
  else
    *to = random () % *from;
}


static void
make_small_random_range (int * from, int * to)
{
  *from = random () % (65536 - 256);
  *to = *from + random () % 256;
}


static void
make_medium_random_range (int * from, int * to)
{
  *from = random () % (65536 - 256);
  *to = *from + random () % 256;
}


static void
make_large_random_range (int * from, int * to)
{
  *to = 1024 + (random () % (65537 - 1024));
  *from = random () % (*to - 768);
}


static void
make_tail_range (int * from, int * to)
{
  *from = random () % 65537;
  *to = 65536;
}

static void
make_head_range (int * from, int * to)
{
  *from = 0;
  *to = random () % 65537;
}

static void (*range_fns[])(int *, int *) = 
{
  make_full_range,
  make_empty_range,
  make_small_random_range,
  make_medium_random_range,
  make_large_random_range,
  make_tail_range,
  make_head_range,
  0
};




int
main (int argc, char * argv[])
{
  int errn;
  int o;
  struct opt_parsed * option;
  int iterations;

  option = 0;
  iterations = 1;

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

  while (iterations--)
    {
      /* functions that operate on a bitset */
      {
	int contents_fn;

	for (contents_fn = 0; contents_fns [contents_fn]; ++contents_fn)
	  {
	    enum bitset_fn
	      {
		is_empty,
		is_full,
		population,
		clear,
		fill,
		complement,
		ffs,
		ffc,
		max_bitset_fn = ffc
	      } fn;

	    for (fn = 0; fn <= max_bitset_fn; ++fn)
	      {
		bitset b;
		bits b16;

		++test_no;
		b = contents_fns[contents_fn]();
		b16 = make_test_set (b);

		switch (fn)
		  {
		  default:
		    panic ("missing test fn");
		  case is_empty:
		    if (!(bitset_is_empty (65536, b) == bits_is_empty (b16)))
		      panic ("bitset_is_empty test failed");
		    break;
		  case is_full:
		    if (!(bitset_is_full (65536, b) == bits_is_full (b16)))
		      panic ("bitset_is_full test failed");
		    break;
		  case population:
		    if (!(bitset_population (65536, b) == bits_population (b16)))
		      panic ("bitset_population test failed");
		    break;
		  case ffs:
		    if (!(bitset_ffs (65536, b) == bits_ffs (b16)))
		      panic ("bitset_ffs test failed");
		    break;
		  case ffc:
		    if (!(bitset_ffc (65536, b) == bits_ffc (b16)))
		      panic ("bitset_ffc test failed");
		    break;
		  case clear:
		    bitset_clear (65536, b);
		    bits_clear (b16);
		    compare_test_result (b16, b);
		    break;
		  case fill:
		    bitset_fill (65536, b);
		    bits_fill (b16);
		    compare_test_result (b16, b);
		    break;
		  case complement:
		    bitset_complement (65536, b);
		    bits_complement (b16);
		    compare_test_result (b16, b);
		    break;
		  }
		free_test_set (b16);
		bitset_free (lim_use_must_malloc, b);
	      }
	  }
      }
      
      /* functions that operate on a bitset and index */
      {
	int contents_fn;

	for (contents_fn = 0; contents_fns [contents_fn]; ++contents_fn)
	  {
	    int index_fn;

	    for (index_fn = 0; index_fns [index_fn]; ++index_fn)
	      {
		enum bitset_n_fn
		  {
		    is_member,
		    adjoin,
		    remove,
		    toggle,
		    max_bitset_n_fn = toggle
		  } fn;

		for (fn = 0; fn <= max_bitset_n_fn; ++fn)
		  {
		    bitset b;
		    bits b16;
		    int n;

		    ++test_no;
		    b = contents_fns[contents_fn] ();
		    b16 = make_test_set (b);
		    n = index_fns[index_fn] ();

		    switch (fn)
		      {
		      default:
			panic ("missing test fn");
		      case is_member:
			if (bitset_is_member (b, n) != bits_is_member (b16, n))
			  panic ("bitset_is_member test failed");
			break;
		      case adjoin:
			bitset_adjoin (b, n);
			bits_adjoin (b16, n);
			compare_test_result (b16, b);
			break;
		      case remove:
			bitset_remove (b, n);
			bits_remove (b16, n);
			compare_test_result (b16, b);
			break;
		      case toggle:
			bitset_toggle (b, n);
			bits_toggle (b16, n);
			compare_test_result (b16, b);
			break;
		      }
		    free_test_set (b16);
		    bitset_free (lim_use_must_malloc, b);
		  }
	      }
	  }
      }
      
      /* functions that operate on a bitset and range */
      {
	int contents_fn;

	for (contents_fn = 0; contents_fns [contents_fn]; ++contents_fn)
	  {
	    int range_fn;

	    for (range_fn = 0; range_fns [range_fn]; ++range_fn)
	      {
		enum bitset_range_fn
		  {
		    is_empty_range,
		    is_full_range,
		    clear_range,
		    fill_range,
		    population_range,
		    ffs_range,
		    ffc_range,
		    max_bitset_range_fn = ffc_range
		  } fn;

		for (fn = 0; fn <= max_bitset_range_fn; ++fn)
		  {
		    bitset b;
		    bits b16;
		    int from;
		    int to;

		    ++test_no;
		    b = contents_fns[contents_fn]();
		    b16 = make_test_set (b);
		    range_fns[range_fn] (&from, &to);

		    switch (fn)
		      {
		      default:
			panic ("missing test fn");
		      case is_empty_range:
			if (bitset_is_empty_range (b, from, to) != bits_is_empty_range (b16, from, to))
			  panic ("bitset_is_empty_range test failed");
			break;
		      case is_full_range:
			if (bitset_is_full_range (b, from, to) != bits_is_full_range (b16, from, to))
			  panic ("bitset_is_empty_range test failed");
			break;
		      case population_range:
			if (bitset_population_range (b, from, to) != bits_population_range (b16, from, to))
			  panic ("bitset_population_range test failed");
			break;
		      case ffs_range:
			if (bitset_ffs_range (b, from, to) != bits_ffs_range (b16, from, to))
			  panic ("bitset_ffs_range test failed");
			break;
		      case ffc_range:
			if (bitset_ffc_range (b, from, to) != bits_ffc_range (b16, from, to))
			  panic ("bitset_ffc_range test failed");
			break;
		      case clear_range:
			bitset_clear_range (b, from, to);
			bits_clear_range (b16, from, to);
			compare_test_result (b16, b);
			break;
		      case fill_range:
			bitset_fill_range (b, from, to);
			bits_fill_range (b16, from, to);
			compare_test_result (b16, b);
			break;
		      }
		    free_test_set (b16);
		    bitset_free (lim_use_must_malloc, b);
		  }
	      }
	  }
      }
      
      /*
       * two_bitsets;
       */
      {
	int contents_fn_a;

	for (contents_fn_a = 0; contents_fns [contents_fn_a]; ++contents_fn_a)
	  {
	    int contents_fn_b;

	    for (contents_fn_b = 0; contents_fns [contents_fn_b]; ++contents_fn_b)
	      {
		enum two_bitsets_fn
		  {
		    is_equal,
		    is_subset,
		    assign,
		    union_,
		    intersection,
		    difference,
		    revdifference,
		    xor,
		    max_two_bitsets_fn = xor
		  } fn;

		for (fn = 0; fn <= max_two_bitsets_fn; ++fn)
		  {
		    bitset a;
		    bitset b;
		    bits a16;
		    bits b16;

		    ++test_no;
		    a = contents_fns[contents_fn_a]();
		    b = contents_fns[contents_fn_b]();
		    a16 = make_test_set (a);
		    b16 = make_test_set (b);

		    switch (fn)
		      {
		      default:
			panic ("missing test fn");
		      case is_equal:
			if (!(bitset_is_equal (65536, a, b) == bits_is_equal (a16, b16)))
			  panic ("bitset_is_equal test failed");
			break;
		      case is_subset:
			if (!(bitset_is_subset (65536, a, b) == bits_is_subset (a16, b16)))
			  panic ("bitset_is_subset test failed");
			break;
		      case assign:
			bitset_assign (65536, a, b);
			bits_assign (a16, b16);
			compare_test_result (a16, a);
			break;
		      case union_:
			bitset_union (65536, a, b);
			bits_union (a16, b16);
			compare_test_result (a16, a);
			break;
		      case intersection:
			bitset_intersection (65536, a, b);
			bits_intersection (a16, b16);
			compare_test_result (a16, a);
			break;
		      case difference:
			bitset_difference (65536, a, b);
			bits_difference (a16, b16);
			compare_test_result (a16, a);
			break;
		      case revdifference:
			bitset_revdifference (65536, a, b);
			bits_revdifference (a16, b16);
			compare_test_result (a16, a);
			break;
		      case xor:
			bitset_xor (65536, a, b);
			bits_xor (a16, b16);
			compare_test_result (a16, a);
			break;
		      }
		    free_test_set (a16);
		    free_test_set (b16);
		    bitset_free (lim_use_must_malloc, a);
		    bitset_free (lim_use_must_malloc, b);
		  }
	      }
	  }
      }
    }
  safe_printfmt (1, "completed %d tests\n", test_no);
  return 0;
}

