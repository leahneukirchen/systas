/* unit-bitset.c - test bitset.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/arrays/ar.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/bitsets/bitset.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-bitset";
static t_uchar * usage = "[options] < tests";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_iterations, "i", "iterations", 0, \
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



static bitset
make_test_set (unsigned int size, unsigned int left_bits, unsigned int right_bits, int * members)
{
  bitset real_bitset;
  bitset returned_bitset;
  int real_size;

  real_size = size + 2 * bits_per_subset;
  real_bitset = bitset_alloc (lim_use_must_malloc, real_size);
  returned_bitset = real_bitset + 1;

  if (left_bits)
    bitset_fill_range (real_bitset, 0, bits_per_subset);
  if (right_bits)
    bitset_fill_range (real_bitset, bits_per_subset + size, real_size);

  if (members == (int *)-1L)
    bitset_fill_range (returned_bitset, 0, size);
  else if (members)
    {
      int x;
      for (x = 0; members[x] >= 0; ++x)
	bitset_adjoin (returned_bitset, members[x]);
    }
  return returned_bitset;
}


static void
parse_uint (int * answer, t_uchar ** line, long * len)
{
  int errn;
  t_uchar * start;
  
  while (*len && char_is_space (**line))
    {
      ++*line;
      --*len;
    }

  start = *line;

  while (*len && char_is_digit (**line))
    {
      ++*line;
      --*len;
    }

  if (start == *line)
    panic ("syntax error in input");

  if (cvt_decimal_to_uint (&errn, answer, start, (*line - start)))
    panic ("overflow error in number");

  while (*len && char_is_space (**line))
    {
      ++*line;
      --*len;
    }
}


static bitset
read_test_set (int * size_ret, t_uchar * line, long len)
{
  int size;
  int left;
  int right;
  int * members;
  bitset answer;

  if (!len)
    {
      panic ("syntax error in input");
    }

  parse_uint (&size, &line, &len);
  *size_ret = size;
  parse_uint (&left, &line, &len);
  parse_uint (&right, &line, &len);

  if (!len)
    members = 0;
  else if (*line == 'e')
    members = 0;
  else if (*line == 'f')
    members = (int *)-1L;
  else
    {
      members = 0;
      while (len)
	{
	  int x;
	  parse_uint ((unsigned int *)&x, &line, &len);
	  *(int *)ar_push ((void **)&members, lim_use_must_malloc, sizeof (int)) = x;
	}
      *(int *)ar_push ((void **)&members, lim_use_must_malloc, sizeof (int)) = -1;
    }

  answer = make_test_set (size, left, right, members);
  if (members && (members != (int *)-1L))
    ar_free ((void **)&members, lim_use_must_malloc);
  return answer;
}


static void
print_test_set (int fd, int test_no, int size, bitset b)
{
  int errn;
  bitset real_bitset;
  int real_size;

  real_size = size + 2 * bits_per_subset;
  real_bitset = b - 1;

  printfmt (&errn, fd, "%d ", test_no);
  printfmt (&errn, fd, "%d ", size);

  /* bits to the left */
  {
    int pop;
    int is_empty;
    int is_full;

    pop = bitset_population (bits_per_subset, real_bitset);
    is_empty = bitset_is_empty (bits_per_subset, real_bitset);
    is_full = bitset_is_full (bits_per_subset, real_bitset);
    if (!(is_empty && !pop) && !(is_full && (pop == bits_per_subset)))
      printfmt (&errn, fd, "X ");
    else
      printfmt (&errn, fd, "%d ", is_full);
  }

  /* bits to the right */
  {
    int pop;
    int is_empty;
    int is_full;

    pop = bitset_population_range (real_bitset, bits_per_subset + size, real_size);
    is_empty = bitset_is_empty_range (real_bitset, bits_per_subset + size, real_size);
    is_full = bitset_is_full_range (real_bitset, bits_per_subset + size, real_size);

    if (!(is_empty && !pop) && !(is_full && (pop == (real_size - size - bits_per_subset))))
      printfmt (&errn, fd, "X");
    else
      printfmt (&errn, fd, "%d", is_full);
  }

  /* bits in set */
  {
    int pop;
    int is_empty;
    int is_full;

    pop = bitset_population (size, b);
    is_empty = bitset_is_empty (size, b);
    is_full = bitset_is_full (size, b);

    if ((is_empty && pop) || (is_full && (pop != size)))
      printfmt (&errn, fd, " X");
    else if (is_empty)
      printfmt (&errn, fd, " e");
    else if (is_full)
      printfmt (&errn, fd, " f");
    else
      {
	int x;
	for (x = 0; x < size; ++x)
	  {
	    if (bitset_is_member (b, x))
	      {
		printfmt (&errn, fd, " %d", x);
		--pop;
	      }
	  }
	if (pop)
	  printfmt (&errn, fd, " X");
      }
  }
  printfmt (&errn, fd, "\n");
}


static void
free_test_set (bitset b)
{
  bitset real_bitset;

  real_bitset = b - 1;
  bitset_free (lim_use_must_malloc, real_bitset);
}



static void
print_ordinary_set (int fd, int test_no, int size, bitset b)
{
  int errn;

  printfmt (&errn, fd, "%d ", test_no);
  printfmt (&errn, fd, "%d", size);

  /* bits in set */
  {
    int pop;
    int is_empty;
    int is_full;

    pop = bitset_population (size, b);
    is_empty = bitset_is_empty (size, b);
    is_full = bitset_is_full (size, b);

    if ((is_empty && pop) || (is_full && (pop != size)))
      printfmt (&errn, fd, " X");
    else if (is_empty)
      printfmt (&errn, fd, " e");
    else if (is_full)
      printfmt (&errn, fd, " f");
    else
      {
	int x;
	for (x = 0; x < size; ++x)
	  {
	    if (bitset_is_member (b, x))
	      {
		printfmt (&errn, fd, " %d", x);
		--pop;
	      }
	  }
	if (pop)
	  printfmt (&errn, fd, " X");
      }
  }
  printfmt (&errn, fd, "\n");
}


static int
kw_cmp (t_uchar * line, int len, char * kws)
{
  t_uchar * kw;
  int x;

  kw = (t_uchar *)kws;
  x = 0;
  while ((x < len) && kws[x] && (line[x] == kws[x]))
    ++x;
  return (!kws[x] && ((x == len) || char_is_space (line[x])));
}




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

  {
    bitset a = 0;
    int a_size;
    
    bitset b = 0;
    int b_size;
    
    int n;
    
    int range_start;
    int range_end;
    
    while (1)
      {
	long len;
	t_uchar * line;

	if (0 > vfdbuf_next_line (&errn, &line, &len, 0))
	  panic ("vfdbuf_next_line error");
	if (len == 0)
	  exit (0);
	else if (len < 0)
	  panic ("input error");
	else
	  {
	    int c;
	    c = *line;
	    ++line;
	    --len;
	    switch (c)
	      {
	      default:
		panic ("command syntax error");
	      case 'a':
		a = read_test_set (&a_size, line, len);
		break;
	      case 'b':
		b = read_test_set (&b_size, line, len);
		break;
	      case 'n':
		parse_uint (&n, &line, &len);
		break;
	      case 'r':
		parse_uint (&range_start, &line, &len);
		parse_uint (&range_end, &line, &len);
		break;
	      case 'c':
		if (a)
		  {
		    free_test_set (a);
		    a = 0;
		  }
		if (b)
		  {
		    free_test_set (b);
		    b = 0;
		  }
		break;

	      case '#':
		{
		  int test_no;

		  parse_uint (&test_no, &line, &len);

		  if (kw_cmp (line, len, "dup"))
		    {
		      bitset copy;
		      copy = bitset_dup (lim_use_must_malloc, a_size, a);
		      print_ordinary_set (1, test_no, a_size, copy);
		      bitset_free (lim_use_must_malloc, copy);
		    }
		  else if (kw_cmp (line, len, "is_member"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_member (a, n));
		    }
		  else if (kw_cmp (line, len, "is_equal"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_equal (a_size, a, b));
		    }
		  else if (kw_cmp (line, len, "is_subset"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_subset (a_size, a, b));
		    }
		  else if (kw_cmp (line, len, "is_empty"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_empty (a_size, a));
		    }
		  else if (kw_cmp (line, len, "is_empty_range"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_empty_range (a, range_start, range_end));
		    }
		  else if (kw_cmp (line, len, "is_full"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_full (a_size, a));
		    }
		  else if (kw_cmp (line, len, "is_full_range"))
		    {
		      printfmt (&errn, 1, "%d %d\n", test_no, bitset_is_full_range (a, range_start, range_end));
		    }
		  else if (kw_cmp (line, len, "adjoin"))
		    {
		      bitset_adjoin (a, n);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "remove"))
		    {
		      bitset_remove (a, n);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "toggle"))
		    {
		      bitset_toggle (a, n);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "clear"))
		    {
		      bitset_clear (a_size, a);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "clear_range"))
		    {
		      bitset_clear_range (a, range_start, range_end);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "fill"))
		    {
		      bitset_fill (a_size, a);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "fill_range"))
		    {
		      bitset_fill_range (a, range_start, range_end);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "complement"))
		    {
		      bitset_complement (a_size, a);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "assign"))
		    {
		      bitset_assign (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "union"))
		    {
		      bitset_union (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "intersection"))
		    {
		      bitset_intersection (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "difference"))
		    {
		      bitset_difference (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "revdifference"))
		    {
		      bitset_revdifference (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "xor"))
		    {
		      bitset_xor (a_size, a, b);
		      print_test_set (1, test_no, a_size, a);
		    }
		  else if (kw_cmp (line, len, "population"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_population (a_size, a));
		    }
		  else if (kw_cmp (line, len, "population_range"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_population_range (a, range_start, range_end));
		    }
		  else if (kw_cmp (line, len, "ffs"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_ffs (a_size, a));
		    }
		  else if (kw_cmp (line, len, "ffs_range"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_ffs_range (a, range_start, range_end));
		    }
		  else if (kw_cmp (line, len, "ffc"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_ffc (a_size, a));
		    }
		  else if (kw_cmp (line, len, "ffc_range"))
		    {
		      printfmt (&errn, 1, "%d %ld\n", test_no, bitset_ffc_range (a, range_start, range_end));
		    }
		  else
		    panic ("unrecognized command");
		}
	      }
	  }
      }
  }
  
  return 0;
}

