/* tag: Tom Lord Tue Dec  4 14:41:22 2001 (test-time-re.c)
 */
/* time-re.c - timing test for rx-xml
 *
 ****************************************************************
 * Copyright (C) 2001  Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/cmd/main.h"
#include "hackerlab/rx-xml/re.h"



static t_uchar * program_name = "time-re";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_verbose, "v", "verbose", 0, \
      "Print cache usage information.") \
  OP (opt_monoscript, "m", "monoscript", 0, \
      "Don't use lots of different scripts in the tests.") \
  OP (opt_utf16, 0, "utf16", 0, \
      "Test matching utf16 strings.") \
  OP (opt_iterations, "i", "iterations=N", 1, \
      "Set the number of test iterations.") \
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



static t_uchar * test_exp = "\\p{Ll}{4}-\\p{Nd}{5}";

#define N_TESTS	6

static t_uchar * test_strings[N_TESTS] =
{
  "lmnop-09876",
  "\\u03ac\\u03ad\\u03ae\\u03af-12345",
  "abcd-13579",
  "\\u0255\\u0256\\u0257\\u0258-67890",
  "\\u0561\\u0562\\u0563\\u0564-24680",
  "\\u00e0\\u00e1\\u00e2\\u00e3-\\u0966\\u0967\\u0968\\u0969\\u096a",
};

#define BUF_SIZE 1024

static t_uchar utf16_tests[N_TESTS][BUF_SIZE];
static size_t utf16_test_sizes[N_TESTS];

static t_uchar utf8_tests[N_TESTS][BUF_SIZE];
static size_t utf8_test_sizes[N_TESTS];


static size_t
translate_str (enum uni_encoding_scheme encoding, t_uchar * output, t_uchar * input)
{
  size_t input_pos;
  size_t input_len;
  size_t output_pos;
  size_t output_len;

  input_pos = 0;
  input_len = str_length (input);

  output_pos = 0;
  output_len = BUF_SIZE;
  
  while (input_pos < input_len)
    {
      t_unicode c;

      c = input[input_pos++];

      if (   (c == '\\')
	  && (input_pos < input_len)
	  && (input[input_pos] == 'u'))
	{
	  int errn;
	  unsigned long x;

	  if (   ((input_pos + 5) > input_len)
	      || (cvt_hex_to_ulong (&errn, &x, input + input_pos + 1, 4)))
	    panic ("bogus escape in test string");
	  c = x;
	  input_pos += 5;
	}

      switch (encoding)
	{
	case uni_utf8:
	  uni_utf8_iput (output, &output_pos, output_len, c);
	  break;
	case uni_utf16:
	  uni_utf16_iput (output, &output_pos, output_len, c);
	  break;
	default:
	  panic ("coding error in unit-re.c");
	}
    }

  return output_pos;
}


static void
init_tests (void)
{
  int x;

  for (x = 0; x < N_TESTS; ++x)
    {
      utf16_test_sizes[x] = translate_str (uni_utf16, utf16_tests[x], test_strings[x]);
      utf8_test_sizes[x] = translate_str (uni_utf8, utf8_tests[x], test_strings[x]);
    }
}



int
main (int argc, char * argv[])
{
  int errn;
  int o;
  struct opt_parsed * option;
  enum uni_encoding_scheme encoding;
  unsigned long cache_size;
  unsigned long iterations;
  int verbose;
  int monoscript;

  option = 0;
  encoding = uni_utf8;
  iterations = 150000;
  verbose = 0;
  monoscript = 0;

  while (1)
    {
      o = opt_standard (lim_use_must_malloc, &option, opts, &argc, argv, program_name, usage, version_string, opt_help_msg, opt_version);
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

	case opt_iterations:
	  if (cvt_decimal_to_ulong (&errn, &iterations, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_utf16:
	  encoding = uni_utf16;
	  break;

	case opt_verbose:
	  verbose = 1;
	  break;

	case opt_monoscript:
	  monoscript = 1;
	  break;
	}
    }

  init_tests ();

  {
    int outer;
    rx_xml_rebuf re;

    if (rx_xml_recomp (&re, uni_iso8859_1, (uni_string)test_exp, str_length (test_exp)))
      panic ("unable to compile regexp");

    for (outer = 0; outer < iterations; ++outer)
      {
	int inner;

	for (inner = 0; inner < N_TESTS; ++inner)
	  {
	    int matches;
	    enum rx_xml_rematch_errno errn;
	    int test;

	    if (inner && monoscript)
	      test = 1;
	    else
	      test = inner;

	    matches = rx_xml_is_match (&errn, &re, encoding,
				       (uni_string)(encoding == uni_utf8 ? utf8_tests[test] : utf16_tests[test]), 
				       (encoding == uni_utf8 ? utf8_test_sizes[test] : utf16_test_sizes[test] / 2));

	    if (matches && (inner == 0))
	      panic ("bogus string matched");
	    else if (matches < 0)
	      panic ("error during match");
	  }
      }
  }

  if (verbose)
    {
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
    }

  return 0;
}

