/* tag: Tom Lord Tue Dec  4 14:41:22 2001 (unit-re.c)
 */
/* unit-re.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/cmd/main.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/rx/dbug.h"
#include "hackerlab/uni/coding.h"
#include "hackerlab/rx-xml/re.h"
#include "hackerlab/xml/charsets.h"
#include "hackerlab/tests/rx-xml-tests/tests.h"


/* __STDC__ prototypes for static functions */
static size_t translate_str (size_t * points, enum uni_encoding_scheme encoding, t_uchar * output, t_uchar * input);



static t_uchar * program_name = "unit-re";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_iterations, "i", "iterations=n", 1, \
      "Iterate N times.") \
  OP (opt_only_test, "t", "test=n", 1, \
      "Run only test N.") \
  OP (opt_find_leak, "l", "find-leak", 0, \
      "Figure out which test is leaking storage.") \
  OP (opt_alternate_syntax, "a", "alternate-syntax", 0, \
      "Use the alternative regexp syntax.") \
  OP (opt_print_re, "p", "print-re", 0, \
      "Print syntax trees.") \
  OP (opt_dfa_cache_threshold, "D", "dfa-cache-threshold=N", 1, \
      "Set the DFA cache GC threshold.") \
  OP (opt_nfa_cache_threshold, "N", "nfa-cache-threshold=N", 1, \
      "Set the NFA cache GC threshold.") \
  OP (opt_cache_compilations, "c", "cache-compilations", 0, \
      "Compile each r.e. only once.") \
  OP (opt_verbose, "v", "verbose", 0, \
      "Print test names and numbers.") \
  OP (opt_range, "r", "range=m,n", 1, \
      "Run tests m..n.") \
  OP (opt_matches_only, "m", "matches-only", 0, \
      "Only run tests cases with matching string.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



int
main (int argc, char * argv[])
{
  int errn;
  int verbose;
  int exit_status;
  int iter;
  int find_leak;
  int alternate_syntax;
  int only_test;
  int print_re;
  int matches_only;
  int cache_comps;
  int o;
  long lo;
  long hi;
  struct opt_parsed * option;
  unsigned long cache_size;
  rx_xml_rebuf * re_cache;
  enum rx_xml_recomp_errno * comp_error_cache;
  int * re_cached;

  lo = -1;
  hi = -1;
  verbose = 0;
  iter = 1;
  find_leak = 0;
  alternate_syntax = 0;
  only_test = -1;
  print_re = 0;
  matches_only = 0;
  cache_comps = 0;
  re_cache = 0;
  comp_error_cache = 0;
  re_cached = 0;

  option = 0;

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

	case opt_iterations:
	  if (cvt_decimal_to_uint (&errn, &iter, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_only_test:
	  if (cvt_decimal_to_uint (&errn, &only_test, option->arg_string, str_length (option->arg_string)))
	    goto bogus_arg;
	  break;

	case opt_find_leak:
	  find_leak = 1;
	  break;

	case opt_alternate_syntax:
	  alternate_syntax = 1;
	  break;

	case opt_print_re:
	  print_re = 1;
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

	case opt_cache_compilations:
	  cache_comps = 1;
	  break;

	case opt_verbose:
	  verbose = 1;
	  break;

	case opt_range:
	  {
	    t_uchar * comma;

	    comma = str_chr_index (option->arg_string, ',');
	    if (!comma)
	      goto bogus_arg;
	    if (   cvt_decimal_to_long (&errn, &lo, option->arg_string, comma - option->arg_string)
		|| (lo < 0))
	      goto bogus_arg;
	    if (   cvt_decimal_to_long (&errn, &hi, comma + 1, str_length (comma + 1))
		|| (hi < lo))
	      goto bogus_arg;
	    break;
	  }

	case opt_matches_only:
	  matches_only = 1;
	  break;
	}
    }


  {
    static enum uni_encoding_scheme encoding_progression[] = { uni_utf8, uni_utf16 };
    int x;
    int n_comps;
    int n_matches;
    size_t n_bytes_matched;
    size_t n_points_matched;
    struct xml_re_test_case * test_cases;
    size_t * str_bytes_table[2];
    size_t * str_points_table[2];
    t_uchar ** str_table[2];

    n_comps = 0;
    n_matches = 0;
    n_bytes_matched = 0;
    n_points_matched = 0;

    if (alternate_syntax)
      test_cases = xml_re_test_cases_alternative_syntax;
    else
      test_cases = xml_re_test_cases;

    str_bytes_table[0] = 0;
    str_bytes_table[1] = 0;
    str_points_table[0] = 0;
    str_points_table[1] = 0;
    str_table[0] = 0;
    str_table[1] = 0;
    for (x = 0; test_cases[x].test_name; ++x)
      {
	int enc;

	for (enc = 0; enc < 2; ++enc)
	  {
	    size_t * points;

	    *(t_uchar **)ar_ref ((void **)&str_table[enc], lim_use_must_malloc, x, sizeof (t_uchar *)) = (t_uchar *)must_malloc (1024);

	    points = (size_t *)ar_ref ((void **)&str_points_table[enc], lim_use_must_malloc, x, sizeof (size_t));
	    *(size_t *)ar_ref ((void **)&str_bytes_table[enc], lim_use_must_malloc, x, sizeof (size_t))
	      = translate_str (points, encoding_progression[enc], str_table[enc][x], test_cases[x].str);
	  }
      }

    exit_status = 0;

    while (iter--)
      {
	int from;
	int to;

	if (only_test >= 0)
	  {
	    int q;
	    from = only_test;
	    to = from + 1;
	    for (q = 0; test_cases[q].test_name; ++q)
	      ;
	    if (from >= q)
	      {
		safe_printfmt (2, "test number (%d) out of range\n", from);
		exit (1);
	      }
	  }
	else
	  {
	    from = 0;
	    for (to = 0; test_cases[to].test_name; ++to)
	      ;

	    if ((lo >= 0) && (lo < to))
	      {
		from = lo;
		if (hi + 1 < to)
		  to = hi + 1;
	      }
	  }
	for (x = from; x < to; ++x)
	  {
	    int re_encoding;

	    if (verbose)
	      {
		safe_printfmt (1, "test %d (%s)\n", x, test_cases[x].test_name);
	      }
	    for (re_encoding = 0; re_encoding < 2; ++re_encoding)
	      {
		t_uchar re_src[1024];
		size_t re_src_bytes;
		rx_xml_rebuf * re;
		enum rx_xml_recomp_errno comp_error;

		re = (rx_xml_rebuf *)ar_ref ((void **)&re_cache, lim_use_must_malloc, x, sizeof (rx_xml_rebuf));
		if (cache_comps && *(int *)ar_ref ((void **)&re_cached, lim_use_must_malloc, x, sizeof (int)))
		  {
		    comp_error = *(enum rx_xml_recomp_errno *)ar_ref ((void **)&comp_error_cache, lim_use_must_malloc, x, sizeof (enum rx_xml_recomp_errno));
		  }
		else
		  {
		    ++n_comps;
		    re_src_bytes = translate_str (0, encoding_progression[re_encoding], re_src, test_cases[x].re);
		    if (alternate_syntax)
		      comp_error = rx_xml_recomp_opts (re,
						       encoding_progression[re_encoding],
						       (uni_string)re_src,
						       re_src_bytes / uni_code_unit_size (encoding_progression[re_encoding]),
						       rx_xml_syntax_dot_dot_ranges | rx_xml_syntax_carrot_set_difference,
						       xml_charset);
		    else
		      comp_error = rx_xml_recomp (re,
						  encoding_progression[re_encoding],
						  (uni_string)re_src,
						  re_src_bytes / uni_code_unit_size (encoding_progression[re_encoding]));
		    *(enum rx_xml_recomp_errno *)ar_ref ((void **)&comp_error_cache, lim_use_must_malloc, x, sizeof (enum rx_xml_recomp_errno)) = comp_error;
		    *(int *)ar_ref ((void **)&re_cached, lim_use_must_malloc, x, sizeof (int)) = 1;
		  }

		if (comp_error != test_cases[x].comp_error)
		  {
		    safe_printfmt (2, "test %d (%s): incorrect compilation error -- got %d, wanted %d\n",
				   x, test_cases[x].test_name, comp_error, test_cases[x].comp_error);
		    safe_printfmt (2, "  re encoding = %s\n", (encoding_progression[re_encoding] == uni_utf8 ? "utf8" : "utf16"));
		    safe_printfmt (2, "  comp # %d, match # %d\n", n_comps, n_matches);
		    exit_status = 1;
		  }

		if (comp_error)
		  {
		    if (find_leak)
		      {
			size_t dfa_bytes;
			size_t nfa_bytes;
			
			dfa_bytes = rx_flush_dfa_cache ();
			nfa_bytes = rx_flush_nfa_cache ();

			if (dfa_bytes || nfa_bytes)
			  {
			    safe_printfmt (2, "test %d (%s): comp error\n", x, test_cases[x].test_name);
			    safe_printfmt (2, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);
			    safe_printfmt (2, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);
			    exit_status = 1;
			  }
		      }
		  }

		if (print_re)
		  {
		    safe_printfmt (1, "test %d (%s)\n", x, test_cases[x].test_name);
		    safe_printfmt (1, "r.e. source:\n");
		    safe_printfmt (1, "    %s\n", test_cases[x].re);
		    safe_printfmt (1, "r.e. syntax:\n");
		    if (!comp_error)
		      rx_print_rexp (1, 1 << 21, 4, re->exp);
		    safe_printfmt (1, "test string:\n");
		    safe_printfmt (1, "    %s\n", test_cases[x].str);
		  }
		

		if (!comp_error && !test_cases[x].comp_error && (!matches_only || test_cases[x].is_match))
		  {
		    int str_encoding;

		    for (str_encoding = 0; str_encoding < 2; ++str_encoding)
		      {
			t_uchar * str;
			size_t str_bytes;
			enum rx_xml_rematch_errno match_error;
			int is_match;

			++n_matches;
			str_bytes = str_bytes_table[str_encoding][x];
			str = str_table[str_encoding][x];

			match_error = 0;
			is_match = rx_xml_is_match (&match_error,
						    re,
						    encoding_progression[str_encoding],
						    (uni_string)str,
						    str_bytes / uni_code_unit_size (encoding_progression[str_encoding]));
			n_bytes_matched += str_bytes;
			n_points_matched += str_points_table[str_encoding][x];

			if (   ((is_match >= 0) && test_cases[x].match_error)
			    || (match_error != test_cases[x].match_error))
			  {
			    safe_printfmt (2, "test %d (%s): incorrect match error -- got %d, wanted %d\n",
					   x, test_cases[x].test_name, match_error, test_cases[x].match_error);
			    safe_printfmt (2, "  re encoding = %s\n", (encoding_progression[re_encoding] == uni_utf8 ? "utf8" : "utf16"));
			    safe_printfmt (2, "  str encoding = %s\n", (encoding_progression[str_encoding] == uni_utf8 ? "utf8" : "utf16"));
			    safe_printfmt (2, "  comp # %d, match # %d\n", n_comps, n_matches);
			    exit_status = 1;
			  }
			else if (is_match != test_cases[x].is_match)
			  {
			    safe_printfmt (2, "test %d (%s): incorrect match result -- got %d, wanted %d\n",
					   x, test_cases[x].test_name, is_match, test_cases[x].is_match);
			    safe_printfmt (2, "  re encoding = %s\n", (encoding_progression[re_encoding] == uni_utf8 ? "utf8" : "utf16"));
			    safe_printfmt (2, "  str encoding = %s\n", (encoding_progression[str_encoding] == uni_utf8 ? "utf8" : "utf16"));
			    safe_printfmt (2, "  comp # %d, match # %d\n", n_comps, n_matches);
			    exit_status = 1;
			  }
		      }
		    if (!cache_comps)
		      {
			rx_xml_free_re (re);

			if (find_leak)
			  {
			    size_t dfa_bytes;
			    size_t nfa_bytes;
			    
			    dfa_bytes = rx_flush_dfa_cache ();
			    nfa_bytes = rx_flush_nfa_cache ();
			    
			    if (dfa_bytes || nfa_bytes)
			      {
				safe_printfmt (2, "test %d (%s)\n", x, test_cases[x].test_name);
				safe_printfmt (2, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);
				safe_printfmt (2, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);
				exit_status = 1;
			      }
			  }
		      }


		  }
	      }
	  }
      }

    if (cache_comps)
      {
	for (x = 0; test_cases[x].test_name; ++x)
	  {
	    if (   *(int *)ar_ref ((void **)&re_cached, lim_use_must_malloc, x, sizeof (int))
		&& !*(enum rx_xml_recomp_errno *)ar_ref ((void **)&comp_error_cache, lim_use_must_malloc, x, sizeof (enum rx_xml_recomp_errno)))
	      {
		rx_xml_rebuf * re;
		re = (rx_xml_rebuf *)ar_ref ((void **)&re_cache, lim_use_must_malloc, x, sizeof (rx_xml_rebuf));
		rx_xml_free_re (re);
	      }
	  }
      }
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
    

    {
      size_t dfa_bytes;
      size_t nfa_bytes;

      dfa_bytes = rx_flush_dfa_cache ();
      safe_printfmt (1, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);

      nfa_bytes = rx_flush_nfa_cache ();
      safe_printfmt (1, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);

      if (dfa_bytes || nfa_bytes)
	exit_status = 1;
    }

    safe_printfmt (1, "%d compiles, %d matches, %lu bytes matched, %lu code points matched\n",
		   n_comps,
		   n_matches,
		   (unsigned long)n_bytes_matched,
		   (unsigned long)n_points_matched);
  }
  
  exit (exit_status);
}



static size_t
translate_str (size_t * points, enum uni_encoding_scheme encoding, t_uchar * output, t_uchar * input)
{
  size_t input_pos;
  size_t input_len;
  size_t output_pos;
  size_t output_len;
  size_t p;

  input_pos = 0;
  input_len = str_length (input);

  output_pos = 0;
  output_len = 1024;
  p = 0;
  
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

      ++p;
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

  if (points)
    *points = p;

  return output_pos;
}


