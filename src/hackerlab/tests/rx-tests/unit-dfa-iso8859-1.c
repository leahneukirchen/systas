/* tag: Tom Lord Tue Dec  4 14:41:20 2001 (unit-dfa-iso8859-1.c)
 */
/* unit-dfa-iso8859-1.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx/dfa-iso8859-1.h"
#include "hackerlab/cmd/main.h"




static t_uchar * program_name = "unit-dfa-iso8859_1";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";


#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.")

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
  int o;
  struct opt_parsed * option;

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

#if 0
	usage_error:
	  opt_usage (2, argv[0], program_name, usage, 1);
	  panic_exit ();

	bogus_arg:
	  safe_printfmt (2, "ill-formed argument for `%s' (`%s')\n", option->opt_string, option->arg_string);
	  goto usage_error;
#endif
	}
    }


  {
    int adv;
    int label;
    int adv_to_final;
    size_t amt;
    t_uchar string[100];
    size_t pos;
    bits a;
    bits b;
    bits big0;
    bits big1;
    bits big2;
    bits c;
    struct rx_exp_node * a_node;
    struct rx_exp_node * b_node;
    struct rx_exp_node * b_star_node;
    struct rx_exp_node * big0_node;
    struct rx_exp_node * big1_node;
    struct rx_exp_node * big2_node;
    struct rx_exp_node * big01_node;
    struct rx_exp_node * bigs_node;
    struct rx_exp_node * bigs_star_node;
    struct rx_exp_node * c_node;
    struct rx_exp_node * a_and_b_star_node;
    struct rx_exp_node * a_and_b_star_and_bigs_star_node;
    struct rx_exp_node * a_and_b_star_and_bigs_star_and_c_node;
    struct rx_nfa * nfa;
    struct rx_nfa_state * start;
    struct rx_nfa_state * end;
    struct rx_dfa * dfa;
    


    a = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
    b = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
    big0 = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
    big1 = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
    big2 = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
    c = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);

    bits_adjoin (a, 'a');
    bits_adjoin (b, 'b');
    bits_adjoin (big0, 0x8a);
    bits_adjoin (big1, 'x');
    bits_adjoin (big2, 0xf5);
    bits_adjoin (c, 'c');

    a_node = rx_mk_r_cset_take (r_cset, 1 << 21, a);
    b_node = rx_mk_r_cset_take (r_cset, 1 << 21, b);
    big0_node = rx_mk_r_cset_take (r_cset, 1 << 21, big0);
    big1_node = rx_mk_r_cset_take (r_cset, 1 << 21, big1);
    big2_node = rx_mk_r_cset_take (r_cset, 1 << 21, big2);
    c_node = rx_mk_r_cset_take (r_cset, 1 << 21, c);

    b_star_node = rx_mk_r_monop (r_star, b_node);
    big01_node = rx_mk_r_binop (r_concat, big0_node, big1_node);
    bigs_node = rx_mk_r_binop (r_concat, big01_node, big2_node);
    bigs_star_node = rx_mk_r_monop (r_star, bigs_node);
    a_and_b_star_node = rx_mk_r_binop (r_concat, a_node, b_star_node);
    a_and_b_star_and_bigs_star_node = rx_mk_r_binop (r_concat, a_and_b_star_node, bigs_star_node);
    a_and_b_star_and_bigs_star_and_c_node = rx_mk_r_binop (r_concat, a_and_b_star_and_bigs_star_node, c_node);

    nfa = rx_nfa_xalloc (1 << 21);
    start = end = 0;
    rx_build_nfa (nfa, a_and_b_star_and_bigs_star_and_c_node, &start, &end);
    rx_set_start_state (nfa, start);
    rx_set_state_label (nfa, end, 69);

    dfa = rx_dfa_alloc (lim_use_must_malloc);
    rx_init_dfa_from_nfa (dfa, nfa);

    pos = 0;
    string[pos++] = 'a';
    string[pos++] = 'b';
    string[pos++] = 'x';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'c';
    rx_dfa_goto_start_superstate (dfa, 1);
    adv = rx_dfa_iso8859_1_fits (&label, dfa, string, pos);
    if (adv < 0)
      panic ("out of memory");
    if (label)
      panic ("bogus string fits");

    pos = 0;
    string[pos++] = 'a';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'c';
    rx_dfa_goto_start_superstate (dfa, 1);
    adv = rx_dfa_iso8859_1_fits (&label, dfa, string, pos);
    if (adv < 0)
      panic ("out of memory");
    if (!label)
      panic ("good string fails");

    pos = 0;
    string[pos++] = 'a';
    string[pos++] = 'b';
    string[pos++] = 'x';
    string[pos++] = 'x';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'c';
    rx_dfa_goto_start_superstate (dfa, 1);
    adv = rx_dfa_iso8859_1_fits (&label, dfa, string, pos);
    if (adv < 0)
      panic ("out of memory");
    if (label)
      panic ("bogus string fits");

    pos = 0;
    string[pos++] = 'a';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 'b';
    string[pos++] = 0x8a;
    string[pos++] = 'x';
    string[pos++] = 0xf5;
    string[pos++] = 'c';
    rx_dfa_goto_start_superstate (dfa, 1);
    adv = rx_dfa_iso8859_1_fits (&label, dfa, string, pos);
    if (adv < 0)
      panic ("out of memory");
    if (!label)
      panic ("good string fails");

    rx_dfa_goto_start_superstate (dfa, 1);
    adv_to_final = rx_dfa_iso8859_1_advance_to_final (&amt, dfa, string, pos);
    if (adv_to_final < 0)
      panic ("out of memory");
    if (!adv_to_final || !dfa->final_tag || (pos != amt))
      panic ("good string doesn't fit (2)");

    if (dfa->final_tag != 69)
      panic ("bogus final tag");


    if (dfa->final_tag != 69)
      panic ("bogus final tag");


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


    rx_clear_dfa_state (dfa);
    rx_free_nfa (nfa);
    rx_free_exp (a_and_b_star_and_bigs_star_and_c_node);

    {
      size_t dfa_bytes;
      size_t nfa_bytes;

      dfa_bytes = rx_flush_dfa_cache ();
      safe_printfmt (1, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);

      nfa_bytes = rx_flush_nfa_cache ();
      safe_printfmt (1, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);

      if (dfa_bytes || nfa_bytes)
	exit (1);
    }
  }

  return 0;
}



