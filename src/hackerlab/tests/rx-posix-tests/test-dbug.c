/* test-dbug.c - test dbug.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/sys/types.h"
#include "hackerlab/rx/dbug.h"
#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx-posix/re8-parse.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/cmd/main.h"




static t_uchar * program_name = "dbug-comp";
static t_uchar * usage = "[options] reegxp";
static t_uchar * version_string = "1.0";



#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_extended, "x", "extended", 0, \
      "Parse using ERE (extended) syntax.") \
  OP (opt_newline, "n", "newline", 0, \
      "Parse assuming newline separates lines.") \
  OP (opt_dfa, "d", "dfa", 0, \
      "Parse using DFA-only language variant.") \
  OP (opt_nfa, "N", "nfa", 0, \
      "Print the resulting nfa.")

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
  int extended;
  int newline;
  int dfa;
  int nfa;
  int o;
  struct opt_parsed * option;

  extended = 0;
  newline = 0;
  dfa = 0;
  nfa = 0;
  
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
#if 0
	bogus_arg:
	  safe_printfmt (2, "ill-formed argument for `%s' (`%s')\n", option->opt_string, option->arg_string);
	  goto usage_error;
#endif

	case opt_extended:
	  extended = 1;
	  break;

	case opt_newline:
	  newline = 1;
	  break;

	case opt_dfa:
	  dfa = 1;
	  break;

	case opt_nfa:
	  nfa = 1;
	  break;
	}
    }
  if (argc != 2)
    goto usage_error;

  {
    struct rx_exp_node * exp;
    struct rx_exp_node ** subexps;
    int nsub;
    int err;

    err = rx_parse (&exp, &nsub, (char *)argv[1], str_length (argv[1]), extended, newline, dfa, 256, 0);
    if (err)
      printfmt (&errn, 1, "compilation error (%d): %s\n", err, rx_error_msg[err]);
    else
      {
	subexps = 0;
	nsub = 1;
	rx_analyze_rexp (&subexps, &nsub, exp);
	rx_print_rexp (1, 256, 0, exp);
	if (nfa)
	  {
	    struct rx_nfa * rx;
	    struct rx_nfa_state * start;
	    struct rx_nfa_state * end;

	    printfmt (&errn, 1, "\n\n");
	    rx = rx_nfa_xalloc (256);
	    start = end = 0;
	    rx_build_nfa (rx, exp, &start, &end);
	    rx_set_start_state (rx, start);
	    rx_set_state_label (rx, end, 1);
	    rx_print_nfa (1, rx);
	  }
      }
  }
  return 0;
}



