/* unit-panic.c - test panic.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-panic";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";



#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_panic, "p", "panic msg", 1, \
      "Exit by `panic' with the indicated message") \
  OP (opt_ok, "i", "invariant", 0, \
      "Execute a successful invariant test (no output).") \
  OP (opt_botched, "b", "botched", 0, \
      "Execute a failed invariant test (exit with a messsage).")


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

	case opt_panic:
	  panic (option->arg_string);
	  exit (0);

	case opt_ok:
	  invariant (1);
	  exit (0);

	case opt_botched:
	  {
	    int zero;
	    zero = 0;
	    invariant(zero + 0);
	    panic ("botched invariant returned");
	    break;
	  }
	}
    }
  return 0;
}



