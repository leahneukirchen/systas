/* opt.c - command-line option parsing
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/stddef.h"
#include "hackerlab/os/limits.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/cmd/opt.h"


/************************************************************************
 *(h0 "Parsing Command-line Options"
 *    :include ("hackerlab/cmd/opt.h"))
 *
 * The functions and macros in this chapter provide a convenient and
 * standard way to parse command line arguments and provide on-line
 * help for command-line invocation.  An example at the end of the
 * chapter illustrates their use.
 * 
 * It is important to understand that these functions are designed
 * to support *consistent* command line interfaces, rather than 
 * *arbitrary* command line interfaces.  Consequently, these functions
 * aren't especially useful for implementing some of the standard Posix 
 * command line syntaxes.  
 * 
 */
/*(menu)
 */

/*(include-documentation "opt.h")
 */

/************************************************************************
 *(h1 "Parsing Options")
 * 
 */

/*(c opt_low)
 * int opt_low (t_uchar * opt_string,
 *	        t_uchar * opt_arg,
 * 	        struct opt_desc ** desc,
 *	        struct opt_desc * opts,
 *	        int * argcp,
 *	        char ** argv,
 *	        t_uchar * opt_string_space);
 * 
 * Return the next command line option and modify argc/argv to 
 * remove that option.
 *
 * `opts' should point to an array of `struct opt_desc' whose final
 * element contains a value less than 0 in the field `opt_value'.
 *
 * Ordinarily `opt' returns the `opt_value' field from the element of
 * `opts' that matches the next argument.  If the field `requires_arg'
 * is not 0, then the option argument is also returned.  The option
 * and its argument are removed from argc/argv.  `opt_string' returns
 * a string naming the option ("-x", "--long-opt" or
 * "--long-opt=value").  The string returned in `opt_string' may be
 * overwritten by later calls to `opt'.  `opt_arg' returns
 * the argument to the option, if the argument requires an argument
 * and one was provided.  A string returned in `opt_arg' points to
 * a string or substring from `argv'.  `desc' returns the option
 * description for the option returned.
 * 
 * Any of `opt_string', `opt_arg', and `desc' may be 0.
 * 
 * `opt_string_space' should point to an array of three characters.
 * If the next option is a short option, the value stored in `*opt_string'
 * will point to `opt_string_space'.  `opt_string_space' may be 0
 * if `opt_string' is 0.
 *
 * If the option requires an argument but none is provided, `*opt_arg'
 * is set to 0, and the `opt_value' field of the option is returned;
 * argc/argv is not modified.  To see that an option with required
 * argument has been correctly provided, it is necessary to check both
 * the return value of `opt', and the value of `*opt_arg'.  This
 * behavior is to facilitate providing a specific error message to the
 * user -- to quote POSIX: "option arguments may not be optional".
 *
 * If the next argument is not an ordinary option, a value less than 0
 * is returned.  These special values are given symbolic names in
 * `opt.h' (`enum opt_return_values'):
 *
 * 		opt_dash	-- the next argument is simply "-".
 * 		opt_double_dash -- the next argument is simply "--".
 *
 * In either case, the argument ("-" or "--") is removed from
 * argc/argv.
 *
 * 		opt_unknown	-- the next argument does not match 
 *				   any known option. 
 *
 * 		opt_bogus_argument -- a recognized long option was 
 *				      provided with an option argument, 
 *				      but the option does not accept 
 *				      an argument.
 * 
 * 		opt_none	-- the next argument does not begin
 *				   with "-".
 *
 * In any of those cases, argc/argv is not modified.
 */
int
opt_low (t_uchar ** opt_string,
	 t_uchar ** opt_arg,
	 struct opt_desc ** desc,
	 struct opt_desc * opts,
	 int * argcp,
	 char ** argv,
	 t_uchar * opt_string_space)
{
  int argc;
  int choice;
  t_uchar ** argvx;

  argc = *argcp;
  argvx = (t_uchar **)argv;

  if (opt_string)
    *opt_string = 0;

  if (opt_arg)
    *opt_arg = 0;

  if (desc)
    *desc = 0;

  if (argc == 0)
    return opt_none;

  if (!argvx[1])
    return opt_none;

  if (argvx[1][0] != '-')
    return opt_none;

  if (argvx[1][1] != '-')
    {
      /* short option */
      if (opt_string)
	{
	  opt_string_space[0] = '-';
	  opt_string_space[1] = argvx[1][1];
	  opt_string_space[2] = 0;
	  *opt_string = opt_string_space;
	}
      if (!argvx[1][1])
	{
	  opt_shift (argcp, argv);
	  return opt_dash;
	}
      for (choice = 0; opts[choice].opt_value >= 0; ++choice)
	{
	  if (opts[choice].char_name)
	    {
	      if (opts[choice].char_name[0] == argvx[1][1])
		{
		  if (desc)
		    *desc = &opts[choice];
		  if (!opts[choice].requires_arg)
		    {
		      /* character option, no option argument. */
		      opt_shift_char_option (argcp, argv);
		      return opts[choice].opt_value;
		    }
		  if (argvx[1][2])
		    {
		      /* character option, with argument, "-xargument". */
		      if (opt_arg)
			*opt_arg = argvx[1] + 2;
		      opt_shift (argcp, argv);
		      return opts[choice].opt_value;
		    }
		  if (argc > 2)
		    {
		      /* character option, with argument, "-x argument". */
		      if (opt_arg)
			*opt_arg = argvx[2];
		      opt_shift (argcp, argv); /* shift away "-x" */
		      opt_shift (argcp, argv); /* shift away "argument" */
		      return opts[choice].opt_value;
		    }
		  {
		    /* character option, missing argument. */
		    if (opt_arg)
		      *opt_arg = 0;
		    return opts[choice].opt_value;
		  }
		}
	    }
	}
      /* unrecognized character option */
      return opt_unknown;
    }
  else
    {
      /* long option */
      t_uchar * opt_name;
      t_uchar * opt_end;
      int opt_length;
      t_uchar * eq_arg;

      if (!str_cmp (argvx[1], "--"))
	{
	  if (opt_string)
	    *opt_string = "--";
	  opt_shift (argcp, argv);
	  return opt_double_dash;
	}
      if (opt_string)
	*opt_string = argvx[1];
      opt_name = argvx[1] + 2;
      eq_arg = str_chr_index (opt_name, '=');
      if (eq_arg)
	opt_end = eq_arg;
      else
	opt_end = opt_name + str_length (opt_name);
      opt_length = opt_end - opt_name;

      for (choice = 0; opts[choice].opt_value >= 0; ++choice)
	{
	  t_uchar * spec_end;
	  int spec_length;

	  if (!opts[choice].long_name)
	    continue;

	  spec_end = str_chr_index (opts[choice].long_name, '=');
	  if (!spec_end)
	    spec_end = str_chr_index (opts[choice].long_name, ' ');
	  if (spec_end)
	    spec_length = spec_end - opts[choice].long_name;
	  else
	    spec_length = str_length (opts[choice].long_name);

	  if (   (opt_length <= spec_length)
	      && !str_cmp_n (opt_name, opt_length, opts[choice].long_name, spec_length)
	      && (   (opt_length == spec_length)
		  || char_is_space(opts[choice].long_name[opt_length])
		  || ('=' == opts[choice].long_name[opt_length])))
	    {
	      if (desc)
		*desc = &opts[choice];
	      if (!opts[choice].requires_arg)
		{
		  /* long option, no argument expected */
		  if (eq_arg)
		    {
		      /* command line contained an argument */
		      return opt_bogus_argument;
		    }
		  opt_shift (argcp, argv);
		  return opts[choice].opt_value;
		}
	      /* long option, argument expected */
	      if (eq_arg)
		{
		  /* --foo=bar */
		  if (opt_arg)
		    *opt_arg = eq_arg + 1;
		  opt_shift (argcp, argv);
		  return opts[choice].opt_value;
		}
	      if (argc > 2)
		{
		  /* --foo bar */
		  if (opt_arg)
		    *opt_arg = argvx[2];
		  opt_shift (argcp, argv);
		  opt_shift (argcp, argv);
		  return opts[choice].opt_value;
		}
	      {
		/* --foo, missing argument. */
		if (opt_arg)
		  *opt_arg = 0;
		return opts[choice].opt_value;
	      }
	    }
	}
      /* unrecognized long option */
      return opt_unknown;
    }
}

int
opt (alloc_limits limits,
     struct opt_parsed ** parsed_p,
     struct opt_desc * opts,
     int * argcp,
     char ** argv)
{
  t_uchar * opt_string;
  t_uchar * arg_string;
  t_uchar arg_string_space[MB_LEN_MAX * 3];
  struct opt_desc * desc;
  int o;
  struct opt_parsed * parsed;

  o = opt_low (&opt_string, &arg_string, &desc, opts, argcp, argv, arg_string_space);

  parsed = (struct opt_parsed *)lim_malloc_contiguous (limits,
						       sizeof (struct opt_parsed),
						       offsetof (struct opt_parsed, opt_string),
						       str_length (opt_string) + 1,
						       offsetof (struct opt_parsed, arg_string),
						       str_length (arg_string) + 1,
						       LIM_END);
  
  if (!parsed)
    return opt_allocation_failure;

  parsed->opt_value = o;

  if (!opt_string)
    parsed->opt_string = 0;
  else
    str_cpy (parsed->opt_string, opt_string);

  if (!arg_string)
    parsed->arg_string = 0;
  else
    str_cpy (parsed->arg_string, arg_string);

  parsed->desc = desc;

  if (*parsed_p)
    lim_free (limits, (void *)*parsed_p);

  *parsed_p = parsed;

  return o;
}


/*(c opt_standard)
 * int opt_standard (alloc_limits limits,
 *                   struct opt_parsed ** parsed_p,
 *                   struct opt_desc * opts,
 *                   int * argcp,
 *                   char * argvp[],
 *                   t_uchar * program_name,
 *                   t_uchar * usage,
 *                   t_uchar * version_string,
 *                   t_uchar * long_help,
 *                   int help_option,
 *                   int long_help_option,
 *                   int version_option);
 * 
 * This function calls `opt', but handles some commonly
 * implemented options internally, in a standard way.
 * 
 * The parameters `limits', `parsed_p', `opts', `argcp',
 * and `argvp' are as to `opt_low'.
 * 
 * The parameters `program_name', `usage' and `version_string' are
 * used in error and informational messages printed by this function.
 * 
 * The parameters `help_option', `long_help_option' and `version_option' 
 * are the `name' value for some standard options, which are
 * usually given the command line names:
 * 
 * 	-h	--help		help_option
 * 	-H			long_help_option
 * 	-V	--version	version_option
 * 
 * If this function detects the `help_option', it prints a 
 * multi-line message summarizing the available options
 * to standard output and exits the process with a 0 status.
 * 
 * If this function detects the `long_help_option', it prints a 
 * multi-line message summarizing the available options
 * to standard output, adds text from `long_help' and exits the 
 * process with a 0 status.  The first line of `long_help' should
 * summarize the command (conventionally uncapitalized and without
 * punctuation).  The rest of `long_help' should expand upon the
 * explanation.  Long help messages are formatted as:
 * 
 *	first line of long_help
 *	usage: program_name ....
 *	option list
 * 
 *	rest of long_help
 * 
 * If this function detects the `version_option', it prints a 
 * single-line message containing `version_string'
 * to standard output and exits the process with a 0 status.
 * 
 * If this function detects an unrecognized option, a missing option
 * argument, or an argument provided for an option that doesn't accept
 * arguments, it prints a usage message to standard error and exits
 * the process with a non-0 status.
 */
int
opt_standard (alloc_limits limits,
	      struct opt_parsed ** parsed_p,
	      struct opt_desc * opts,
	      int * argc,
	      char * argv[],
	      t_uchar * program_name,
	      t_uchar * usage,
	      t_uchar * version_string,
	      t_uchar * long_help,
	      int help_option,
	      int long_help_option,
	      int version_option)
{
  int o;

  o = opt (limits, parsed_p, opts, argc, argv);

  if (o == opt_none)
    return o;

  if ((*parsed_p)->desc && (*parsed_p)->desc->requires_arg && !(*parsed_p)->arg_string)
    {
      safe_printfmt (2, "missing argument for `%s'\n", (*parsed_p)->opt_string);
      opt_usage (2, argv[0], program_name, usage, 1);
      panic_exit ();
    }

  if (o == help_option)
    {
      if (long_help)
	{
	  t_uchar * nl;

	  nl = str_chr_index (long_help, '\n');
	  if (nl)
	    {
	      safe_printfmt (1, "%.*s\n", nl - long_help, long_help);
	    }
	}
      opt_usage (1, argv[0], program_name, usage, 0);
      safe_printfmt (1, "\n");
      opt_help (1, opts);
      safe_printfmt (1, "\n");
      exit (0);
    }

  if (o == long_help_option)
    {
      t_uchar * nl;

      nl = str_chr_index (long_help, '\n');
      if (nl)
	safe_printfmt (1, "%.*s\n", nl - long_help, long_help);
      opt_usage (1, argv[0], program_name, usage, 0);
      safe_printfmt (1, "\n");
      opt_help (1, opts);
      safe_printfmt (1, "\n");
      if (nl)
	safe_printfmt (1, "%s", nl + 1);
      else
	safe_printfmt (1, "%s", long_help);
      safe_printfmt (1, "\n");
      exit (0);
    }

  if (o == version_option)
    {
      safe_printfmt (1, "%s %s\n", program_name, version_string);
      exit (0);
    }

  if (o == opt_allocation_failure)
    {
      panic ("allocation failure while parsing arguments");
    }

  switch (o)
    {
    case opt_dash:
      return o;

    case opt_double_dash:
      return o;

    case opt_unknown:
      safe_printfmt (2, "unhandled option `%s'\n", argv[1]);
      opt_usage (2, argv[0], program_name, usage, 1);
      panic_exit ();

    case opt_bogus_argument:
      safe_printfmt (2, "option `%s' does not accept an argument\n", argv[1]);
      opt_usage (2, argv[0], program_name, usage, 1);
      panic_exit ();

    default:
      if (o < 0)
	{
	  safe_printfmt (2, "illegal option code %d for `%s'\n", o, argv[1]);
	  panic ("internal error processing options");
	}
      return o;
    }
}




/************************************************************************
 *(h1 "Printing Help Messages")
 * 
 */


/*(c opt_usage)
 * void opt_usage (int fd,
 *		   char * argv0,
 *                 t_uchar * program_name,
 *                 t_uchar * usage,
 *                 int suggest_help);
 * 
 * Print a usage message on `fd' using the format:
 * 
 *	"usage: %s %s\n", program_name, usage
 * 
 * If `suggest_help' is not 0, also print:
 * 
 * 	"try %s --help\n", argv0
 * 
 * By convention, `argv0' should be the first command line string
 * passed to the program (the name by which the program was invoked).
 * 
 * `program_name' should be the default name for the program (not
 * necessarily the name by which it was invoked).
 */
void
opt_usage (int fd,
	   char * argv0,
	   t_uchar * program_name,
	   t_uchar * usage,
	   int suggest_help)
{
  safe_printfmt (fd, "usage: %s %s\n", program_name, usage);

  if (suggest_help)
    safe_printfmt (fd, "try %s --help\n", argv0);
}


/*(c opt_help)
 * void opt_help (int fd, struct opt_desc * opts);
 * 
 * Print a help message based on `opts'.
 *
 * A long option name in `opts' can have the form "--foo=BAR".
 * The text "=BAR" is included in the help message.
 */
void
opt_help (int fd, struct opt_desc * opts)
{
  int x;
  int max_char_opt_len;
  int max_long_opt_len;
  int max_desc_len;
  int desc_column;
  t_uchar * buf;

  max_long_opt_len = 0;
  max_char_opt_len = 0;
  max_desc_len = 0;
  for (x = 0; opts[x].opt_value >= 0; ++x)
    {
      int q;
      if (opts[x].char_name)
	{
	  q = 1 + str_length (opts[x].char_name);
	  if (q > max_char_opt_len)
	    max_char_opt_len = q;
	}
      if (opts[x].long_name)
	{
	  q = 2 + str_length (opts[x].long_name);
	  if (q > max_long_opt_len)
	    max_long_opt_len = q;
	}
      if (opts[x].desc)
	{
	  q = str_length (opts[x].desc);
	  if (q > max_desc_len)
	    max_desc_len = q;
	}
    }

  desc_column = 2 + max_char_opt_len + 2 + max_long_opt_len + 2;
  buf = (t_uchar *)alloca (desc_column + max_desc_len + 1);

  for (x = 0; opts[x].opt_value >= 0; ++x)
    {
      int column;
      int amt;

      buf[0] = 0;
      str_cat (buf, "  ");
      if (opts[x].char_name)
	{
	  str_cat (buf, "-");
	  str_cat (buf, opts[x].char_name);
	  str_cat (buf, ", ");
	}
      column = str_length (buf);
      if (opts[x].long_name)
	{
	  str_cat (buf, "--");
	  str_cat (buf, opts[x].long_name);
	  column = str_length (buf);
	}
      while (column < desc_column)
	buf[column++] = ' ';
      buf[column] = 0;
      if (opts[x].desc)
	str_cat (buf, opts[x].desc);
      str_cat (buf, "\n");
      amt = str_length (buf);
      safe_write_retry (fd, buf, amt);
    }
}


/************************************************************************
 *(h1 "Shifting Arguments")
 * 
 */

/*(c opt_shift_char_option)
 * void opt_shift_char_option (int * argcp, char ** argv);
 * 
 * Remove a single character argument from argc/argv.  For example,
 *
 * Before:
 *
 *	*argcp = 3
 *	argv = {"prog", "-abc", "-def", 0}
 *
 * After:
 *
 *	*argcp = 3
 *	argv = {"prog", "-bc", "-def", 0}
 *
 * Before:
 *
 *	*argcp = 3
 *	argv = {"prog", "-a", "-def", 0}
 *
 * After:
 *
 *	*argcp = 2
 *	argv = {"prog", "-def", 0}
 *
 */
void
opt_shift_char_option (int * argcp, char ** argv)
{
  t_uchar ** argvx;

  argvx = (t_uchar **)argv;
  if (!argvx[1][2])
    opt_shift (argcp, argv);
  else
    mem_move (argvx[1] + 1, argvx[1] + 2, 1 + str_length (argvx[1] + 2));
}


/*(c opt_shift)
 * void opt_shift (int * argcp, char ** argv);
 * 
 * Remove a single argument from argc/argv.  For example,
 *
 * Before:
 *
 *	*argcp = 3
 *	argv = {"prog", "--abc", "-def", 0}
 * 
 * After:
 *
 *	*argcp = 2
 *	argv = {"prog", "-def", 0}
 * 
 */
void
opt_shift (int * argcp, char ** argv)
{
  int argc;
  int x;
  t_uchar ** argvx;

  argc = *argcp;
  argvx = (t_uchar **)argv;
  --argc;
  for (x = 1; x < argc; ++x)
    argvx[x] = argvx[x + 1];
  argvx[x] = 0;
  *argcp = argc;
}



#if 0
/************************************************************************
 *(h1 "An Example of Option Parsing ")
 * 
 * The following program illustrates option parsing with `opt'.
 * These examples illustrates its usage:
 * 
 * 	% test --version
 *	test 1.0.0
 *
 * 	% test --help
 *      usage: ./,test [options] [input-file]
 *
 *        -h, --help                   Display a help message and exit.
 *        -V, --version                Display a release identifier string
 *                                     and exit.
 *        -o file, --output-file=file  Write output to FILE.
 *
 *      % ./,test -o one --output-file two -othree --output-file=four
 *      output file argument: `one'
 *      output file argument: `two'
 *      output file argument: `three'
 *      output file argument: `four'
 * 
 *      % ./,test -xyzzy
 *      unhandled option `-xyzzy'
 *      usage: ./,test [options] [input-file]
 *      try ./,test --help
 *
 * 
 * Here's the code:
 * 
 insert*/

#include "hackerlab/cmd/main.h"




static t_uchar * program_name = "test";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";


#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_output_file, "o file", "output-file=file", 1, \
      "Write output to FILE.")

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
  int o;
  struct opt_parsed * option;

  option = 0;

  while (1)
    {
      o = opt_standard (lim_use_must_malloc,
			&option,
			opts,
			&argc, argv,
			program_name,
			usage,
			version_string,
			opt_help_msg,
			opt_version);
      if (o == opt_none)
	break;
      switch (o)
	{
	default:
	  safe_printfmt (2, "unhandled option `%s'\n",
			 option->opt_string);
	  panic ("internal error parsing arguments");

	usage_error:
	  opt_usage (2, argv[0], program_name, usage, 1);
	  panic_exit ();

	bogus_arg:
	  safe_printfmt (2, "ill-formed argument for `%s' (`%s')\n",
			 option->opt_string,
			 option->arg_string);
	  goto usage_error;

	case opt_output_file:
	  if (!opt_arg)
	    {
	      printfmt (&errn,
			2,
			"missing argument for `%s'\n",
			opt_string);
	      goto usage_error;
	    }
	  printfmt (&errn,
		    2,
		    "output file argument: `%s'\n",
		    opt_arg);
	  break;
	}
    }
  return 0;
}
/*end-insert
 */

#endif
