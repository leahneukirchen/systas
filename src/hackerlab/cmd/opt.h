/* opt.h - decls for command-line option parsing
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__CMD__OPT_H
#define INCLUDE__CMD__OPT_H


#include "hackerlab/machine/types.h"
#include "hackerlab/mem/alloc-limits.h"


/************************************************************************
 *(h1 "Option Tables")
 * 
 * 
 * 
 */


/*(c opt_desc :category type)
 insert*/
struct opt_desc
{
  int opt_value;
  t_uchar * char_name;
  t_uchar * long_name;
  int requires_arg;
  t_uchar * desc;
};
/*end-insert
 */

/*(c opt_parsed :category type)
 insert*/
struct opt_parsed
{
  int opt_value;
  t_uchar * opt_string;
  t_uchar * arg_string;
  struct opt_desc * desc;
};
/*end-insert
 */

#if 0
/*(text)
 *
 * An array of `struct opt_desc' is used to describe the options
 * accepted by a program.
 *
 * The easiest way to create this array is by defining a macro `OPTS'
 * of two arguments: `OP', which is used to define a program option
 * and `OP2', which is used to add additional lines of documentation
 * to an option.
 *
 * Both `OP' and `OP2' are used as macros which accept 5 arguments:
 *
 *	name		an enum name for the option
 *	char_name	0 or a string beginning with a one-character 
 *			name for the option.  
 *	long_name	0 or a string beginning with a long name for 
 *			the option
 * 	arg		1 if the option requires an argument, 0 
 *			otherwise
 *	desc		A documentation string for the option
 *
 * Once you have defined `OPTS' it is easy to create a table of
 * `struct opt_desc', as in the following example:
 *
 insert*/

#define OPTS(OP, OP2) \
  OP (opt_help_msg,	/* An enum name for the option */ \
      "h",		/* A short name for the option */ \
      "help",		/* A long name for the option */ \
      0,		/* The option takes no arguments */ \
      "Display a help message and exit.") /*  help message */ \
  \
  OP (opt_version, "V", "version", 0, \
	   "Display a release identifier string and exit.") \
  \
  /* The next option illustrates how to handle a multi-line */ \
  /* help message: */ \
  \
  OP (opt_output_file, "o file", "output-file=file", 1, \
	   "Write output to FILE.") \
  OP2 (opt_output_file, 0, 0, 1, \
	    "The file must not already exist.") \
  OP (...) ... 

/* Note that the short and long names for an option are optional
 * (may be 0) but if both are omitted, then there is no way to
 * specify the option on a command line.
 */

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};
/*end-insert
 */
#endif

#define OPT_ENUM(name,char_name,long_name,arg,desc)	name,
#define OPT_DESC(name,char_name,long_name,arg,desc)	{name,char_name,long_name,arg,desc},
#define OPT_IGN(name,char_name,long_name,arg,desc)




/* See `opt' in `opt.c'. 
 */
enum opt_return_values
{
  opt_dash = -1,		/* "-" */
  opt_double_dash = -2,		/* "--" */
  opt_unknown = -3,		/* unrecognized option */
  opt_bogus_argument = -4,	/* --foo=bar but foo does not take an argument */
  opt_none = -5,		/* next argument is not an option */
  opt_allocation_failure = -6	/* from op */
};



/* automatically generated __STDC__ prototypes */
extern int opt_low (t_uchar ** opt_string,
		    t_uchar ** opt_arg,
		    struct opt_desc ** desc,
		    struct opt_desc * opts,
		    int * argcp,
		    char ** argv,
		    t_uchar * opt_string_space);
extern int opt (alloc_limits limits,
		struct opt_parsed ** parsed_p,
		struct opt_desc * opts,
		int * argcp,
		char ** argv);
extern int opt_standard (alloc_limits limits,
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
			 int version_option);
extern void opt_usage (int fd,
		       char * argv0,
		       t_uchar * program_name,
		       t_uchar * usage,
		       int suggest_help);
extern void opt_help (int fd, struct opt_desc * opts);
extern void opt_shift_char_option (int * argcp, char ** argv);
extern void opt_shift (int * argcp, char ** argv);
extern int main (int argc, char * argv[]);
#endif  /* INCLUDE__CMD__OPT_H */
