/* tag: Tom Lord Tue Dec  4 14:39:39 2001 (unit-test-coverage.c)
 */
/* unit-test-coverage.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/test-coverage.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-test-coverage";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_skip_a, "a", "skip-a", 0, \
      "Skip coverage point A.") \
  OP (opt_skip_b, "b", "skip-b", 0, \
      "Skip coverage point B.") \


enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



#define TEST_COVERAGE_LIST(MACRO) \
	TEST_COVERAGE_ ## MACRO (point_a); \
	TEST_COVERAGE_ ## MACRO (point_b)

TEST_COVERAGE_LIST(DECL);

int
main (int argc, char * argv[])
{
  int o;
  struct opt_parsed * option;
  int skip_a;
  int skip_b;

  option = 0;
  skip_a = 0;
  skip_b = 0;

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

	case opt_skip_a:
	  skip_a = 1;
	  break;

	case opt_skip_b:
	  skip_b = 1;
	  break;
	}
    }

  if (!skip_a)
    TEST_COVERED(point_a);

  if (!skip_b)
    TEST_COVERED(point_b);

  TEST_COVERAGE_LIST(CHECK);

  return 0;
}

