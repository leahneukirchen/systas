/* unit-char-name.c - test char-name.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-name.h"
#include "hackerlab/cmd/main.h"
#include "hackerlab/tests/char-tests/unit-char-name.h"



static t_uchar * program_name = "unit-char-name";
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
	}
    }

  {
    unsigned int x;

    safe_printfmt (1, "static t_uchar test[] = \"");

    for (x = 0; x < 256; ++x)
      {
	safe_printfmt (1, "%s", char_name[x]);
      }

    safe_printfmt (1, "\";\n");
    
    for (x = 0; x < 256; ++x)
      {
	invariant (test[x] == x);
      }
  }
  return 0;
}



