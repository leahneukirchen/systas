/* unit-char-class.c - test char-class.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/char-name.h"
#include "hackerlab/cmd/main.h"
#include "hackerlab/tests/char-tests/unit-char-class.h"



static t_uchar * program_name = "unit-char-class";
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
    int x;
    int count;

#define CATEGORIES(FN) \
	CATEGORY_##FN (char_is_ascii) \
	CATEGORY_##FN (char_is_upper) \
	CATEGORY_##FN (char_is_lower) \
	CATEGORY_##FN (char_is_alpha) \
	CATEGORY_##FN (char_is_digit) \
	CATEGORY_##FN (char_is_alnum) \
	CATEGORY_##FN (char_is_control) \
	CATEGORY_##FN (char_is_printable) \
	CATEGORY_##FN (char_is_space) \
	CATEGORY_##FN (char_is_graph) \
	CATEGORY_##FN (char_is_c_id) \
	CATEGORY_##FN (char_is_xdigit) \
	CATEGORY_##FN (char_is_odigit) \
	CATEGORY_##FN (char_is_punct) \
	CATEGORY_##FN (char_is_blank) 

#define CATEGORY_COGEN(FN) \
    safe_printfmt (1, "static t_uchar test_%s[] = \"", (#FN)); \
    for (x = 0; x < 256; ++x) \
      { \
	if (FN (x)) \
	  safe_printfmt (1, "%s", char_name[x]); \
      } \
    safe_printfmt (1, "\";\n");
  
  CATEGORIES(COGEN);

#define CATEGORY_TEST(FN) \
    for (x = 0; x < (sizeof (test_##FN) - 1); ++x) \
      { \
 	int y; \
	y = test_##FN [x]; \
	invariant (FN (y)); \
      } \
    count = 0; \
    for (x = 0; x < 256; ++x) \
      { \
        if (FN (x)) \
	  ++count; \
      } \
    invariant ((sizeof (test_##FN) - 1) == count);

  CATEGORIES(TEST);

    for (x = 0; x < 256; ++x)
      {
	if (!char_is_alpha (x))
	  {
	    invariant (char_to_lower (x) == x);
	    invariant (char_to_lower (x) == char_to_upper (x));
	  }
	else if (char_is_upper (x))
	  {
	    invariant (char_to_lower (x) != x);
	    invariant (char_to_upper (x) == x);
	    invariant (char_to_upper (char_to_lower (x)) == x);
	    invariant ((x - 'A') == (char_to_lower (x) - 'a'));
	  }
	else if (char_is_lower (x))
	  {
	    invariant (char_to_lower (x) == x);
	    invariant (char_to_upper (x) != x);
	    invariant (char_to_lower (char_to_upper (x)) == x);
	    invariant ((x - 'a') == (char_to_upper (x) - 'A'));
	  }
	else
	  panic ("unrecognized character");
      }

    {
      char * h0 = "0123456789abcdefghijklmnopqrstuvwxyz";
      char * h1 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

      for (x = 0; x < 36; ++x)
	{
	  invariant (x == char_digit_value (h0[x]));
	  invariant (x == char_digit_value (h1[x]));
	}

      for (x = 0; x < 256; ++x)
	{
	  invariant (char_is_alnum (x) || (char_digit_value (x) == -1));
	}
    }
  }
  return 0;
}



