/* unit-cvt.c - test cvt.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/cmd/main.h"





static t_uchar * program_name = "unit-cvt";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";



#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_sizeof_long, 0, "sl", 0, \
      "Print the number of bytes in a long integer.") \
  OP (opt_sizeof_int, 0, "si", 0, \
      "Print the number of bytes in an integer.") \
  OP (opt_ulong_decimal, 0, "uld N", 1, \
      "Decimal number to unsigned long.") \
  OP (opt_long_decimal, 0, "ld N", 1, \
      "Decimal number to long.") \
  OP (opt_uint_decimal, 0, "ud N", 1, \
      "Decimal number to unsigned integer.") \
  OP (opt_int_decimal, 0, "d N", 1, \
      "Decimal number to integer.") \
  OP (opt_ulong_hex, 0, "ulx N", 1, \
      "Hexadecimal number to unsigned long.") \
  OP (opt_long_hex, 0, "lx N", 1, \
      "Hexadecimal number to long.") \
  OP (opt_uint_hex, 0, "ux N", 1, \
      "Hexadecimal number to unsigned integer.") \
  OP (opt_int_hex, 0, "x N", 1, \
      "Hexadecimal number to integer.") \
  OP (opt_ulong_HEX, 0, "ulX N", 1, \
      "Hexadecimal number to unsigned long.") \
  OP (opt_long_HEX, 0, "lX N", 1, \
      "Hexadecimal number to long.") \
  OP (opt_uint_HEX, 0, "uX N", 1, \
      "Hexadecimal number to unsigned integer.") \
  OP (opt_int_HEX, 0, "X N", 1, \
      "Hexadecimal number to integer.") \
  OP (opt_ulong_octal, 0, "ulo N", 1, \
      "Octal number to unsigned long.") \
  OP (opt_long_octal, 0, "lo N", 1, \
      "Octal number to long.") \
  OP (opt_uint_octal, 0, "uo N", 1, \
      "Octal number to unsigned integer.") \
  OP (opt_int_octal, 0, "o N", 1, \
      "Octal number to integer.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



static long
cast_long (long * x)
{
  return * x;
}

static long
cast_int (int * x)
{
  return (long)*x;
}

static long
cast_uint (unsigned int * x)
{
  return (long)((unsigned long)*x);
}

int
main (int argc, char * argv[])
{
  int o;
  struct opt_parsed * option;

  option = 0;

  while (1)
    {
      int errn;
      unsigned long ul;
      unsigned int ui;
      long l;
      int i;
      void * answer;
      int (*cvt_fn)();
      void (*cvt_back_fn)();
      long (*cast_fn)();

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

	case opt_sizeof_long:
	  printfmt (&errn, 1, "%d\n", sizeof (long));
	  break;

	case opt_sizeof_int:
	  printfmt (&errn, 1, "%d\n", sizeof (int));
	  break;

	case opt_ulong_decimal:
	  answer = (void *)&ul;
	  cvt_fn = cvt_decimal_to_ulong;
	  cvt_back_fn = cvt_ulong_to_decimal;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_long_decimal:
	  answer = (void *)&l;
	  cvt_fn = cvt_decimal_to_long;
	  cvt_back_fn = cvt_long_to_decimal;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_uint_decimal:
	  answer = (void *)&ui;
	  cvt_fn = cvt_decimal_to_uint;
	  cvt_back_fn = cvt_ulong_to_decimal;
	  cast_fn = cast_uint;
	  goto generic_cvt_test;

	case opt_int_decimal:
	  answer = (void *)&i;
	  cvt_fn = cvt_decimal_to_int;
	  cvt_back_fn = cvt_long_to_decimal;
	  cast_fn = cast_int;
	  goto generic_cvt_test;

	case opt_ulong_hex:
	  answer = (void *)&ul;
	  cvt_fn = cvt_hex_to_ulong;
	  cvt_back_fn = cvt_ulong_to_hex;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_long_hex:
	  answer = (void *)&l;
	  cvt_fn = cvt_hex_to_long;
	  cvt_back_fn = cvt_long_to_hex;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_uint_hex:
	  answer = (void *)&ui;
	  cvt_fn = cvt_hex_to_uint;
	  cvt_back_fn = cvt_ulong_to_hex;
	  cast_fn = cast_uint;
	  goto generic_cvt_test;

	case opt_int_hex:
	  answer = (void *)&i;
	  cvt_fn = cvt_hex_to_int;
	  cvt_back_fn = cvt_long_to_hex;
	  cast_fn = cast_int;
	  goto generic_cvt_test;

	case opt_ulong_HEX:
	  answer = (void *)&ul;
	  cvt_fn = cvt_hex_to_ulong;
	  cvt_back_fn = cvt_ulong_to_HEX;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_long_HEX:
	  answer = (void *)&l;
	  cvt_fn = cvt_hex_to_long;
	  cvt_back_fn = cvt_long_to_HEX;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_uint_HEX:
	  answer = (void *)&ui;
	  cvt_fn = cvt_hex_to_uint;
	  cvt_back_fn = cvt_ulong_to_HEX;
	  cast_fn = cast_uint;
	  goto generic_cvt_test;

	case opt_int_HEX:
	  answer = (void *)&i;
	  cvt_fn = cvt_hex_to_int;
	  cvt_back_fn = cvt_long_to_HEX;
	  cast_fn = cast_int;
	  goto generic_cvt_test;


	case opt_ulong_octal:
	  answer = (void *)&ul;
	  cvt_fn = cvt_octal_to_ulong;
	  cvt_back_fn = cvt_ulong_to_octal;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_long_octal:
	  answer = (void *)&l;
	  cvt_fn = cvt_octal_to_long;
	  cvt_back_fn = cvt_long_to_octal;
	  cast_fn = cast_long;
	  goto generic_cvt_test;

	case opt_uint_octal:
	  answer = (void *)&ui;
	  cvt_fn = cvt_octal_to_uint;
	  cvt_back_fn = cvt_ulong_to_octal;
	  cast_fn = cast_uint;
	  goto generic_cvt_test;

	case opt_int_octal:
	  answer = (void *)&i;
	  cvt_fn = cvt_octal_to_int;
	  cvt_back_fn = cvt_long_to_octal;
	  cast_fn = cast_int;
	  goto generic_cvt_test;

	  
	generic_cvt_test:
	  if (cvt_fn (&errn, answer, option->arg_string, str_length (option->arg_string)))
	    printfmt (&errn, 1, "%s\n", ((errn == EINVAL)
					 ? "EINVAL"
					 : ((errn == ERANGE)
					    ? "ERANGE"
					    : "unexpected errno value")));
	  else
	    {
	      t_uchar buf[100];
	      
	      cvt_back_fn (buf, cast_fn (answer));
	      printfmt (&errn, 1, "%s\n", buf);
	    }
	  break;
	}
    }
  return 0;
}



