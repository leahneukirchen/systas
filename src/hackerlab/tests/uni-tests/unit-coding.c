/* tag: Tom Lord Tue Dec  4 14:41:21 2001 (unit-coding.c)
 */
/* unit-coding.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/uni/coding.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-coding";
static t_uchar * usage = "[options] input-file";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_cvt_8, "8", "8-to-16", 0, \
      "Convert UTF-8 to UTF-16 (default is UTF-16 to UTF-8")

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
  int from_8;
  int o;
  struct opt_parsed * option;

  from_8 = 0;
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

	case opt_cvt_8:
	  from_8 = 1;
	  break;
	}
    }

  if (argc != 1)
    goto usage_error;

  {
    t_uchar * file_contents;
    size_t len;
    size_t file_pos;

    t_uchar * converted_text;
    size_t allocated;
    size_t converted_pos;

    if (vu_file_to_string (&errn, &file_contents, &len, 0))
      panic ("error reading file");


    allocated = len * 6;
    converted_text = (t_uchar *)must_malloc (allocated);

    converted_pos = 0;
    file_pos = 0;

    if (from_8)
      {
	if (uni_utf8_to_utf16 (converted_text, &converted_pos, allocated,
			       file_contents, &file_pos, len))
	  panic ("conversion failure");
	if (converted_pos != vu_write_retry (&errn, 1, converted_text, converted_pos))
	  panic ("write failure");
      }
    else
      {
	if (uni_utf16_to_utf8 (converted_text, &converted_pos, allocated,
			       file_contents, &file_pos, len))
	  panic ("conversion failure");
	if (converted_pos != vu_write_retry (&errn, 1, converted_text, converted_pos))
	  panic ("write failure");
      }
  }
  return 0;
}



