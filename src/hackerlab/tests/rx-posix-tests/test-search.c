/* tag: Tom Lord Tue Dec  4 14:41:16 2001 (test-search.c)
 */
/* test-search.c -
 *
 ****************************************************************
 * Copyright (C) YEAR  NAME
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "test-search";
static t_uchar * usage = "[options] regexp file";
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



void
panic_exit (void)
{
  exit (2);
}

int
main (int argc, char * argv[])
{
  int o;
  struct opt_parsed * option;
  regex_t regexp;
  int fd;
  t_uchar * buf;
  size_t len;
  int exec_stat;
  regmatch_t match_pos_space[1];
  regmatch_t * match_pos = match_pos_space;

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
	}
    }

  if (argc != 3)
    goto usage_error;

  if (regcomp (&regexp, argv[1], REG_EXTENDED | REG_NEWLINE))
    {
      safe_printfmt (2, "unable to compile regexp\n");
      panic_exit ();
    }

  fd = safe_open (argv[2], O_RDONLY, 0);
  safe_file_to_string (&buf, &len, fd);

  exec_stat = regnexec (&regexp, buf, len, 1, &match_pos, 0);

  if (exec_stat == REG_NOMATCH)
    {
      safe_printfmt (1, "no match found\n");
      exit (1);
    }
  else if (exec_stat)
    {
      safe_printfmt (2, "regexec error (%d)\n", exec_stat);
      panic ("regexec failure");
    }
  else
    {
      size_t newlines;
      t_uchar * beg;
      t_uchar * end;

      newlines = mem_occurrences (buf, '\n', match_pos[0].rm_so);
      end = str_chr_index (buf + match_pos[0].rm_so, '\n');
      if (!end)
	end = buf + len;
      beg = str_chr_rindex_n (buf, match_pos[0].rm_so, '\n');
      if (!beg)
	beg = buf;
      beg = beg + 1;
      safe_printfmt (1, "%d:%.*s\n", newlines + 1, (int)(end - beg), beg);
    }
  return 0;
}

