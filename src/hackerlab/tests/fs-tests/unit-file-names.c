/* unit-file-names.c - test file-names.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/fs/file-names.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-file-names.c";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";


#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_home, 0, "home", 0, \
      "Display the users home directory") \
  OP (opt_is_absolute, "a", "is-absolute file-name", 1, \
      "Output `yes' if `file-name' is absolute, `no' otherwise.") \
  OP (opt_expand, "x", "expand file-name", 1, \
      "Display `file-name', expanded.") \
  OP (opt_tail, "t", "tail file-name", 1, \
      "Display the tail of `file-name'.") \
  OP (opt_as_directory, "D", "as-directory file-name", 1, \
      "Display `file-name' as a directory name.") \
  OP (opt_file_name_from_directory, "F", "directory-file-name dir-name", 1, \
      "Display `dir-name' as a file name.") \
  OP (opt_file_name_directory, "d", "file-name-directory file-name", 1, \
      "Display the directory part of `file-name'.") \
  OP (opt_file_name_in_vicinity, "v", "in-vicinity dir", 1, \
      "Display \"some-file\" and \"some/file\" in-vicinity of `dir'.") \
  OP (opt_path, "p", "path path", 1, \
      "Parse `path' and display a list of its elements.") \
  OP (opt_which, "w", "which executable", 1, \
      "Find `executable' on $PATH and display its location.")

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

	case opt_home:
	  safe_printfmt (1, "%s\n", file_name_home_directory ());
	  break;

	case opt_is_absolute:
	  safe_printfmt (1, "%s\n", file_name_is_absolute (option->arg_string) ? "yes" : "no");
	  break;
	  
	case opt_expand:
	  {
	    t_uchar * f;
	    
	    f = file_name_tilde_expand (lim_use_must_malloc, option->arg_string);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    break;
	  }

	case opt_tail:
	  {
	    t_uchar * f;

	    f = file_name_tail (lim_use_must_malloc, option->arg_string);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    break;
	  }

	case opt_as_directory:
	  {
	    t_uchar * f;

	    f = file_name_as_directory (lim_use_must_malloc, option->arg_string);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    break;
	  }

	case opt_file_name_from_directory:
	  {
	    t_uchar * f;

	    f = file_name_from_directory (lim_use_must_malloc, option->arg_string);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    break;
	  }

	case opt_file_name_directory:
	  {
	    t_uchar * f;

	    f = file_name_directory (lim_use_must_malloc, option->arg_string);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    break;
	  }

	case opt_file_name_in_vicinity:
	  {
	    t_uchar * f;

	    if (argc < 2)
	      {
		safe_printfmt (2, "missing second argument to --in-vicinity\n");
		opt_usage (2, argv[0], program_name, usage, 1);
		panic_exit ();
	      }
	    
	    f = file_name_in_vicinity (lim_use_must_malloc, option->arg_string, argv[1]);
	    safe_printfmt (1, "%s\n", f);
	    lim_free (lim_use_must_malloc, f);
	    opt_shift (&argc, argv);
	    break;
	  }

	case opt_path:
	  {
	    t_uchar ** f;
	    int x;

	    f = path_parse (lim_use_must_malloc, option->arg_string);
	    for (x = 0; f && f[x]; ++x)
	      safe_printfmt (1, "%s\n", f[x]);
	    free_path (lim_use_must_malloc, f);
	    break;
	  }

	case opt_which:
	  {
	    t_uchar ** f;
	    t_uchar * x;

	    f = path_parse (lim_use_must_malloc, getenv ("PATH"));
	    x = path_find_executable (lim_use_must_malloc, f, option->arg_string);
	    if (!x)
	      safe_printfmt (1, "%s not found\n", option->arg_string);
	    else
	      safe_printfmt (1, "%s\n", x);
	    free_path (lim_use_must_malloc, f);
	    lim_free (lim_use_must_malloc, x);
	    break;
	  }
	}
    }
  return 0;
}



