/* config-info.c
 *
 ****************************************************************
 * Copyright (C) 2001, 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <stdio.h>
#include "config-options.h"


static int
string_equal_p (char * a, char * b)
{
  while (*a && *b)
    {
      if (*a != *b)
	return 0;
      ++a;
      ++b;
    }

  return !*a && !*b;
}


static int
standard_option (char * name)
{
  if (   (name[0] == 'c')
      && (name[1] == 'f')
      && (name[2] == 'g')
      && (name[3] == '_')
      && (name[4] == '_')
      && (name[5] == 's')
      && (name[6] == 't')
      && (name[7] == 'd')
      && (name[8] == '_')
      && (name[9] == '_'))
    return 1;
  else
    return 0;
}


static void
print_option_name (char * name)
{
  if (   (name[0] == 'c')
      && (name[1] == 'f')
      && (name[2] == 'g')
      && (name[3] == '_')
      && (name[4] == '_'))
    {
      name += 5;
      if (   (name[0] == 's')
	  && (name[1] == 't')
	  && (name[2] == 'd')
	  && (name[3] == '_')
	  && (name[4] == '_'))
	name += 5;
    }

  while (*name)
    {
      putchar ((*name == '_') ? '-' : *name);
      ++name;
    }
}


int
main (int argc, char * argv[])
{
  if (argc == 0)
    {
      printf ("ERROR: argc is 0!\n");
      exit (1);
    }

  if (argc == 1)
    {
       printf ("%s\n", cfg__std__prefix);
      exit (0);
    }

  if (string_equal_p (argv[1], "--help") || string_equal_p (argv[1], "-h"))
    {
      printf ("usage: %s [option]\n", argv[0]);
      puts ("");
      puts ("-h --help                  print the message");
      puts ("-V --version               print version information");
      puts ("");
      puts ("   --package               print the package name");
      puts ("   --options               print all compile-time options");
      puts ("");
      puts ("   --is-compatible relase  exit with status 1 or 0 depending");
      puts ("                           on whether or not this release is");
      puts ("                           purportedly compatible with RELEASE?");
      puts ("");
      puts ("   --compatible            print information about what other");
      puts ("                           packages this version is purportedly");
      puts ("                           compatible with");
      puts ("");
      puts ("At most one option may be specified.");
      puts ("");
      puts ("With no options, print the install prefix.");
      puts ("");
      exit (0);
    }
  
  if (string_equal_p (argv[1], "--version") || string_equal_p (argv[1], "-V"))
    {
      puts ("config-info " cfg__std__package " from regexps.com");
      puts ("");
      puts ("Copyright (C) 2001, 2002 Tom Lord");
      puts ("");
      puts ("This is free software; see the source for copying conditions.");
      puts ("There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A");
      puts ("PARTICULAR PURPOSE.");
      puts ("");
      puts ("Report bugs to <" cfg__bug_mail ">.");
      puts ("");
      exit (0);
    }

  if (string_equal_p (argv[1], "--package"))
    {
      if (argc != 2)
	{
	  fprintf (stderr, "usage: %s --package\n", argv[0]);
	  fprintf (stderr, "try --help\n");
	  exit (1);
	}
      printf ("%s\n", cfg__std__package);
      exit (0);
    }

  if (string_equal_p (argv[1], "--is-compatible"))
    {
	if (argc != 3)
	  {
	    fprintf (stderr, "usage: %s --compatible package-name\n", argv[0]);
	    fprintf (stderr, "try --help\n");
	    exit (1);
	  }

#undef CFG__COMPATIBLE_RELEASE
#define CFG__COMPATIBLE_RELEASE(release) if (string_equal_p (release, argv[2])) exit (0);
      
	CFG__COMPATIBLE_RELEASES();
      exit (1);
    }

  if (string_equal_p (argv[1], "--compatible"))
    {

      if (argc != 2)
	{
	  fprintf (stderr, "usage: %s --options\n", argv[0]);
	  fprintf (stderr, "try --help\n");
	  exit (1);
	}

      puts ("");

      printf ("Compatibility list for %s\n", cfg__std__package);

      puts ("");

#undef CFG__COMPATIBLE_RELEASE
#define CFG__COMPATIBLE_RELEASE(release) printf ("  %s\n", release);

	CFG__COMPATIBLE_RELEASES();
      
      puts ("");
      exit (0);
    }

  if (string_equal_p (argv[1], "--options"))
    {

      if (argc != 2)
	{
	  fprintf (stderr, "usage: %s --options\n", argv[0]);
	  fprintf (stderr, "try --help\n");
	  exit (1);
	}

      puts ("");

      printf ("Configuration options for %s\n", cfg__std__package);

      puts ("");

#undef CFG__STRING_OPTION
#define CFG__STRING_OPTION(name,value) if (standard_option (#name)) { printf ("  "); print_option_name (#name); printf (" = %s\n", value); }
      CFG__STRING_OPTIONS();

      puts ("");

#undef CFG_BINARY_OPTION
#define CFG__BINARY_OPTION(name,value) printf ("  "); print_option_name (#name); printf (" = %s\n", value ? "yes" : "no");
      CFG__BINARY_OPTIONS();

      puts ("");

#undef CFG__STRING_OPTION
#define CFG__STRING_OPTION(name,value) if (!standard_option (#name)) { printf ("  "); print_option_name (#name); printf (" = %s\n", value); }
      CFG__STRING_OPTIONS();

      puts ("");

      exit (0);
    }

  fprintf (stderr, "%s: unrecognized option\n", argv[0]);
  fprintf (stderr, "try --help\n");
  exit (1);
}


/* tag: A generic program for reporting config info
 */
