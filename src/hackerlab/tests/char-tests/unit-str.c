/* unit-str.c - test str.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-str";
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
    static t_uchar alnum[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    static t_uchar lcalnum[] = "0123456789abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
    t_uchar buffer[1024];
    int x;

    invariant (str_length (0) == 0);
    invariant (str_length ("") == 0);
    invariant (str_length ("a") == 1);
    invariant (str_length ("abc") == 3);
    invariant (str_length ("abcd") == 4);
    invariant (str_length ("abcde") == 5);
    invariant (str_length ("abcdefg") == 7);
    invariant (str_length ("abcdefgh") == 8);
    invariant (str_length ("abcdefghi") == 9);
    invariant (str_length ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") == 62);


    invariant (str_length_n (0, 5) == 0);
    invariant (str_length_n ("", 5) == 0);
    invariant (str_length_n ("a", 5) == 1);
    invariant (str_length_n ("abc", 5) == 3);
    invariant (str_length_n ("abcd", 5) == 4);
    invariant (str_length_n ("abcde", 5) == 5);
    invariant (str_length_n ("abcdefg", 5) == 5);
    invariant (str_length_n ("abcdefgh", 5) == 5);
    invariant (str_length_n ("abcdefghi", 5) == 5);
    invariant (str_length_n ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", 5) == 5);
    
    for (x = 'a'; x <= 'z'; ++x)
      {
	invariant (str_chr_index (lcalnum, x) == (lcalnum + 10 + (x - 'a')));
	invariant (str_chr_rindex (lcalnum, x) == (lcalnum + 36 + (x - 'a'))); 
	invariant (str_chr_index_n (lcalnum, 62, x) == (lcalnum + 10 + (x - 'a')));
	invariant (str_chr_rindex_n (lcalnum, 62, x) == (lcalnum + 36 + (x - 'a')));
      }
    {
      invariant (!str_cmp (0, ""));
      invariant (!str_cmp ("", 0));

      invariant (str_cmp ("", "a") == -1);
      invariant (str_cmp (0, "a") == -1);
      invariant (str_cmp ("a", "") == 1);
      invariant (str_cmp ("a", 0) == 1);
      invariant (!str_cmp ("a", "a"));
      invariant (str_cmp ("abcdefg", "abcdefh") == -1);
      invariant (str_cmp ("abcdefh", "abcdefg") == 1);
      invariant (str_cmp ("abcdefg", "abcdefg") == 0);
      invariant (!str_cmp (alnum, alnum));
      invariant (!str_cmp (lcalnum, lcalnum));
      invariant (str_cmp (alnum, lcalnum) == -1);
      invariant (str_cmp (lcalnum, alnum) == 1);

      invariant (str_cmp_n ("", sizeof ("") - 1, "a", sizeof ("a") - 1) == -1);
      invariant (str_cmp_n (0, 0, "a", sizeof ("a") - 1) == -1);
      invariant (str_cmp_n ("a", sizeof ("a") - 1, "", sizeof ("") - 1) == 1);
      invariant (str_cmp_n ("a", sizeof ("a") - 1, 0, 0) == 1);
      invariant (!str_cmp_n ("a", sizeof ("a") - 1, "a", sizeof ("a") - 1));
      invariant (str_cmp_n ("abcdefg", sizeof ("abcdefg") - 1, "abcdefh", sizeof ("abcdefh") - 1) == -1);
      invariant (str_cmp_n ("abcdefh", sizeof ("abcdefh") - 1, "abcdefg", sizeof ("abcdefg") - 1) == 1);
      invariant (str_cmp_n ("abcdefg", sizeof ("abcdefg") - 1, "abcdefg", sizeof ("abcdefg") - 1) == 0);
      invariant (!str_cmp_n (alnum, sizeof (alnum) - 1, alnum, sizeof (alnum) - 1));
      invariant (!str_cmp_n (lcalnum, sizeof (lcalnum) - 1, lcalnum, sizeof (lcalnum) - 1));
      invariant (str_cmp_n (alnum, sizeof (alnum) - 1, lcalnum, sizeof (lcalnum) - 1) == -1);
      invariant (str_cmp_n (lcalnum, sizeof (lcalnum) - 1, alnum, sizeof (alnum) - 1) == 1);

      invariant (str_casecmp ("", "a") == -1);
      invariant (str_casecmp (0, "a") == -1);
      invariant (str_casecmp ("a", "") == 1);
      invariant (str_casecmp ("a", 0) == 1);
      invariant (!str_casecmp ("a", "a"));
      invariant (str_casecmp ("abcdefg", "abcdefh") == -1);
      invariant (str_casecmp ("abcdefh", "abcdefg") == 1);
      invariant (str_casecmp ("abcdefg", "abcdefg") == 0);
      invariant (!str_casecmp (alnum, alnum));
      invariant (!str_casecmp (lcalnum, lcalnum));
      invariant (!str_casecmp (alnum, lcalnum));
      invariant (!str_casecmp (lcalnum, alnum));

      invariant (str_casecmp_n ("", sizeof ("") - 1, "a", sizeof ("a") - 1) == -1);
      invariant (str_casecmp_n (0, 0, "a", sizeof ("a") - 1) == -1);
      invariant (str_casecmp_n ("a", sizeof ("a") - 1, "", sizeof ("") - 1) == 1);
      invariant (str_casecmp_n ("a", sizeof ("a") - 1, 0, 0) == 1);
      invariant (!str_casecmp_n ("a", sizeof ("a") - 1, "a", sizeof ("a") - 1));
      invariant (str_casecmp_n ("abcdefg", sizeof ("abcdefg") - 1, "abcdefh", sizeof ("abcdefh") - 1) == -1);
      invariant (str_casecmp_n ("abcdefh", sizeof ("abcdefh") - 1, "abcdefg", sizeof ("abcdefg") - 1) == 1);
      invariant (str_casecmp_n ("abcdefg", sizeof ("abcdefg") - 1, "abcdefg", sizeof ("abcdefg") - 1) == 0);
      invariant (!str_casecmp_n (alnum, sizeof (alnum) - 1, alnum, sizeof (alnum) - 1));
      invariant (!str_casecmp_n (lcalnum, sizeof (lcalnum) - 1, lcalnum, sizeof (lcalnum) - 1));
      invariant (!str_casecmp_n (alnum, sizeof (alnum) - 1, lcalnum, sizeof (lcalnum) - 1));
      invariant (!str_casecmp_n (lcalnum, sizeof (lcalnum) - 1, alnum, sizeof (alnum) - 1));

      invariant (!str_cmp_prefix ("01234", alnum));
      invariant (str_cmp_prefix ("abc", "ABCDEFGHIJKL") == 1);
      invariant (str_cmp_prefix ("ABC", "ABCDEFGHIJKL") == 0);
      invariant (str_cmp_prefix ("ABB", "ABCDEFGHIJKL") == -1);
      invariant (str_cmp_prefix ("", "ABCDEFGHIJKL") == 0);
      invariant (str_cmp_prefix (0, "ABCDEFGHIJKL") == 0);
      invariant (str_cmp_prefix ("abc", "a") == 1);
      invariant (str_cmp_prefix ("abc", "") == 1);
      invariant (str_cmp_prefix ("abc", 0) == 1);

      invariant (str_casecmp_prefix ("01234", alnum) == 0);
      invariant (str_casecmp_prefix ("abc", "ABCDEFGHIJKL") == 0);
      invariant (str_casecmp_prefix ("ABC", "ABCDEFGHIJKL") == 0);
      invariant (str_casecmp_prefix ("ABB", "ABCDEFGHIJKL") == -1);
      invariant (str_casecmp_prefix ("", "ABCDEFGHIJKL") == 0);
      invariant (str_casecmp_prefix (0, "ABCDEFGHIJKL") == 0);
      invariant (str_casecmp_prefix ("abc", "a") == 1);
      invariant (str_casecmp_prefix ("abc", "") == 1);
      invariant (str_casecmp_prefix ("abc", 0) == 1);

      buffer[0] = 1;
      str_cpy (buffer, 0);
      invariant (str_length (buffer) == 0);
      str_cpy (buffer, "abc");
      invariant (str_cmp (buffer, "abc") == 0);
      str_cpy (buffer, "abcd");
      invariant (str_cmp (buffer, "abcd") == 0);
      str_cpy (buffer, "abcde");
      invariant (str_cmp (buffer, "abcde") == 0);
      str_cpy (buffer, "abcdefg");
      invariant (str_cmp (buffer, "abcdefg") == 0);
      str_cpy (buffer, "abcdefgh");
      invariant (str_cmp (buffer, "abcdefgh") == 0);
      str_cpy (buffer, "abcdefghi");
      invariant (str_cmp (buffer, "abcdefghi") == 0);
      str_cpy (buffer, alnum);
      invariant (str_cmp (buffer, alnum) == 0);


      buffer[0] = 1;
      str_cpy_n (buffer, 0, 1);
      invariant (str_length (buffer) == 0);
      str_cpy_n (buffer, "abc", sizeof ("abc"));
      invariant (str_cmp (buffer, "abc") == 0);
      str_cpy_n (buffer, "abc", 100 + sizeof ("abc"));
      invariant (str_cmp (buffer, "abc") == 0);
      str_cpy_n (buffer, "abcd", sizeof ("abcd"));
      invariant (str_cmp (buffer, "abcd") == 0);
      str_cpy_n (buffer, "abcde", sizeof ("abcde"));
      invariant (str_cmp (buffer, "abcde") == 0);
      str_cpy_n (buffer, "abcdefg", sizeof ("abcdefg"));
      invariant (str_cmp (buffer, "abcdefg") == 0);
      str_cpy_n (buffer, "abcdefgh", sizeof ("abcdefgh"));
      invariant (str_cmp (buffer, "abcdefgh") == 0);
      str_cpy_n (buffer, "abcdefghi", sizeof ("abcdefghi"));
      invariant (str_cmp (buffer, "abcdefghi") == 0);
      str_cpy_n (buffer, alnum, sizeof (alnum));
      invariant (str_cmp (buffer, alnum) == 0);


      buffer[0] = 0;
      str_cat (buffer, 0);
      invariant (str_cmp (buffer, "") == 0);
      str_cat (buffer, "one");
      invariant (str_cmp (buffer, "one") == 0);
      str_cat (buffer, " ");
      invariant (str_cmp (buffer, "one ") == 0);
      str_cat (buffer, "two three four");
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat (buffer, "");
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat (buffer, 0);
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat (buffer, " five.");
      invariant (str_cmp (buffer, "one two three four five.") == 0);


      buffer[0] = 0;
      str_cat_n (buffer, 0, 0);
      invariant (str_cmp (buffer, "") == 0);
      str_cat_n (buffer, "one", sizeof ("one"));
      invariant (str_cmp (buffer, "one") == 0);
      str_cat_n (buffer, " ", sizeof (" "));
      invariant (str_cmp (buffer, "one ") == 0);
      str_cat_n (buffer, "two three four", sizeof ("two three four"));
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat_n (buffer, "", sizeof (""));
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat_n (buffer, 0, 0);
      invariant (str_cmp (buffer, "one two three four") == 0);
      str_cat_n (buffer, " five.", sizeof (" five."));
      invariant (str_cmp (buffer, "one two three four five.") == 0);
      str_cat_n (buffer, " xyzzy", 2);
      invariant (str_cmp (buffer, "one two three four five. x") == 0);
    }

    {
      invariant (!str_cmp (0, str_save (lim_use_must_malloc, (t_uchar *)"")));
      invariant (!str_cmp ("", str_save (lim_use_must_malloc, 0)));

      invariant (str_cmp ("", str_save (lim_use_must_malloc, (t_uchar *)"a")) == -1);
      invariant (str_cmp (0, str_save (lim_use_must_malloc, (t_uchar *)"a")) == -1);
      invariant (str_cmp ("a", str_save (lim_use_must_malloc, (t_uchar *)"")) == 1);
      invariant (str_cmp ("a", str_save (lim_use_must_malloc, 0)) == 1);
      invariant (!str_cmp ("a", str_save (lim_use_must_malloc, (t_uchar *)"a")));
      invariant (str_cmp ("abcdefg", str_save (lim_use_must_malloc, (t_uchar *)"abcdefh")) == -1);
      invariant (str_cmp ("abcdefh", str_save (lim_use_must_malloc, (t_uchar *)"abcdefg")) == 1);
      invariant (str_cmp ("abcdefg", str_save (lim_use_must_malloc, (t_uchar *)"abcdefg")) == 0);
      invariant (!str_cmp (alnum, str_save (lim_use_must_malloc, (t_uchar *)alnum)));
      invariant (!str_cmp (lcalnum, str_save (lim_use_must_malloc, (t_uchar *)lcalnum)));
      invariant (str_cmp (alnum, str_save (lim_use_must_malloc, (t_uchar *)lcalnum)) == -1);
      invariant (str_cmp (lcalnum, str_save (lim_use_must_malloc, (t_uchar *)alnum)) == 1);

      invariant (str_cmp_n ("", sizeof ("") - 1, str_save_n (lim_use_must_malloc, (t_uchar *)"a", sizeof ("a") - 1), sizeof ("a") - 1) == -1);
      invariant (str_cmp_n (0, 0, str_save_n (lim_use_must_malloc, (t_uchar *)"a", sizeof ("a") - 1), sizeof ("a") - 1) == -1);
      invariant (str_cmp_n ("a", sizeof ("a") - 1, str_save_n (lim_use_must_malloc, (t_uchar *)"", sizeof ("") - 1), sizeof ("") - 1) == 1);
      invariant (str_cmp_n ("a", sizeof ("a") - 1, str_save_n (lim_use_must_malloc, 0, 0), 0) == 1);
      invariant (str_cmp_n ("a", sizeof ("a") - 1, str_save_n (lim_use_must_malloc, 0, 0), 0) == 1);
      invariant (!str_cmp_n ("a", sizeof ("a") - 1, "a", sizeof ("a") - 1));
      invariant (str_cmp_n ("abcdefg", sizeof ("abcdefg") - 1,
			    str_save_n (lim_use_must_malloc, (t_uchar *)"abcdefh", sizeof ("abcdefh") - 1), sizeof ("abcdefh") - 1) == -1);
      invariant (str_cmp_n ("abcdefh", sizeof ("abcdefh") - 1,
			    str_save_n (lim_use_must_malloc, (t_uchar *)"abcdefg", sizeof ("abcdefg") - 1), sizeof ("abcdefg") - 1) == 1);
      invariant (str_cmp_n ("abcdefg", sizeof ("abcdefg") - 1,
			    str_save_n (lim_use_must_malloc, (t_uchar *)"abcdefg", sizeof ("abcdefg") - 1), sizeof ("abcdefg") - 1) == 0);
      invariant (!str_cmp_n (alnum, sizeof (alnum) - 1, str_save_n (lim_use_must_malloc, (t_uchar *)alnum, sizeof (alnum) - 1), sizeof (alnum) - 1));
      invariant (!str_cmp_n (lcalnum, sizeof (lcalnum) - 1, str_save_n (lim_use_must_malloc, (t_uchar *)lcalnum, sizeof (lcalnum) - 1), sizeof (lcalnum) - 1));
      invariant (str_cmp_n (alnum, sizeof (alnum) - 1, str_save_n (lim_use_must_malloc, (t_uchar *)lcalnum, sizeof (lcalnum) - 1), sizeof (lcalnum) - 1) == -1);
      invariant (str_cmp_n (lcalnum, sizeof (lcalnum) - 1, str_save_n (lim_use_must_malloc, (t_uchar *)alnum, sizeof (alnum) - 1), sizeof (alnum) - 1) == 1);
    }
    {
      t_uchar * b;

      buffer[0] = 0;
      b = str_alloc_cat (lim_use_must_malloc, buffer, 0);
      invariant (str_cmp (b, "") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, "one");
      invariant (str_cmp (b, "one") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, " ");
      invariant (str_cmp (b, "one ") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, "two three four");
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, "");
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, 0);
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat (lim_use_must_malloc, b, " five.");
      invariant (str_cmp (b, "one two three four five.") == 0);


      buffer[0] = 0;
      b = str_alloc_cat_n (lim_use_must_malloc, buffer, 0, 0);
      invariant (str_cmp (b, "") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, "one", sizeof ("one"));
      invariant (str_cmp (b, "one") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, " ", sizeof (" "));
      invariant (str_cmp (b, "one ") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, "two three four", sizeof ("two three four"));
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, "", sizeof (""));
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, 0, 0);
      invariant (str_cmp (b, "one two three four") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, " five.", sizeof (" five."));
      invariant (str_cmp (b, "one two three four five.") == 0);
      b = str_alloc_cat_n (lim_use_must_malloc, b, " xyzzy", 2);
      invariant (str_cmp (b, "one two three four five. x") == 0);
      
    }
  }
  return 0;
}



