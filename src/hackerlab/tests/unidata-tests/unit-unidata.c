/* tag: Tom Lord Tue Dec  4 14:41:21 2001 (unit-unidata.c)
 */
/* unit-unidata.c -
 *
 ****************************************************************
 * Copyright (C) YEAR  NAME
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/unidata/unidata.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-unidata";
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



struct unidata_range
{
  t_unicode first;
  t_unicode last;
};

static struct unidata_range abbreviated_ranges[] =
{
  { 0x3400, 0x4db5 },
  { 0x4e00, 0x9fa5 },
  { 0xac00, 0xd7a3 },
  { 0xd800, 0xdb7f },
  { 0xdb80, 0xdbff },
  { 0xdc00, 0xdfff },
  { 0xe000, 0xf8ff },
  { 0, 0 }
};

static struct unidata_range db_ranges[] = 
{
  { 0, 0xffff },
  { 0, 0 }
};



static void
print_char_data (t_unicode c)
{
  static t_uchar * digits[] = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
  int decimal;
  struct uni_decomposition_mapping * decomp;

  decimal = unidata_decimal_digit_value (c);
  safe_printfmt (1, "%04X;%s;%d;%s;",
		 c,
		 uni_general_category_names[unidata_general_category (c)].name,
		 unidata_canonical_combining_class (c),
		 uni_bidi_category_names[unidata_bidi_category (c)].name);

  decomp = unidata_character_decomposition_mapping (c);
  if (decomp->type != uni_decomposition_none)
    {
      int x;
      if (decomp->type != uni_decomposition_canonical)
	{
	  safe_printfmt (1, "<%s> ", uni_decomposition_type_names[decomp->type].name);
	}
      for (x = 0; decomp->decomposition[x]; ++x)
	{
	  safe_printfmt (1, "%04X%s", decomp->decomposition[x], (decomp->decomposition[x + 1] ? " " : ""));
	}
    }

  safe_printfmt (1, ";%s;%s;",
		 ((decimal == -1) ? (t_uchar *)"" : digits[decimal]),
		 (unidata_is_mirrored (c) ? "Y" : "N"));

  if (c != unidata_to_upper (c))
    safe_printfmt (1, "%04X", unidata_to_upper (c));
  safe_printfmt (1, ";");
  if (c != unidata_to_lower (c))
    safe_printfmt (1, "%04X", unidata_to_lower (c));
  safe_printfmt (1, ";");
  if (c != unidata_to_title (c))
    safe_printfmt (1, "%04X", unidata_to_title (c));
  safe_printfmt (1, "\r\n");
}

int
main (int argc, char * argv[])
{
  int errn;
  int o;
  struct opt_parsed * option;

  if (vfdbuf_buffer_fd (&errn, 1, 0, O_WRONLY, 0))
    panic ("unable to buffer fd");

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
	}
    }

  {
    int r;
    t_unicode c;
    int s;

    s = 0;
    for (r = 0; db_ranges[r].last; ++r)
      {
	for (c = db_ranges[r].first; c <= db_ranges[r].last; ++c)
	  {
	    if (c != abbreviated_ranges[s].first)
	      {
		if (unidata_is_assigned_code_point (c))
		  {
		    print_char_data (c);
		  }
		else if (unidata_general_category (c) == uni_general_category_Co)
		  {
		    safe_printfmt (1, "%04X;Co;0;L;;;N;;;\r\n", c);
		  }
	      }
	    else
	      {
		enum uni_general_category cat;
		int combine;
		enum uni_bidi_category bidi;
		int decimal;
		int mirrored;
		t_unicode upper;
		t_unicode lower;
		t_unicode title;
		t_unicode q;
		struct uni_decomposition_mapping * decomp;

		cat = unidata_general_category (c);
		if (cat == uni_general_category_Cs)
		  {
		    bidi = uni_bidi_L;
		    decimal = -1;
		    mirrored = 0;
		  }
		else
		  {
		    bidi = unidata_bidi_category (c);
		    decimal = unidata_decimal_digit_value (c);
		    mirrored = unidata_is_mirrored (c);
		  }

		combine = unidata_canonical_combining_class (c);
		decomp = unidata_character_decomposition_mapping (c);
		upper = unidata_to_upper (c);
		lower = unidata_to_lower (c);
		title = unidata_to_title (c);
		
		for (q = c; q <= abbreviated_ranges[s].last; ++q)
		  {
		    invariant (cat == unidata_general_category (q));
		    if (cat != uni_general_category_Cs)
		      {
			invariant (combine == unidata_canonical_combining_class (q));
			invariant (bidi == unidata_bidi_category (q));
			invariant (decimal == unidata_decimal_digit_value (q));
			invariant (mirrored == unidata_is_mirrored (q));
			invariant (decomp == unidata_character_decomposition_mapping (q));
			invariant (((upper == c) && (unidata_to_upper (q) == q)) || (upper == unidata_to_upper (q)));
			invariant (((lower == c) && (unidata_to_lower (q) == q)) || (lower == unidata_to_lower (q)));
			invariant (((title == c) && (unidata_to_title (q) == q)) || (title == unidata_to_title (q)));
		      }
		  }

		{
		  int z;
		  for (z = 0; z < 2; ++z)
		    {
		      static t_uchar * digits[] = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };

		      safe_printfmt (1, "%04X;%s;%d;%s;",
				     (z ? q - 1 : c),
				     uni_general_category_names[cat].name,
				     unidata_canonical_combining_class (c),
				     uni_bidi_category_names[bidi].name);
		      if (decomp->type != uni_decomposition_none)
			{
			  int x;
			  if (decomp->type != uni_decomposition_canonical)
			    {
			      safe_printfmt (1, "<%s> ", uni_decomposition_type_names[decomp->type].name);
			    }
			  for (x = 0; decomp->decomposition[x]; ++x)
			    {
			      safe_printfmt (1, "%04X%s", decomp->decomposition[x], (decomp->decomposition[x + 1] ? " " : ""));
			    }
			}
		      safe_printfmt (1, ";%s;%s;",
				     ((decimal == -1) ? (t_uchar *)"" : digits[decimal]),
				     (mirrored ? "Y" : "N"));
		      if (c != upper)
			safe_printfmt (1, "%04X", upper);
		      safe_printfmt (1, ";");
		      if (c != lower)
			safe_printfmt (1, "%04X", lower);
		      safe_printfmt (1, ";");
		      if (c != title)
			safe_printfmt (1, "%04X", title);
		      safe_printfmt (1, "\r\n");
		    }
		}
		c = abbreviated_ranges[s].last;
		++s;
	      }
	  }
      }
  }
  return 0;
}

