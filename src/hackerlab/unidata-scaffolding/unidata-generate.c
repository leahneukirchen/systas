/* tag: Tom Lord Tue Dec  4 14:41:49 2001 (unidata-generate.c)
 */
/* unidata-generate.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/arrays/pow2-array-compact.h"
#include "hackerlab/arrays/pow2-array-print.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/bitsets/bits-print.h"
#include "hackerlab/bitsets/uni-bits.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/uni/unidata.h"
#include "hackerlab/unidata/db-macros.h"
#include "hackerlab/unidata/case-db-macros.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unidata-generate";
static t_uchar * usage = "[options] input-file";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_long_help, "H", 0, 0, \
      "Display a verbose help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.") \
  OP (opt_verbose, "v", "verbose", 0, \
      "Display information about the unidata database on stderr.")

static t_uchar long_help[] = ("Generate C source code from teh unidata database.\n"
			      "This program is used during the build process of \n"
			      "the hackerlab C library.\n");


enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



struct unidata
{
  t_unicode code_value;
  t_uchar * character_name;
  enum uni_general_category general_category;
  t_uint canonical_combining_class;
  enum uni_bidi_category bidi_category;
  struct uni_decomposition_mapping character_decomposition_mapping;
  int decimal_digit_value;
  int digit_value;
  struct uni_numeric_value numeric_value;
  int mirrored;
  t_uchar * unicode_1_name;
  t_uchar * comment_10646;
  t_unicode uppercase_mapping;
  t_unicode lowercase_mapping;
  t_unicode titlecase_mapping;
};



#define UNIDATA_FIELDS \
  UNIDATA_FIELD(unidata_code_value, "[0-9a-fA-F]\\+") \
  UNIDATA_FIELD(unidata_character_name, "[^;]\\+") \
  UNIDATA_FIELD(unidata_general_category, "[A-Z][a-z]") \
  UNIDATA_FIELD(unidata_canonical_combining_class, "[0-9]\\+") \
  UNIDATA_FIELD(unidata_bidi_category, "[A-Z]\\{1,3\\}") \
  UNIDATA_FIELD(unidata_character_decomposition_mapping, "[^;]*") \
  UNIDATA_FIELD(unidata_decimal_digit_value, "[0-9]*") \
  UNIDATA_FIELD(unidata_digit_value, "[0-9]*") \
  UNIDATA_FIELD(unidata_numeric_value, "[0-9/]*") \
  UNIDATA_FIELD(unidata_mirrored, "[YN]") \
  UNIDATA_FIELD(unidata_unicode_1_name, "[^;]*") \
  UNIDATA_FIELD(unidata_comment_10646, "[^;]*") \
  UNIDATA_FIELD(unidata_uppercase_mapping, "[0-9a-fA-F]*") \
  UNIDATA_FIELD(unidata_lowercase_mapping, "[0-9a-fA-F]*") \
  UNIDATA_FIELDX(unidata_titlecase_mapping, "[0-9a-fA-F]*")

/* positions within pmatch data of fields:
 */
enum unidata_field_positions
{
#undef UNIDATA_FIELD
#define UNIDATA_FIELD(A,B)	A,
#undef UNIDATA_FIELDX
#define UNIDATA_FIELDX(A,B)	A,

  unidata_entire_line = 0,
  UNIDATA_FIELDS

    n_unidata_fields
};

/* regexp for unidata fields
 */
static char unidata_regexp_source[] =
#undef UNIDATA_FIELD
#define UNIDATA_FIELD(A,B) "\\(" B "\\)" ";"
#undef UNIDATA_FIELDX
#define UNIDATA_FIELDX(A,B) "\\(" B "\\)" 
  "^" UNIDATA_FIELDS "$";
                                 
static regex_t *
unidata_regexp (void)
{
  static int done = 0;
  static regex_t answer;

  if (done)
    return &answer;

  if (regcomp (&answer, unidata_regexp_source, REG_NEWLINE))
    panic ("internal regcomp error for unidata_regexp_source");

  done = 1;

  return &answer;
}




/* Parsed format of a unidata line.
 */


#undef UNI_DECOMPOSITION_TYPE
#define UNI_DECOMPOSITION_TYPE(NAME) \
  "<" #NAME ">"  "[[:cut %:]]\\|"

static char uni_decomposition_type_regexp_source[] =
  "^" "[[:(" UNI_DECOMPOSITION_TYPES "[[:cut 2:]]" "):]]";

static regex_t *
uni_decomposition_type_regexp (void)
{
  static int done = 0;
  static regex_t answer;

  if (done)
    return &answer;

  if (regcomp (&answer, uni_decomposition_type_regexp_source, 0))
    panic ("internal regcomp error for uni_decomposition_type_regexp_source");

  done = 1;

  return &answer;
}




static char uni_range_first_regexp_source[] = "^[^;]*;[^;]*[[:([[:( First>;[[:cut 1:]]):]]\\|[[:( Last>;[[:cut 2:]]):]]):]]";

static regex_t *
uni_range_first_regexp (void)
{
  static int done = 0;
  static regex_t answer;

  if (done)
    return &answer;

  if (regcomp (&answer, uni_range_first_regexp_source, 0))
    panic ("internal regcomp error for uni_range_first_regexp");

  done = 1;

  return &answer;
}




void
unidata_parse (struct unidata * unidata, int * in_range, int line_no, t_uchar * line, long len)
{
  int errn;
  int match;
  regmatch_t pmatch[n_unidata_fields];
  regmatch_t * pmatch_p = pmatch;
  t_uint n;
  t_uchar * syntax_error;
  
  if (len && (line[len - 1] == '\n'))
    --len;
  if (len && (line[len - 1] == '\r'))
    --len;

  match = regnexec (unidata_regexp (), (char *)line, len, n_unidata_fields, &pmatch_p, 0);

  if (match)
    {
      syntax_error = "parsing entire line into fields";
    syntax_exit:
      safe_printfmt (2, "unicode database:%d: syntax error (%s)\n", line_no, syntax_error);
      safe_printfmt (2, "\t%.*s\n", (int)len, line);
      panic ("unrecoverable error parsing unicode database");
    }


  {
    regmatch_t range_pmatch[1];
    regmatch_t * range_pmatch_p = range_pmatch;

    match = regnexec (uni_range_first_regexp (), line, len, 1, &range_pmatch_p, 0);
    switch (match)
      {
      case 0:
	*in_range = range_pmatch[0].final_tag;
	break;
      case REG_NOMATCH:
	*in_range = 0;
	break;
      default:
	safe_printfmt (2, "unicode database:%d:\n", line_no);
	panic ("internal regexp error");
	break;
      }
  }

  
  if (cvt_hex_to_uint (&errn,
		       &n,
		       line + pmatch[unidata_code_value].rm_so,
		       pmatch[unidata_code_value].rm_eo - pmatch[unidata_code_value].rm_so))
    {
      syntax_error = "parsing code value";
      goto syntax_exit;
    }
  else
    unidata->code_value = (t_unicode)n;

  unidata->character_name = str_save_n (lim_use_must_malloc,
					line + pmatch[unidata_character_name].rm_so,
					pmatch[unidata_character_name].rm_eo - pmatch[unidata_character_name].rm_so);

  unidata->general_category = uni_general_category_lookup_n (line + pmatch[unidata_general_category].rm_so,
						     pmatch[unidata_general_category].rm_eo - pmatch[unidata_general_category].rm_so);

  if (cvt_decimal_to_uint (&errn, &unidata->canonical_combining_class,
			   line + pmatch[unidata_canonical_combining_class].rm_so,
			   pmatch[unidata_canonical_combining_class].rm_eo - pmatch[unidata_canonical_combining_class].rm_so))
    {
      syntax_error = "parsing canonical combining class";
      goto syntax_exit;
    }

  unidata->bidi_category = uni_bidi_category_lookup_n (line + pmatch[unidata_bidi_category].rm_so,
						       pmatch[unidata_bidi_category].rm_eo - pmatch[unidata_bidi_category].rm_so);

  if (pmatch[unidata_character_decomposition_mapping].rm_eo == pmatch[unidata_character_decomposition_mapping].rm_so)
    {
      unidata->character_decomposition_mapping.type = uni_decomposition_none;
      unidata->character_decomposition_mapping.decomposition = 0;
    }
  else
    {
      regmatch_t decomp_pmatch[1];
      regmatch_t * decomp_pmatch_p = decomp_pmatch;
      t_uchar * str;
      size_t len;

      match = regnexec (uni_decomposition_type_regexp (),
			line + pmatch[unidata_character_decomposition_mapping].rm_so,
			pmatch[unidata_character_decomposition_mapping].rm_eo - pmatch[unidata_character_decomposition_mapping].rm_so,
			1,
			&decomp_pmatch_p,
			0);
      if (match)
	{
	  syntax_error = "parsing character decomposition mapping type";
	  goto syntax_exit;
	}

      unidata->character_decomposition_mapping.type = decomp_pmatch[0].final_tag - 1;
      unidata->character_decomposition_mapping.decomposition = 0;

      str = line + pmatch[unidata_character_decomposition_mapping].rm_so + decomp_pmatch[0].rm_eo;
      len = (pmatch[unidata_character_decomposition_mapping].rm_eo - pmatch[unidata_character_decomposition_mapping].rm_so) - decomp_pmatch[0].rm_eo;

      while (1)
	{
	  t_uint d;
	  t_uchar * d_start;
	  size_t d_len;

	  while (len && char_is_space (*str))
	    {
	      ++str;
	      --len;
	    }

	  if (!len)
	    break;

	  d_start = str;
	  d_len = 0;
	  while (len && char_is_xdigit (*str))
	    {
	      ++d_len;
	      ++str;
	      --len;
	    }

	  if (cvt_hex_to_uint (&errn, &d, d_start, d_len))
	    {
	      syntax_error = "parsing decomposition value";
	      goto syntax_exit;
	    }

	  *(t_unicode *)ar_push ((void **)&unidata->character_decomposition_mapping.decomposition, lim_use_must_malloc, sizeof (t_unicode)) = (t_unicode)d;
	}
    }


  if (pmatch[unidata_decimal_digit_value].rm_so == pmatch[unidata_decimal_digit_value].rm_eo)
    unidata->decimal_digit_value = 10;
  else if (cvt_decimal_to_int (&errn, &unidata->decimal_digit_value,
			       line + pmatch[unidata_decimal_digit_value].rm_so,
			       pmatch[unidata_decimal_digit_value].rm_eo - pmatch[unidata_decimal_digit_value].rm_so))
    {
      syntax_error = "parsing decimal digit value";
      goto syntax_exit;
    }
    
  if (pmatch[unidata_digit_value].rm_so == pmatch[unidata_digit_value].rm_eo)
    unidata->digit_value = -1;
  else if (cvt_decimal_to_int (&errn, &unidata->digit_value,
			       line + pmatch[unidata_digit_value].rm_so,
			       pmatch[unidata_digit_value].rm_eo - pmatch[unidata_digit_value].rm_so))
    {
      syntax_error = "parsing digit value";
      goto syntax_exit;
    }

    
  if (pmatch[unidata_numeric_value].rm_so == pmatch[unidata_numeric_value].rm_eo)
    unidata->numeric_value.numerator = -1;
  else
    {
      t_uchar * slash;
      t_uchar * str;
      size_t len;

      str = line + pmatch[unidata_numeric_value].rm_so;
      len = pmatch[unidata_numeric_value].rm_eo - pmatch[unidata_numeric_value].rm_so;
      slash = str_chr_index_n (str, len, '/');
      if (!slash)
	{
	  unidata->numeric_value.denominator = 1;
	  if (cvt_decimal_to_int (&errn, &unidata->numeric_value.numerator, str, len))
	    {
	      syntax_error = "parsing numeric value";
	      goto syntax_exit;
	    }
	}
      else
	{
	  if (cvt_decimal_to_uint (&errn, &unidata->numeric_value.numerator, str, slash - str))
	    {
	      syntax_error = "parsing numerator of numeric value";
	      goto syntax_exit;
	    }
	  if (cvt_decimal_to_int (&errn, &unidata->numeric_value.denominator, slash + 1, len - (slash - str) - 1))
	    {
	      syntax_error = "parsing denominator of numeric value";
	      goto syntax_exit;
	    }
	}
    }


  switch (line[pmatch[unidata_mirrored].rm_so])
    {
    case 'Y':
      unidata->mirrored = 1;
      break;
    case 'N':
      unidata->mirrored = 0;
      break;
    default:
      syntax_error = "parsing mirrored";
      goto syntax_exit;
    }


  if (pmatch[unidata_unicode_1_name].rm_so == pmatch[unidata_unicode_1_name].rm_eo)
    unidata->unicode_1_name = 0;
  else
    unidata->unicode_1_name = str_save_n (lim_use_must_malloc,
					  line + pmatch[unidata_unicode_1_name].rm_so,
					  pmatch[unidata_unicode_1_name].rm_eo - pmatch[unidata_unicode_1_name].rm_so);

  if (pmatch[unidata_comment_10646].rm_so == pmatch[unidata_comment_10646].rm_eo)
    unidata->comment_10646 = 0;
  else
    unidata->comment_10646 = str_save_n (lim_use_must_malloc,
					 line + pmatch[unidata_comment_10646].rm_so,
					 pmatch[unidata_comment_10646].rm_eo - pmatch[unidata_comment_10646].rm_so);

  if (pmatch[unidata_uppercase_mapping].rm_so == pmatch[unidata_uppercase_mapping].rm_eo)
    unidata->uppercase_mapping = 0;
  else if (cvt_hex_to_uint (&errn, &n,
			    line + pmatch[unidata_uppercase_mapping].rm_so,
			    pmatch[unidata_uppercase_mapping].rm_eo - pmatch[unidata_uppercase_mapping].rm_so))
    {
      syntax_error = "parsing uppercase mapping";
      goto syntax_exit;
    }
  else
    unidata->uppercase_mapping = n;

  if (pmatch[unidata_lowercase_mapping].rm_so == pmatch[unidata_lowercase_mapping].rm_eo)
    unidata->lowercase_mapping = 0;
  else if (cvt_hex_to_uint (&errn, &n,
			    line + pmatch[unidata_lowercase_mapping].rm_so,
			    pmatch[unidata_lowercase_mapping].rm_eo - pmatch[unidata_lowercase_mapping].rm_so))
    {
      syntax_error = "parsing lowercase mapping";
      goto syntax_exit;
    }
  else
    unidata->lowercase_mapping = n;

  if (pmatch[unidata_titlecase_mapping].rm_so == pmatch[unidata_titlecase_mapping].rm_eo)
    unidata->titlecase_mapping = 0;
  else if (cvt_hex_to_uint (&errn, &n,
			    line + pmatch[unidata_titlecase_mapping].rm_so,
			    pmatch[unidata_titlecase_mapping].rm_eo - pmatch[unidata_titlecase_mapping].rm_so))
    {
      syntax_error = "parsing titlecase mapping";
      goto syntax_exit;
    }
  else
    unidata->titlecase_mapping = n;
}



void
unidata_free (struct unidata * ud)
{
  lim_free (lim_use_must_malloc, ud->character_name);
  ar_free ((void **)&ud->character_decomposition_mapping.decomposition, lim_use_must_malloc);
  lim_free (lim_use_must_malloc, ud->unicode_1_name);
  lim_free (lim_use_must_malloc, ud->comment_10646);
}



int
unidata_next (struct unidata * data, int * in_range, int * line_no, int fd)
{
  int errn;
  t_uchar * line;
  long len;

  ++*line_no;

  if (0 > vfdbuf_next_line (&errn, &line, &len, fd))
    {
      safe_printfmt (2, "unicode database (%d): %s\n", errn, errno_to_string (errn));
      panic ("unrecoverable error parsing unicode database\n");
    }

  if (!line)
    return 0;

  unidata_parse (data, in_range, *line_no, line, (size_t)len);
  return 1;
}


static void
print_t_uint16 (int fd, void * elt)
{
  safe_printfmt (fd, "%d", (int)(*(t_uint16 *)elt));
}

static void
print_t_case (int fd, void * elt)
{
  struct uni_case_mapping * mapping;
  mapping = (struct uni_case_mapping *)elt;
  safe_printfmt (fd, "{ 0x%04lX, 0x%04lX, 0x%04lX }", (unsigned long)mapping->upper, (unsigned long)mapping->lower, (unsigned long)mapping->title);
  /* safe_printfmt (fd, "{ 0x%l04X, 0x%l04X }", (unsigned long)mapping->upper, (unsigned long)mapping->lower); */
  /* safe_printfmt (fd, "{ 0x%l04X }", (unsigned long)mapping->upper); */
}

static void
print_t_uint8 (int fd, void * elt)
{
  safe_printfmt (fd, "%d", (int)(*(t_uint8 *)elt));
}

static void
print_t_int16 (int fd, void * elt)
{
  safe_printfmt (fd, "%d", (int)(*(t_int16 *)elt));
}


int
main (int argc, char * argv[])
{
  int errn;
  t_uchar * input_file;
  t_uchar * bits_file;
  t_uchar * bits_h_file;
  t_uchar * db_file;
  t_uchar * db_h_file;
  t_uchar * case_db_file;
  t_uchar * case_db_h_file;
  t_uchar * combine_db_file;
  t_uchar * combine_db_h_file;
  t_uchar * decomp_db_file;
  t_uchar * decomp_db_h_file;
  int input_fd;
  int bits_fd;
  int bits_h_fd;
  int db_fd;
  int db_h_fd;
  int case_db_fd;
  int case_db_h_fd;
  int combine_db_fd;
  int combine_db_h_fd;
  int decomp_db_fd;
  int decomp_db_h_fd;
  bits * sets;
  bits all_chars;
  int x;
  int line_no;
  int has_decomp;
  int max_decomp;
  int total_decomp;
  t_unicode worst_decomp;
  int non0_combine;
  int uppers;
  int lowers;
  int titles;
  int uppers_and_lowers;
  int uppers_and_title;
  int two_case;
  int three_case;
  int have_case[256];
  int have_case2[512];
  int numerics;
  int non_dec_digits;
  pow2_array_rules db_rules;
  pow2_array db_array;
  pow2_array_rules case_rules;
  pow2_array case_db_array;
  pow2_array_rules combine_rules;
  pow2_array combine_db_array;
  pow2_array_rules decomp_rules;
  pow2_array decomp_db_array;
  struct uni_decomposition_mapping * decompositions;
  int verbose;
  int o;
  struct opt_parsed * option;

  verbose = 0;
  option = 0;

  while (1)
    {
      o = opt_standard (lim_use_must_malloc, &option, opts, &argc, argv, program_name, usage, version_string, long_help, opt_help_msg, opt_long_help, opt_version);
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

	case opt_verbose:
	  verbose = 1;
	  break;
	}
    }

  rx_set_dfa_cache_threshold (2 * 2097152);

  if (argc != 2)
    goto usage_error;

  input_file = argv[1];
  bits_file = "bitsets.c";
  bits_h_file = "bitsets.h";
  db_file = "db.c";
  db_h_file = "db.h";
  case_db_file = "case-db.c";
  case_db_h_file = "case-db.h";
  combine_db_file = "combine-db.c";
  combine_db_h_file = "combine-db.h";
  decomp_db_file = "decomp-db.c";
  decomp_db_h_file = "decomp-db.h";

  input_fd = safe_open (input_file, O_RDONLY, 0);
  bits_fd = safe_open (bits_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  bits_h_fd = safe_open (bits_h_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  db_fd = safe_open (db_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  db_h_fd = safe_open (db_h_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  case_db_fd = safe_open (case_db_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  case_db_h_fd = safe_open (case_db_h_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  combine_db_fd = safe_open (combine_db_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  combine_db_h_fd = safe_open (combine_db_h_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  decomp_db_fd = safe_open (decomp_db_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
  decomp_db_h_fd = safe_open (decomp_db_h_file, O_WRONLY | O_CREAT | O_EXCL, 0644);

  if (vfdbuf_buffer_fd (&errn, input_fd, 0, O_RDONLY, 0))
    panic ("unable to buffer input file");
  if (vfdbuf_buffer_fd (&errn, bits_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer bitset output file");
  if (vfdbuf_buffer_fd (&errn, bits_h_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer bitset header output file");
  if (vfdbuf_buffer_fd (&errn, db_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db output file");
  if (vfdbuf_buffer_fd (&errn, db_h_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db header output file");
  if (vfdbuf_buffer_fd (&errn, case_db_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db output file");
  if (vfdbuf_buffer_fd (&errn, case_db_h_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db header output file");
  if (vfdbuf_buffer_fd (&errn, combine_db_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db output file");
  if (vfdbuf_buffer_fd (&errn, combine_db_h_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db header output file");
  if (vfdbuf_buffer_fd (&errn, decomp_db_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db output file");
  if (vfdbuf_buffer_fd (&errn, decomp_db_h_fd, 0, O_WRONLY, 0))
    panic ("unable to buffer db header output file");

  sets = (bits *)must_malloc (uni_n_categories * sizeof (bits));
  for (x = 0; x < uni_n_categories; ++x)
    sets[x] = bits_alloc (0, uni_bits_tree_rule);

  all_chars = bits_alloc (0, uni_bits_tree_rule);

  {
    static t_uint16 db_default_page[1 << 11];
    static struct uni_case_mapping case_default_page[16];
    static t_uint8 combine_default_page[16];
    static t_int16 decomp_default_page[16];

    {
      int i;
      t_uint16 v;

      v = unidata__assemble_db (0, 10, 0, uni_bidi_ON, uni_general_category_Cn);
      for (i = 0; i < (sizeof (db_default_page) / sizeof (db_default_page[0])); ++i)
	{
	  db_default_page[i] = v;
	}
    }

    db_rules = make_pow2_array_rules (lim_use_must_malloc,
				      sizeof (t_uint16),
				      (void *)db_default_page,
				      11, 0x3ff,
				      0, 0x7ff);
    case_rules = make_pow2_array_rules (lim_use_must_malloc,
					sizeof (struct uni_case_mapping),
					(void *)case_default_page,
					16, 0x1f,
					12, 0xf,
					8, 0xf,
					4, 0xf,
					0, 0xf);
    combine_rules = make_pow2_array_rules (lim_use_must_malloc,
					   sizeof (t_uint8),
					   (void *)combine_default_page,
					   16, 0x1f,
					   12, 0xf,
					   8, 0xf,
					   4, 0xf,
					   0, 0xf);
    decomp_rules = make_pow2_array_rules (lim_use_must_malloc,
					  sizeof (t_int16),
					  (void *)decomp_default_page,
					  16, 0x1f,
					  12, 0xf,
					  8, 0xf,
					  4, 0xf,
					  0, 0xf);
  }

  decompositions = 0;
  ar_push ((void **)&decompositions, lim_use_must_malloc, sizeof (struct uni_decomposition_mapping));
  decompositions[0].type = uni_decomposition_none;
  decompositions[0].decomposition = 0;
				 
  db_array = pow2_array_alloc (lim_use_must_malloc, db_rules);
  case_db_array = pow2_array_alloc (lim_use_must_malloc, case_rules);
  combine_db_array = pow2_array_alloc (lim_use_must_malloc, combine_rules);
  decomp_db_array = pow2_array_alloc (lim_use_must_malloc, decomp_rules);

  line_no = 0;
  has_decomp = 0;
  max_decomp = 0;
  total_decomp = 0;
  non0_combine = 0;
  uppers = 0;
  uppers_and_title = 0;
  uppers_and_lowers = 0;
  lowers = 0;
  titles = 0;
  for (x = 0; x < 256; ++x)
    have_case[x] = 0;
  for (x = 0; x < 512; ++x)
    have_case2[x] = 0;
  two_case = 0;
  three_case = 0;
  numerics = 0;
  non_dec_digits = 0;

  while (1)
    {
      struct unidata data;
      int in_range;
      struct unidata data_2;

      if (!unidata_next (&data, &in_range, &line_no, input_fd))
	break;			/* eof */

      if (verbose && !(line_no % 500))
	safe_printfmt (2, "line %d\n", line_no);

      if (data.general_category == uni_general_category_Cn)
	{
	  safe_printfmt (2, "Character U+%X is an unassigned character in unidata.txt!", data.code_value);
	  panic ("unidata.txt is broken");
	}

      {
	t_uint16 dbv;

	dbv = unidata__assemble_db (1, data.decimal_digit_value, data.mirrored, data.bidi_category, data.general_category);
	*(t_uint16 *)pow2_array_ref (db_array, data.code_value) = dbv;
      }

      if (data.digit_value >= 0)
	++non_dec_digits;

      if (data.numeric_value.numerator >= 0)
	{
	  ++numerics;
	}

      if (data.character_decomposition_mapping.decomposition)
	{
	  size_t size;
	  ++has_decomp;
	  size = ar_size ((void *)data.character_decomposition_mapping.decomposition, lim_use_must_malloc, sizeof (*data.character_decomposition_mapping.decomposition));
	  total_decomp += (int)size;
	  if (size > max_decomp)
	    {
	      max_decomp = (int) size;
	      worst_decomp = data.code_value;
	    }
	}

      if (data.uppercase_mapping)
	{
	  ++uppers;
	  if (data.lowercase_mapping)
	    ++uppers_and_lowers;
	  if (data.titlecase_mapping)
	    ++uppers_and_title;
	}

      if (data.lowercase_mapping)
	++lowers;

      if (data.titlecase_mapping)
	++titles;

      {
	int q;

	q = !!data.uppercase_mapping + !!data.lowercase_mapping + !!data.titlecase_mapping;
	if (q == 2)
	  ++two_case;
	else if (q == 3)
	  ++three_case;
      }

      if (data.uppercase_mapping || data.lowercase_mapping || data.titlecase_mapping)
	{
	  have_case[0xff & (data.code_value >> 8)] = 1;
	  have_case2[0x1ff & (data.code_value >> 7)] = 1;
	}

      if (data.uppercase_mapping || data.lowercase_mapping || data.titlecase_mapping)
	{
	  struct uni_case_mapping * mapping;

	  mapping = (struct uni_case_mapping *)pow2_array_ref (case_db_array, data.code_value);
	  mapping->upper = data.uppercase_mapping;
	  mapping->lower = data.lowercase_mapping;
	  mapping->title = data.titlecase_mapping;
	}

      if (data.canonical_combining_class)
	{
	  ++non0_combine;
	  *(t_uint8 *)pow2_array_ref (combine_db_array, data.code_value) = data.canonical_combining_class;
	}

      if (data.character_decomposition_mapping.type != uni_decomposition_none)
	{
	  struct uni_decomposition_mapping * decomp;
	  t_int16 index;

	  if ((1 << 16) <= ar_size ((void *)decompositions, lim_use_must_malloc, sizeof (*decompositions)))
	    panic ("too many characters have decomposition mappings -- unidata-generate needs to be modified\n");
	  index = (t_int16)ar_size ((void *)decompositions, lim_use_must_malloc, sizeof (*decompositions));
	  decomp = (struct uni_decomposition_mapping *)ar_push ((void **)&decompositions, lim_use_must_malloc, sizeof (*decompositions));
	  decomp->type = data.character_decomposition_mapping.type;
	  decomp->decomposition = (t_unicode *)ar_copy ((void *)data.character_decomposition_mapping.decomposition,
						      lim_use_must_malloc,
						      sizeof (t_unicode));
	  *(t_int16 *)pow2_array_ref (decomp_db_array, data.code_value) = index;
	}

      bits_adjoin (sets[data.general_category], data.code_value);

      if ((data.general_category != uni_general_category_Cs) && (data.general_category != uni_general_category_Co))
	bits_adjoin (all_chars, data.code_value);

      if (in_range && (in_range != 1))
	{
	  safe_printfmt (2, "unicode database:%d: found end of range (\"..., Last>;\") without start of range\n", line_no);
	  panic ("unrecoverable error parsing unicode database");
	}
      else if (in_range)
	{
	  if (!unidata_next (&data_2, &in_range, &line_no, input_fd))
	    {
	      safe_printfmt (2, "unicode database:%d: end of line encountered looking for range end\n", line_no);
	      panic ("unrecoverable error parsing unicode database");
	    }

	  /* safe_printfmt (2, "line %d (range end)\n", line_no); */

	  if (in_range != 2)
	    {
	      safe_printfmt (2, "unicode database:%d: missing end of range (\"..., Last>;\")\n", line_no);
	      panic ("unrecoverable error parsing unicode database");
	    }

	  bits_fill_range (sets[data.general_category], data.code_value, data_2.code_value + 1);

	  if ((data.general_category != uni_general_category_Cs) && (data.general_category != uni_general_category_Co))
	    {
	      bits_fill_range (all_chars, data.code_value, data_2.code_value + 1);
	    }

	  {
	    t_uint16 dbv;
	    int q;

	    dbv = unidata__assemble_db (1, data.decimal_digit_value, data.mirrored, data.bidi_category, data.general_category);
	    for (q = data.code_value; q <= data_2.code_value; ++q)
	      *(t_uint16 *)pow2_array_ref (db_array, q) = dbv;
	    if (data.uppercase_mapping || data.lowercase_mapping || data.titlecase_mapping)
	      {
		struct uni_case_mapping * mapping;

		mapping = (struct uni_case_mapping *)pow2_array_ref (case_db_array, data.code_value);
		mapping->upper = data.uppercase_mapping;
		mapping->lower = data.lowercase_mapping;
		mapping->title = data.titlecase_mapping;
		for (q = data.code_value; q <= data_2.code_value; ++q)
		  *(struct uni_case_mapping *)pow2_array_ref (case_db_array, q) = *mapping;
	      }
	    if (data.canonical_combining_class)
	      {
		for (q = data.code_value; q <= data_2.code_value; ++q)
		  {
		    ++non0_combine;
		    *(t_uint8 *)pow2_array_ref (combine_db_array, q) = data.canonical_combining_class;
		  }
	      }

	    if (data.character_decomposition_mapping.type != uni_decomposition_none)
	      {
		t_int16 index;

		index = (t_int16)(ar_size ((void *)decompositions, lim_use_must_malloc, sizeof (*decompositions)) - 1);
		for (q = data.code_value; q <= data_2.code_value; ++q)
		  *(t_int16 *)pow2_array_ref (decomp_db_array, q) = index;
	      }
	  }
	}
    }

  /* "The Private Use character outside of the BMP (U+F0000..U+FFFFD,
   * U+100000..U+10FFFD) are not listed. These correspond to surrogate
   * pairs where the first surrogate is in the High Surrogate Private
   * Use section." - The UnicodeData File Format Version 3.0.0
   */
  bits_fill_range (sets[uni_general_category_Co], 0xf0000, 0xffffe);
  bits_fill_range (all_chars, 0xf0000, 0xffffe);
  bits_fill_range (sets[uni_general_category_Co], 0x100000, 0x10fff2);
  bits_fill_range (all_chars, 0x100000, 0x10fff2);

  /* These should appear to be unassigned characters in the database.
   *
   * If you encounter a file with private-use characters you don't 
   * recognize, that's an error.
   *
   * If you have an application that uses private use characters,
   * you should make a modified unidata.txt assigning them appropriate
   * categories (not Co).
   */


  {
    enum uni_general_category cat;

    for (cat = uni_first_synthetic_category; cat < uni_n_categories; ++cat)
      {
	int first_char;
	int x;
	bits it;

	first_char = uni_general_category_names[cat].name[0];

	it = bits_alloc (0, uni_bits_tree_rule);

	for (x = 0; uni_general_category_names[x].name; ++x)
	  {
	    if (   (uni_general_category_names[x].name[0] == first_char)
		&& (sets[x]))
	      {
		bits_union (it, sets[x]);
	      }
	  }
	
	sets[cat] = it;
      }

    safe_printfmt (bits_fd, "/* This file automatically generated by unidata-generate */\n\n");
    safe_printfmt (bits_fd, "#include \"bitsets.h\"\n");
    safe_printfmt (bits_fd, "\n\n");

    safe_printfmt (bits_h_fd, "/* This file automatically generated by unidata-generate */\n\n");
    safe_printfmt (bits_h_fd, "#include \"hackerlab/bitsets/bits.h\"\n");
    safe_printfmt (bits_h_fd, "\n\n");

    for (x = 0; x < uni_n_categories; ++x)
      {
	t_uchar * name;
	t_uchar * stub;

	name = str_alloc_cat (lim_use_must_malloc, "unidata_bitset_", uni_general_category_names[x].name);
	stub = str_alloc_cat (lim_use_must_malloc, name, "_");
	bits_compact (sets[x]);

	bits_print (bits_fd, sets[x], name, stub, 0, 0, 0);
	safe_printfmt (bits_fd, "\n\f\n");

	bits_print (bits_h_fd, sets[x], name, stub, 0, 1, 0);
	safe_printfmt (bits_h_fd, "\n\f\n");
      }
  }

  bits_compact (all_chars);

  bits_print (bits_fd, all_chars, "unidata_bitset_universal", "unidata_bitset_universal_", 0, 0, 0);
  safe_printfmt (bits_fd, "\n\n");

  bits_print (bits_h_fd, all_chars, "unidata_bitset_universal", "unidata_bitset_universal_", 0, 1, 0);
  safe_printfmt (bits_h_fd, "\n\n");

  if (verbose)
    {
      safe_printfmt (2, "%d characters have a decomposition mapping\n", has_decomp);
      safe_printfmt (2, "%d characters in the widest decomp mapping\n", max_decomp);
      safe_printfmt (2, "U+%X is the code value of the widest decomp mapping\n", worst_decomp);
      safe_printfmt (2, "%d characters (total) in decomp mappings\n", total_decomp);
      safe_printfmt (2, "%d have a non-0 canonical combining class\n", non0_combine);
      safe_printfmt (2, "%d have uppercase mappings\n", uppers);
      safe_printfmt (2, "%d have lowercase mappings\n", lowers);
      safe_printfmt (2, "%d have titlecase mappings\n", titles);
      safe_printfmt (2, "%d have upper and lower mappings\n", uppers_and_lowers);
      safe_printfmt (2, "%d have upper and title mappings\n", uppers_and_title);
      safe_printfmt (2, "%d have lower and title mappings\n", two_case - (uppers_and_lowers + uppers_and_title));
    }

  if (verbose)
    {
      {
	int case_pages;
	int case_half_pages;

	case_pages = 0;
	case_half_pages = 0;

	for (x = 0; x < 256; ++x)
	  if (have_case[x])
	    ++case_pages;

	for (x = 0; x < 512; ++x)
	  if (have_case2[x])
	    ++case_half_pages;

	safe_printfmt (2, "%d pages (256 characters/page) have case mappings\n", case_pages);
	safe_printfmt (2, "%d half pages (128 characters/page) have case mappings\n", case_half_pages);
	safe_printfmt (2, "%d characters have exactly two case mappings\n", two_case);
	safe_printfmt (2, "%d characters have exactly three case mappings\n", three_case);
      }

      safe_printfmt (2, "%d characters have a numeric value\n", numerics);
      safe_printfmt (2, "%d characters are non-decimal digits\n", non_dec_digits);
    }

  safe_printfmt (db_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (db_fd, "#include \"db.h\"\n");
  safe_printfmt (db_fd, "\n\n");
  
  pow2_array_compact (db_array, 0, 0, 0);
  pow2_array_print (db_fd, db_array, "unidata__db", "unidata__db", 0, 0, 0, "t_uint16", print_t_uint16);
  safe_printfmt (db_fd, "\n\n");

  safe_printfmt (db_h_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (db_h_fd, "#include \"hackerlab/arrays/pow2-array.h\"\n");
  safe_printfmt (db_h_fd, "\n\n");

  pow2_array_print (db_h_fd, db_array, "unidata__db", "unidata__db", 1, "unidata__db_ref", 0, "t_uint16", 0);
  safe_printfmt (db_h_fd, "\n\n");

  safe_printfmt (case_db_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (case_db_fd, "#include \"case-db.h\"\n");
  safe_printfmt (case_db_fd, "\n\n");
  
  pow2_array_compact (case_db_array, 0, 0, 0);
  pow2_array_print (case_db_fd, case_db_array, "unidata__case_db", "unidata__case_db", 0, 0, 0, "struct uni_case_mapping", print_t_case);
  safe_printfmt (case_db_fd, "\n\n");

  safe_printfmt (case_db_h_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (case_db_h_fd, "#include \"hackerlab/arrays/pow2-array.h\"\n");
  safe_printfmt (case_db_h_fd, "#include \"hackerlab/unidata/case-db-macros.h\"\n");
  safe_printfmt (case_db_h_fd, "\n\n");

  pow2_array_print (case_db_h_fd, case_db_array, "unidata__case_db", "unidata__case_db", 1, "unidata__case_db_ref", 0, "struct uni_case_mapping", 0);
  safe_printfmt (case_db_h_fd, "\n\n");

  safe_printfmt (combine_db_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (combine_db_fd, "#include \"combine-db.h\"\n");
  safe_printfmt (combine_db_fd, "\n\n");
  
  pow2_array_compact (combine_db_array, 0, 0, 0);
  pow2_array_print (combine_db_fd, combine_db_array, "unidata__combine_db", "unidata__combine_db", 0, 0, 0, "t_uint8", print_t_uint8);
  safe_printfmt (combine_db_fd, "\n\n");

  safe_printfmt (combine_db_h_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (combine_db_h_fd, "#include \"hackerlab/arrays/pow2-array.h\"\n");
  safe_printfmt (combine_db_h_fd, "#include \"hackerlab/unidata/combine-db-macros.h\"\n");
  safe_printfmt (combine_db_h_fd, "\n\n");

  pow2_array_print (combine_db_h_fd, combine_db_array, "unidata__combine_db", "unidata__combine_db", 1, "unidata__combine_db_ref", 0, "t_uint8", 0);
  safe_printfmt (combine_db_h_fd, "\n\n");

  safe_printfmt (decomp_db_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (decomp_db_fd, "#include \"decomp-db.h\"\n");
  safe_printfmt (decomp_db_fd, "\n\n");
  
  pow2_array_compact (decomp_db_array, 0, 0, 0);
  pow2_array_print (decomp_db_fd, decomp_db_array, "unidata__decomp_db", "unidata__decomp_db", 0, 0, 0, "t_int16", print_t_int16);
  safe_printfmt (decomp_db_fd, "\n\n");
  {
    size_t d;
    size_t n_d;
    size_t off;

    n_d = ar_size ((void *)decompositions, lim_use_must_malloc, sizeof (*decompositions));

    safe_printfmt (decomp_db_fd, "static t_unicode unidata_decomposition_data[] =\n");
    safe_printfmt (decomp_db_fd, "{\n");
    for (d = 0; d < n_d; ++d)
      {
	size_t c;
	size_t n_c;

	n_c = ar_size ((void *)decompositions[d].decomposition, lim_use_must_malloc, sizeof (*decompositions[d].decomposition));
	for (c = 0; c < n_c; ++c)
	  {
	    safe_printfmt (decomp_db_fd, "  0x%04X,\n", decompositions[d].decomposition[c]);
	  }
	safe_printfmt (decomp_db_fd, "  0x0,\n");
      }
    safe_printfmt (decomp_db_fd, "};\n\n");

    safe_printfmt (decomp_db_fd, "struct uni_decomposition_mapping unidata_decomposition_table[] =\n");
    safe_printfmt (decomp_db_fd, "{\n");
    off = 0;
    for (d = 0; d < n_d; ++d)
      {
	safe_printfmt (decomp_db_fd, "  { %d, unidata_decomposition_data + %lu },\n", decompositions[d].type, (unsigned long)off);
	off += ar_size ((void *)decompositions[d].decomposition, lim_use_must_malloc, sizeof (*decompositions[d].decomposition)) + 1;
      }
    safe_printfmt (decomp_db_fd, "};\n\n");
  }

  safe_printfmt (decomp_db_h_fd, "/* This file automatically generated by unidata-generate */\n\n");
  safe_printfmt (decomp_db_h_fd, "#include \"hackerlab/arrays/pow2-array.h\"\n");
  safe_printfmt (decomp_db_h_fd, "#include \"hackerlab/unidata/decomp-db-macros.h\"\n");
  safe_printfmt (decomp_db_h_fd, "\n\n");

  pow2_array_print (decomp_db_h_fd, decomp_db_array, "unidata__decomp_db", "unidata__decomp_db", 1, "unidata__decomp_db_ref", 0, "t_int16", 0);
  safe_printfmt (decomp_db_h_fd, "\n\n");
  safe_printfmt (decomp_db_h_fd, "extern struct uni_decomposition_mapping unidata_decomposition_table[];\n\n\n");

  safe_close (input_fd);
  safe_close (bits_fd);
  safe_close (bits_h_fd);
  safe_close (db_fd);
  safe_close (db_h_fd);
  safe_close (case_db_fd);
  safe_close (case_db_h_fd);
  safe_close (combine_db_fd);
  safe_close (combine_db_h_fd);
  safe_close (decomp_db_fd);
  safe_close (decomp_db_h_fd);

  return 0;
}

