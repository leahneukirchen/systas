/* re.c - An XML regular expression matcher 
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/os/setjmp.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/unidata/unidata.h"
#include "hackerlab/xml/charsets.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx/dfa.h"
#include "hackerlab/rx/dfa-iso8859-1.h"
#include "hackerlab/rx/dfa-utf8.h"
#include "hackerlab/rx/dfa-utf16.h"
#include "hackerlab/rx-xml/re.h"

struct xmlre_parse_state;


/* __STDC__ prototypes for static functions */
static void rx_xml_parse_regexp (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static void rx_xml_parse_branch (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static void rx_xml_parse_piece (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static struct rx_exp_node * rx_xml_build_interval_tree (struct xmlre_parse_state * state, struct rx_exp_node * exp, t_uint from, t_uint to);
static struct rx_exp_node * rx_xml_build_repeat_tree (struct rx_exp_node * n, t_uint amt);
static void rx_xml_parse_atom (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static int rx_xml_eop (struct xmlre_parse_state * state);
static int rx_xml_scan_op (struct xmlre_parse_state * state, t_unicode c);
static int rx_xml_scan_digit (struct xmlre_parse_state * state, int * d);
static int rx_xml_scan_op_ahead (struct xmlre_parse_state * state, t_unicode c);
static int rx_xml_scan_uint (struct xmlre_parse_state * state, t_uint * x);
static int rx_xml_scan_opt_uint (struct xmlre_parse_state * state, t_uint * x);
static int rx_xml_scan_single_character_escape (struct xmlre_parse_state * state, t_unicode * c);
static int rx_xml_scan_normal_char (struct xmlre_parse_state * state, t_unicode * c);
static int rx_xml_scan_multi_character_escape (struct xmlre_parse_state * state, bits * b);
static int block_name_cmp (t_uchar * source, t_uchar * canonical);
static int rx_xml_scan_category_escape (struct xmlre_parse_state * state, bits * b);
static int rx_xml_scan_character_class (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_group (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_range (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_class_escape (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_range_char (struct xmlre_parse_state * state, t_unicode * c);
static bits rx_xml_dot_set (struct xmlre_parse_state * state);
static bits rx_xml_space_set (void);
static bits rx_xml_name_initial_set (void);
static bits rx_xml_name_set (void);
static bits rx_xml_letter_set (void);
static bits rx_xml_unicode_category_set (struct xmlre_parse_state * state,
					 enum uni_general_category c);
static bits rx_xml_word_category_set (struct xmlre_parse_state * state);


/************************************************************************
 *(h1 "XML Regular Expression Functions"
 *	:includes ("hackerlab/rx-unicode/re.h"))
 * 
 * |XML regular expression|
 * |regular expression (XML)|
 * 
 */

/*(menu)
 */



struct xmlre_parse_state
{
  struct rx_exp_node * root;

  t_ulong syntax_options;

  bits complete_cset;

  enum uni_encoding_scheme encoding;
  uni_iscan_fn scan;

  t_uchar * source;
  size_t len;
  size_t pos;

  enum  rx_xml_recomp_errno err;
  jmp_buf *err_escape;
};


/* __STDC__ prototypes for static functions */
static void rx_xml_parse_regexp (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static void rx_xml_parse_branch (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static void rx_xml_parse_piece (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static struct rx_exp_node * rx_xml_build_interval_tree (struct xmlre_parse_state * state, struct rx_exp_node * exp, t_uint from, t_uint to);
static struct rx_exp_node * rx_xml_build_repeat_tree (struct rx_exp_node * n, t_uint amt);
static void rx_xml_parse_atom (struct xmlre_parse_state * state, struct rx_exp_node ** where);
static int rx_xml_eop (struct xmlre_parse_state * state);
static int rx_xml_scan_op (struct xmlre_parse_state * state, t_unicode c);
static int rx_xml_scan_digit (struct xmlre_parse_state * state, int * d);
static int rx_xml_scan_op_ahead (struct xmlre_parse_state * state, t_unicode c);
static int rx_xml_scan_uint (struct xmlre_parse_state * state, t_uint * x);
static int rx_xml_scan_opt_uint (struct xmlre_parse_state * state, t_uint * x);
static int rx_xml_scan_single_character_escape (struct xmlre_parse_state * state, t_unicode * c);
static int rx_xml_scan_normal_char (struct xmlre_parse_state * state, t_unicode * c);
static int rx_xml_scan_multi_character_escape (struct xmlre_parse_state * state, bits * b);
static int block_name_cmp (t_uchar * source, t_uchar * canonical);
static int rx_xml_scan_category_escape (struct xmlre_parse_state * state, bits * b);
static int rx_xml_scan_character_class (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_group (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_range (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_character_class_escape (struct xmlre_parse_state * state, bits * set);
static int rx_xml_scan_range_char (struct xmlre_parse_state * state, t_unicode * c);
static bits rx_xml_dot_set (struct xmlre_parse_state * state);
static bits rx_xml_space_set (void);
static bits rx_xml_name_initial_set (void);
static bits rx_xml_name_set (void);
static bits rx_xml_letter_set (void);
static bits rx_xml_unicode_category_set (struct xmlre_parse_state * state,
					 enum uni_general_category c);
static bits rx_xml_word_category_set (struct xmlre_parse_state * state);


/************************************************************************
 *(h2 "Compiling XML Regular Expressions")
 * 
 * The functions in this section compile regular expressions using the
 * syntax defined for XML Schema.  (See also xref:"Using Rx in XML
 * Processors".)
 */

/*(c rx_xml_recomp)
 * enum rx_xml_recomp_errno
 * rx_xml_recomp (rx_xml_rebuf * re,
 *                enum uni_encoding_scheme encoding,
 *                uni_string * source,
 *                size_t length);
 * 
 * Compile a regular expression using XML syntax.
 * 
 * `re' is an opaque output parameter which will be filled with
 * the compiled expression.
 * 
 * `encoding' describes the encoding scheme of the source expression.
 * It may be any of:
 * 
 *	uni_iso8859_1,		8-bit characters (u+0..u+255 only)
 *	uni_utf8,		UTF-8 encoding
 *	uni_utf16,		UTF-16 in native byte order
 * 
 * Note that the encoding scheme of the source expression has no bearing
 * on the encoding scheme of strings tested for a match.
 * 
 * `source' points to the source for the regular expression, a string
 * encoded in the manner specified by `encoding'.  
 * 
 * `length' is the size of the source expression, measured in code units.
 * 		
 * Return 0 upon success, an error code otherwise.  See
 * `<hackerlab/rx-unicode/re.h>' for the list of error codes.
 * The list of error codes is likely to change in future releaes.
 * Future releases will add a function for translating 
 * error codes to error messages.
 */
enum rx_xml_recomp_errno
rx_xml_recomp (rx_xml_rebuf * re,
	       enum uni_encoding_scheme encoding,
	       uni_string source,
	       size_t length)
{
  return rx_xml_recomp_opts (re, encoding, source, length, rx_xml_syntax_xml, xml_charset);
}


/*(c rx_xml_recomp_branch)
 * enum rx_xml_recomp_errno
 * rx_xml_recomp_branch (rx_xml_rebuf * re,
 *                       enum uni_encoding_scheme encoding,
 *                       uni_string source,
 *                       size_t length);
 * 
 * Compile a Unicode regular expression in XML syntax adding an
 * alternative branch to an already compiled expression.
 * 
 * Parameters and return values are as to xref:"rx_xml_recomp" except
 * that `re' must contain an already compiled expression.
 * 
 * If, on entry, `re' contains a compiled form of expression `RE1',
 * and `source' points to expression `RE2', the result is the same as
 * if the expression:
 * 
 * 		(RE1)|(RE2)
 * 
 * were compiled.
 */
enum rx_xml_recomp_errno
rx_xml_recomp_branch (rx_xml_rebuf * re,
		      enum uni_encoding_scheme encoding,
		      uni_string source,
		      size_t length)
{
  return rx_xml_recomp_opts (re, encoding, source, length, rx_xml_syntax_xml | rx_xml_syntax_add_branch, xml_charset);
}


/************************************************************************
 *(h2 "Compiling With Syntax Options")
 * 
 * |syntax options|
 * |compiling regular expressions (any syntax)|
 * 
 * The function in this section compiles Unicode regular expressions
 * using any of a variety of syntaxes which can be selected by
 * options provided to `rx_xml_recomp_opts'.
 * 
 */

/*(c rx_xml_recomp_opts)
 * enum rx_xml_recomp_errno
 * rx_xml_recomp_opts (rx_xml_rebuf * re,
 *                     enum uni_encoding_scheme encoding,
 *                     uni_string source,
 *                     size_t length,
 *                     t_ulong syntax_options,
 *                     bits cset);
 * 
 * Compile a Unicode regular expression.
 * 
 * Parameters `re', `encoding', `source', and `length' are as to
 * xref:"rx_xml_recomp".
 * 
 * `syntax_options' indicates what regular expression syntax to use
 * and how to interpret that syntax.  It is a bit-wise ^or^ of any of:
 * 
 *	rx_xml_syntax_unicode_escapes
 * 	    Permit numeric character escapes ("\u+xxxx" and "\v+xxxxxx")
 * 
 *	rx_xml_syntax_consistent_metacharacters
 * 	    Require all special characters to be escaped in 
 * 	    character class expressions.  Ordinarilly,
 * 	    some special characters do not need to be 
 * 	    escaped in character class expressions (as per
 * 	    XML syntax).
 * 
 *	rx_xml_syntax_dot_dot_ranges
 * 	    Use ".." instead of "-" to indicate character ranges
 * 	    in character class expressions ("[a..z]")
 * 
 *	rx_xml_syntax_carrot_set_difference
 * 	    Use "^" instead of "-" to indicate character set
 * 	    subtraction ("[\p{L}^[a..z]]")
 * 
 *	rx_xml_syntax_add_branch
 *	    `re' must contain a previously compiled expression.
 * 	    Compile this expression as an alternative branch.
 * 
 *	rx_xml_syntax_no_newlines
 *	rx_xml_syntax_no_cr
 *	rx_xml_syntax_no_linesep
 *	    Do not include newline (carriage return, line separator) 
 * 	    in the character set matched by `.' or (implicitly) in
 * 	    negated character set expressions.
 * 
 *	rx_xml_syntax_dot_star_prefix
 * 	    Compile the expression as if it were prefixed by:
 * 
 * 		`(.*)'
 * 
 * Passing 0 or `rx_xml_syntax_xml' for `syntax_options' 
 * causes XML syntax to be used.
 * 
 * `cset' is the set of all valid code code points.
 * 
 * Common choices for `cset' are:
 * 
 * 	xml_charset	-- the set of code points permitted in 
 * 			   XML documents (declared in 
 * 			   <hackerlab/xml/charsets.h>
 * 
 * 	unidata_bitset_universal -- the set of all assigned
 * 			   code points (declared in
 * 			   <hackerlab/unicode/unicode.h>
 * 	
 */
enum rx_xml_recomp_errno
rx_xml_recomp_opts (rx_xml_rebuf * re,
		    enum uni_encoding_scheme encoding,
		    uni_string src,
		    size_t len,
		    t_ulong syntax_options,
		    bits cset)
{
  t_uchar * source;
  size_t n_bytes;
  struct xmlre_parse_state state;
  jmp_buf escape;

  source = (t_uchar *)src;
  n_bytes = len * uni_code_unit_size (encoding);

  mem_set0 ((t_uchar *)re, sizeof (*re));
  mem_set0 ((t_uchar *)&state, sizeof (state));

  state.encoding = encoding;
  state.complete_cset = xml_charset;
  state.scan = uni_encoding_iscan_fn (encoding);
  state.source = source;
  state.len = n_bytes;
  state.pos = 0;
  state.syntax_options = syntax_options;

  state.root = 0;

  state.err_escape = &escape;
  if (setjmp (*state.err_escape))
    {
    error:
      rx_free_exp (state.root);
      return state.err;
    }

  rx_xml_parse_regexp (&state, &state.root);

  if (!rx_xml_eop (&state))
    {
      state.err = rx_xml_recomp_GARBAGE_AFTER_REGEXP;
      goto error;
    }

  if (syntax_options & rx_xml_syntax_dot_star_prefix)
    {
      bits dot_set;
      struct rx_exp_node * dot;
      struct rx_exp_node * dot_star;
      struct rx_exp_node * concat;

      dot_set = rx_xml_dot_set (&state);
      if (!dot)
	{
	  state.err = rx_xml_recomp_OUT_OF_MEMORY;
	  goto error;
	}

      dot = rx_mk_r_cset_take (r_cset, (1 << 21), dot_set);
      if (!dot)
	{
	  bits_free (dot_set);
	  state.err = rx_xml_recomp_OUT_OF_MEMORY;
	  goto error;
	}

      dot_star = rx_mk_r_monop (r_star, dot);
      if (!dot_star)
	{
	  rx_free_exp (dot);
	  state.err = rx_xml_recomp_OUT_OF_MEMORY;
	  goto error;
	}

      concat = rx_mk_r_binop (r_concat, dot_star, state.root);
      if (!concat)
	{
	  rx_free_exp (dot_star);
	  state.err = rx_xml_recomp_OUT_OF_MEMORY;
	  goto error;
	}
      state.root = concat;
    }

  if (syntax_options & rx_xml_syntax_add_branch)
    {
      struct rx_exp_node * n;
      n = rx_mk_r_binop (r_alternate, re->exp, state.root);
      if (!n)
	{
	  state.err = rx_xml_recomp_OUT_OF_MEMORY;
	  goto error;
	}
      state.root = n;
    }

  re->exp = state.root;
  re->nfa = 0;
  return rx_xml_recomp_OK;
}




/************************************************************************
 *(h2 "Freeing a Compiled XML Regular Expression")
 * 
 * 
 */


/*(c rx_xml_free_re)
 * void rx_xml_free_re (rx_xml_rebuf * re);
 * 
 * Release all storage associated with a previously compiled expression.
 * This does not free the memory pointed to by `re'.
 * 
 */
void
rx_xml_free_re (rx_xml_rebuf * re)
{
  rx_free_exp (re->exp);
  rx_free_unfa (re->nfa);
  mem_set0 ((t_uchar *)re, sizeof (*re));
}



/************************************************************************
 *(h2 "Comparing a String To An XML Regular Expression")
 * 
 */


/*(c rx_xml_is_match)
 * int rx_xml_is_match (enum rx_xml_rematch_errno * errn,
 *                      rx_xml_rebuf * re,
 *                      enum uni_encoding_scheme encoding,
 *                      uni_string * string,
 *                      size_t length);
 * 
 * Compare the compiled expression `re' to `string'.  Return 1 if the
 * entire string matches, 0 if it does not, and `-1' if an error
 * occurs.
 * 
 * `errn' is an output parameter that returns an error code if an
 * error occurs (see `<hackerlab/rx-unicode/re.h>' for the list of error
 * codes.  The list of error codes is likely to change in future
 * releaes.  Future releases will add a function for translating error
 * codes to error messages.
 * 
 * `re' is a previously compiled regular expression.
 * 
 * `encoding' describes the encoding scheme of `string'.
 * It may be any of:
 * 
 *	uni_iso8859_1,		8-bit characters (u+0..u+255 only)
 *	uni_utf8,		UTF-8 encoding
 *	uni_utf16,		UTF-16 in native byte order
 * 
 * `string' is the string to test for a match, encoded according to
 * `encoding'.  
 * 
 * `length' is the length of that string in code units.
 */
int
rx_xml_is_match (enum rx_xml_rematch_errno * errn,
		 rx_xml_rebuf * re,
		 enum uni_encoding_scheme encoding,
		 uni_string string,
		 size_t length)
{
  t_uchar * str;
  size_t len;
  int adv;
  int label;
  struct rx_dfa dfa;
  int storage_unit_size;

  str = (t_uchar *)string;
  len = length * uni_code_unit_size (encoding);

  if ((encoding == uni_utf16) && (len & 1))
    {
      *errn = rx_xml_rematch_ILLEGAL_STR;
      return -1;
    }

  mem_set0 ((t_uchar *)&dfa, sizeof (dfa));

  if (!re->nfa)
    {
      re->nfa = rx_unfa (re->exp, (1 << 21));
      if (!re->nfa)
	{
	  *errn = rx_xml_rematch_OUT_OF_MEMORY;
	  return -1;
	}
    }

  rx_init_dfa_from_nfa (&dfa, re->nfa->nfa);

  storage_unit_size = ((encoding == uni_utf16) ? 2 : 1);
  if (rx_dfa_goto_start_superstate (&dfa, storage_unit_size))
    {
      *errn = rx_xml_rematch_OUT_OF_MEMORY;
      return -1;
    }

  switch (encoding)
    {
    default:
    case uni_iso8859_1:
      adv = rx_dfa_iso8859_1_fits (&label, &dfa, str, len);
      break;
    case uni_utf8:
      adv = rx_dfa_utf8_fits (&label, &dfa, str, len);
      break;
    case uni_utf16:
      adv = rx_dfa_utf16_fits (&label, &dfa, (t_uint16 *)str, len / 2);
      break;
    }

  rx_clear_dfa_state (&dfa);

  if (adv < 0)
    {
      *errn = rx_xml_rematch_OUT_OF_MEMORY;
      return -1;
    }
  return !!label;
}


/*(c rx_xml_longest_match)
 * enum rx_xml_longest_status
 * rx_xml_longest_match (enum rx_xml_rematch_errno * errn,
 *                       size_t * match_len,
 *                       rx_xml_rebuf * re,
 *                       enum uni_encoding_scheme encoding,
 *                       uni_string string,
 *                       size_t length)
 * 
 * Look for the longest matching prefix of `string'.  There are
 * five possible return values:
 * 
 *     rx_xml_longest_error
 *		An error occured (check *errn).
 * 
 *     rx_xml_longest_out_of_input_match
 * 		A match was found.  If the string were longer,
 *		a longer match might be found.
 * 
 *     rx_xml_longest_out_of_input_nomatch
 * 		No match was found.  If the string were longer,
 * 		a match might be found.
 * 
 *     rx_xml_longest_found
 * 		The longest possible match was found.
 * 
 *     rx_xml_longest_nomatch
 * 		No prefix matches.
 * 
 * `errn' is an output parameter that returns an error code if an
 * error occurs (see `<hackerlab/rx-unicode/re.h>' for the list of error
 * codes).  The list of error codes is likely to change in future
 * releaes.  Future releases will add a function for translating error
 * codes to error messages.
 * 
 * `match_len' returns the length (in code units) of the longest match
 * found (if any).
 * 
 * `re' is a previously compiled regular expression.
 * 
 * `encoding' describes the encoding scheme of `string'.
 * It may be any of:
 * 
 *	uni_iso8859_1,		8-bit characters (u+0..u+255 only)
 *	uni_utf8,		UTF-8 encoding
 *	uni_utf16,		UTF-16 in native byte order
 * 
 * `string' is the string to test for a match, encoded according to
 * `encoding'.  
 * 
 * `length' is the length of that string in code units.
 */
enum rx_xml_longest_status
rx_xml_longest_match (enum rx_xml_rematch_errno * errn,
		      size_t * match_len,
		      rx_xml_rebuf * re,
		      enum uni_encoding_scheme encoding,
		      uni_string string,
		      size_t length)
{
  size_t code_unit_size;
  t_uchar * str;
  size_t len;
  struct rx_dfa dfa;
  int found_match;
  size_t best_len;
  size_t pos;
  int storage_unit_size;

  code_unit_size = uni_code_unit_size (encoding);
  str = (t_uchar *)string;
  len = length * code_unit_size;

  if ((encoding == uni_utf16) && (len & 1))
    {
      *errn = rx_xml_rematch_ILLEGAL_STR;
      return rx_xml_longest_error;
    }

  mem_set0 ((t_uchar *)&dfa, sizeof (dfa));

  if (!re->nfa)
    {
      re->nfa = rx_unfa (re->exp, (1 << 21));
      if (!re->nfa)
	{
	out_of_memory:
	  *errn = rx_xml_rematch_OUT_OF_MEMORY;
	  return rx_xml_longest_error;
	}
    }

  rx_init_dfa_from_nfa (&dfa, re->nfa->nfa);

  storage_unit_size = ((encoding == uni_utf16) ? 2 : 1);
  if (rx_dfa_goto_start_superstate (&dfa, 1))
    goto out_of_memory;

  found_match = !!rx_dfa_tag (&dfa);
  best_len = 0;
  pos = 0;

  while (pos < len)
    {
      int adv;
      size_t amt;

      switch (encoding)
	{
	default:
	case uni_iso8859_1:
	  adv = rx_dfa_iso8859_1_advance_to_final (&amt, &dfa, str + pos, len - pos);
	  break;
	case uni_utf8:
	  adv = rx_dfa_utf8_advance_to_final (&amt, &dfa, str + pos, len - pos);
	  break;
	case uni_utf16:
	  adv = rx_dfa_utf16_advance_to_final (&amt, &dfa, (t_uint16 *)(str + pos), (len - pos) / 2);
	  amt *= 2;
	  break;
	}

      if (adv < 0)
	{
	  rx_clear_dfa_state (&dfa);
	  goto out_of_memory;
	}

      if (adv == 0)
	{
	  rx_clear_dfa_state (&dfa);
	  if (!found_match)
	    {
	      return rx_xml_longest_nomatch;
	    }
	  else
	    {
	      *match_len = best_len / code_unit_size;
	      return rx_xml_longest_found;
	    }
	}

      if (rx_dfa_tag (&dfa))
	{
	  best_len += amt;
	  found_match = 1;
	}
      pos += amt;
    }

  if (found_match)
    {
      *match_len = best_len / code_unit_size;
      return rx_xml_longest_out_of_input_match;
    }
  else
    {
      *match_len = best_len / code_unit_size;
      return rx_xml_longest_out_of_input_nomatch;
    }
}



/*(c rx_xml_prefix_match)
 * enum rx_xml_prefix_status
 * rx_xml_prefix_match (enum rx_xml_rematch_errno * errn,
 *                      rx_xml_rebuf * re,
 *                      enum uni_encoding_scheme encoding,
 *                      uni_string string,
 *                      size_t length);
 * 
 * Look for any matching prefix of `string'.  Note that this
 * function does not look for the *longest* matching prefix,
 * and does not return the length of the prefix found -- it
 * merely verifies the existence of *some* matching prefix.
 * 
 * There are four possible return values:
 * 
 *     rx_xml_prefix_error
 *		An error occured (check *errn).
 * 
 *     rx_xml_prefix_out_of_input
 * 		No match was found.  If the string were longer,
 *		a match might be found.
 * 
 *     rx_xml_prefix_found
 * 		A matching prefix was found.
 * 
 *     rx_xml_prefix_nomatch
 * 		No prefix matches.
 * 
 * `errn' is an output parameter that returns an error code if an
 * error occurs (see `<hackerlab/rx-unicode/re.h>' for the list of error
 * codes).  The list of error codes is likely to change in future
 * releaes.  Future releases will add a function for translating error
 * codes to error messages.
 * 
 * `re' is a previously compiled regular expression.
 * 
 * `encoding' describes the encoding scheme of `string'.
 * It may be any of:
 * 
 *	uni_iso8859_1,		8-bit characters (u+0..u+255 only)
 *	uni_utf8,		UTF-8 encoding
 *	uni_utf16,		UTF-16 in native byte order
 * 
 * `string' is the string to test for a match, encoded according to
 * `encoding'.  
 * 
 * `length' is the length of that string in code units.
 */
enum rx_xml_prefix_status
rx_xml_prefix_match (enum rx_xml_rematch_errno * errn,
		     rx_xml_rebuf * re,
		     enum uni_encoding_scheme encoding,
		     uni_string string,
		     size_t length)
{
  t_uchar * str;
  size_t len;
  struct rx_dfa dfa;
  int adv;
  size_t amt;

  str = (t_uchar *)string;
  len = length * uni_code_unit_size (encoding);

  if ((encoding == uni_utf16) && (len & 1))
    {
      *errn = rx_xml_rematch_ILLEGAL_STR;
      return rx_xml_prefix_error;
    }

  mem_set0 ((t_uchar *)&dfa, sizeof (dfa));

  if (!re->nfa)
    {
      re->nfa = rx_unfa (re->exp, (1 << 21));
      if (!re->nfa)
	{
	out_of_memory:
	  *errn = rx_xml_rematch_OUT_OF_MEMORY;
	  return rx_xml_prefix_error;
	}
    }

  rx_init_dfa_from_nfa (&dfa, re->nfa->nfa);
  if (rx_dfa_goto_start_superstate (&dfa, 1))
    goto out_of_memory;

  switch (encoding)
    {
    default:
    case uni_iso8859_1:
      adv = rx_dfa_iso8859_1_advance_to_final (&amt, &dfa, str, len);
      break;
    case uni_utf8:
      adv = rx_dfa_utf8_advance_to_final (&amt, &dfa, str, len);
      break;
    case uni_utf16:
      adv = rx_dfa_utf16_advance_to_final (&amt, &dfa, (t_uint16 *)str, len / 2);
      break;
    }
  
  if (adv < 0)
    {
      rx_clear_dfa_state (&dfa);
      goto out_of_memory;
    }
  
  if (adv == 0)
    {
      rx_clear_dfa_state (&dfa);
      return rx_xml_prefix_nomatch;
    }
  
  if (rx_dfa_tag (&dfa))
    {
      return rx_xml_prefix_found;
    }

  return rx_xml_prefix_out_of_input;
}




static void
rx_xml_parse_regexp (struct xmlre_parse_state * state, struct rx_exp_node ** where)
{
  if (rx_xml_eop (state))
    {
      *where = 0;
      return;
    }

  rx_xml_parse_branch (state, where);

  while (rx_xml_scan_op (state, '|'))
    {
      struct rx_exp_node * alt;
      
      alt = rx_mk_r_binop (r_alternate, *where, 0);
      if (!alt)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = alt;
      rx_xml_parse_branch (state, &alt->right);
    }
}


static void
rx_xml_parse_branch (struct xmlre_parse_state * state, struct rx_exp_node ** where)
{
  rx_xml_parse_piece (state, where);

  while (*where && !rx_xml_eop (state) && !rx_xml_scan_op_ahead (state, '|') && !rx_xml_scan_op_ahead (state, ')'))
    {
      struct rx_exp_node * branch;

      branch = rx_mk_r_binop (r_concat, *where, 0);
      if (!branch)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}	
      *where = branch;
      rx_xml_parse_piece (state, &branch->right);
    }
}


static void
rx_xml_parse_piece (struct xmlre_parse_state * state, struct rx_exp_node ** where)
{
  rx_xml_parse_atom (state, where);

  if (rx_xml_scan_op (state, '?'))
    {
      struct rx_exp_node * n;
      n = rx_mk_r_binop (r_alternate, *where, 0);
      if (!n)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = n;
    }
  else if (rx_xml_scan_op (state, '*'))
    {
      struct rx_exp_node * n;
      n = rx_mk_r_monop (r_star, *where);
      if (!n)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = n;
    }
  else if (rx_xml_scan_op (state, '+'))
    {
      struct rx_exp_node * n;
      rx_save_exp (*where);
      n = rx_mk_r_binop (r_concat, *where, rx_mk_r_monop (r_star, *where));
      if (!n)
	{
	  rx_free_exp (*where);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = n;
    }
  else if (rx_xml_scan_op (state, '{'))
    {
      t_uint m;
      t_uint n;
      int unbounded;
      struct rx_exp_node * exp;

      unbounded = 0;
      exp = *where;
      rx_save_exp (exp);

      rx_xml_scan_uint (state, &m);
      if (rx_xml_scan_op (state, '}'))
	{
	  n = m;
	}
      else
	{
	  if (!rx_xml_scan_op (state, ','))
	    {
	      rx_free_exp (exp);
	      state->err = rx_xml_recomp_MISSING_COMMA;
	      longjmp (*state->err_escape, 1);
	    }

	  if (!rx_xml_scan_opt_uint (state, &n))
	    {
	      n = m;
	      unbounded = 1;
	    }
	  if (!rx_xml_scan_op (state, '}'))
	    {
	      rx_free_exp (exp);
	      state->err = rx_xml_recomp_MISSING_BRACE;
	      longjmp (*state->err_escape, 1);
	    }
	}

      /* m and n set.
       * *where == exp == repeated expression (2 ref counts)
       */
      if ((m == 0) && (n == 0))
	{
	  rx_free_exp (*where);
	  *where = 0;
	}
      else
	{
	  if (m > n)
	    {
	      rx_free_exp (exp);
	      state->err = rx_xml_recomp_BAD_DUPLICATION_RANGE;
	      longjmp (*state->err_escape, 1);
	    }
	  if (m == 0)
	    {
	      struct rx_exp_node * n;
	      n = rx_mk_r_binop (r_alternate, 0, *where);
	      if (!n)
		{
		  rx_free_exp (exp);
		  state->err = rx_xml_recomp_OUT_OF_MEMORY;
		  longjmp (*state->err_escape, 1);
		}
	      *where = n;
	      where = &(*where)->right;
	      m = 1;
	    }

	  *where = rx_xml_build_interval_tree (state, *where, m, n);
	}
      /* *where == RE{m,n}
       * exp == RE	(w/ ref count)
       */
      if (!unbounded)
	rx_free_exp (exp);
      else
	{
	  struct rx_exp_node * star;
	  struct rx_exp_node * concat;

	  star = rx_mk_r_monop (r_star, exp);
	  if (!star)
	    {
	      rx_free_exp (exp);
	      state->err = rx_xml_recomp_OUT_OF_MEMORY;
	      longjmp (*state->err_escape, 1);
	    }

	  concat = rx_mk_r_binop (r_concat, *where, star);
	  if (!concat)
	    {
	      rx_free_exp (star);
	      state->err = rx_xml_recomp_OUT_OF_MEMORY;
	      longjmp (*state->err_escape, 1);
	    }
	  *where = concat;
	}
    }
}


static struct rx_exp_node *
rx_xml_build_interval_tree (struct xmlre_parse_state * state, struct rx_exp_node * exp, t_uint from, t_uint to)
{
  t_uint mandatory;
  t_uint optional;
  struct rx_exp_node * mandatory_tree;
  struct rx_exp_node * opt_exp;
  struct rx_exp_node * opt_tree;
  struct rx_exp_node * combined;

  mandatory = from;
  optional = to - from;

  /* exp has ref count to take over
   *
   * mandatory >= 1
   * optional >= 0
   */
  mandatory_tree = rx_xml_build_repeat_tree (exp, mandatory);

  if (!mandatory_tree)
    {
      rx_free_exp (exp);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  if (!optional)
    {
      rx_free_exp (exp);
      return mandatory_tree;
    }

  opt_exp = rx_mk_r_binop (r_alternate, exp, 0);
  if (!opt_exp)
    {
      rx_free_exp (mandatory_tree);
      rx_free_exp (exp);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  opt_tree = rx_xml_build_repeat_tree (opt_exp, optional);
  rx_free_exp (opt_exp);
  if (!opt_tree)
    {
      rx_free_exp (mandatory_tree);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  combined = rx_mk_r_binop (r_concat, mandatory_tree, opt_tree);
  if (!combined)
    {
      rx_free_exp (mandatory_tree);
      rx_free_exp (opt_tree);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }
  return combined;
}

static struct rx_exp_node *
rx_xml_build_repeat_tree (struct rx_exp_node * n, t_uint amt)
{
  t_uint left;
  t_uint right;
  struct rx_exp_node * left_tree;
  struct rx_exp_node * right_tree;
  struct rx_exp_node * combined;

  if (amt == 1)
    {
      rx_save_exp (n);
      return n;
    }

  left = amt / 2;
  right = amt - left;

  left_tree = rx_xml_build_repeat_tree (n, left);
  right_tree = rx_xml_build_repeat_tree (n, right);

  if (!left_tree || !right_tree)
    {
      rx_free_exp (left_tree);
      rx_free_exp (right_tree);
      return 0;
    }

  combined = rx_mk_r_binop (r_concat, left_tree, right_tree);
  if (!combined)
    {
      rx_free_exp (left_tree);
      rx_free_exp (right_tree);
      return 0;
    }

  return combined;
}


static void
rx_xml_parse_atom (struct xmlre_parse_state * state, struct rx_exp_node ** where)
{
  t_unicode c;
  bits b;

  if (rx_xml_scan_op (state, '('))
    {
      rx_xml_parse_regexp (state, where);
      if (!rx_xml_scan_op (state, ')'))
	{
	  state->err = rx_xml_recomp_MISSING_RPAREN;
	  longjmp (*state->err_escape, 1);
	}
    }
  else if (rx_xml_scan_op_ahead (state, '['))
    {
      bits set;
      struct rx_exp_node * n;

      if (!rx_xml_scan_character_class (state, &set))
	{
	  state->err = rx_xml_recomp_BOGUS_CHARACTER_CLASS;
	  longjmp (*state->err_escape, 1);
	}

      if ((state->syntax_options & rx_xml_syntax_no_newlines) && bits_remove (set, '\n'))
	{
	  bits_free (set);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      if ((state->syntax_options & rx_xml_syntax_no_cr) && bits_remove (set, '\r'))
	{
	  bits_free (set);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      if ((state->syntax_options & rx_xml_syntax_no_linesep) && bits_remove (set, 0x2028))
	{
	  bits_free (set);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      n = rx_mk_r_cset_take (r_cset, (1 << 21), set);
      if (!n)
	{
	  bits_free (set);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      *where = n;
      return;
    }
  else if (   rx_xml_scan_category_escape (state, &b)
	   || rx_xml_scan_multi_character_escape (state, &b))
    {
      struct rx_exp_node * n;

      if (!b)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      
      if ((state->syntax_options & rx_xml_syntax_no_newlines) && bits_remove (b, '\n'))
	{
	  bits_free (b);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      if ((state->syntax_options & rx_xml_syntax_no_cr) && bits_remove (b, '\r'))
	{
	  bits_free (b);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      if ((state->syntax_options & rx_xml_syntax_no_linesep) && bits_remove (b, 0x2028))
	{
	  bits_free (b);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      n = rx_mk_r_cset_take (r_cset, (1 << 21), b);
      if (!n)
	{
	  bits_free (b);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = n;
      return;
    }
  else if (rx_xml_scan_single_character_escape (state, &c))
    {
      t_uchar str[4];
      size_t pos;
      int len;
      struct rx_exp_node * n;

      if ((state->syntax_options & rx_xml_syntax_no_newlines) && (c == '\n'))
	{
	  state->err = rx_xml_recomp_BOGUS_CHAR;
	  longjmp (*state->err_escape, 1);
	}
      if ((state->syntax_options & rx_xml_syntax_no_cr) && (c == '\r'))
	{
	  state->err = rx_xml_recomp_BOGUS_CHAR;
	  longjmp (*state->err_escape, 1);
	}
      if ((state->syntax_options & rx_xml_syntax_no_linesep) && (c == 0x2028))
	{
	  state->err = rx_xml_recomp_BOGUS_CHAR;
	  longjmp (*state->err_escape, 1);
	}

      pos = 0;
      len = uni_utf16_iput (str, &pos, (size_t)4, c);
      invariant (len > 0);
      n = rx_mk_r_str (r_string, str, pos, uni_utf16);
      if (!n)
	{
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}
      *where = n;
      return;
    }
  else
    {
      size_t start;
      size_t saved;
      size_t end;
      int n_chars;
      t_uchar * str;
      size_t len;

      start = state->pos;
      end = start;
      n_chars = 0;

      while (1)
	{
	  saved = state->pos;
	  if (!rx_xml_scan_normal_char (state, &c))
	    break;

	  if ((state->syntax_options && rx_xml_syntax_no_newlines) && (c == '\n'))
	    {
	      state->err = rx_xml_recomp_BOGUS_CHAR;
	      longjmp (*state->err_escape, 1);
	    }

	  if ((state->syntax_options && rx_xml_syntax_no_cr) && (c == '\r'))
	    {
	      state->err = rx_xml_recomp_BOGUS_CHAR;
	      longjmp (*state->err_escape, 1);
	    }
	  if ((state->syntax_options && rx_xml_syntax_no_linesep) && (c == 0x2028))
	    {
	      state->err = rx_xml_recomp_BOGUS_CHAR;
	      longjmp (*state->err_escape, 1);
	    }

	  if (   rx_xml_scan_op_ahead (state, '?')
	      || rx_xml_scan_op_ahead (state, '*')
	      || rx_xml_scan_op_ahead (state, '+')
	      || rx_xml_scan_op_ahead (state, '{'))
	    {
	      if (n_chars == 0)
		{
		  end = state->pos;
		  n_chars = 1;
		  break;
		}
	      else
		{
		  state->pos = saved;
		  break;
		}
	    }
	  ++n_chars;
	}

      if (n_chars == 0)
	{
	  state->err = rx_xml_recomp_BOGUS_CHAR;
	  longjmp (*state->err_escape, 1);
	}

      end = state->pos;
      len = (end - start);
      str = state->source + start;

      {
	struct rx_exp_node * n;
	n = rx_mk_r_str (r_string, str, len, state->encoding);
	if (!n)
	  {
	    state->err = rx_xml_recomp_OUT_OF_MEMORY;
	    longjmp (*state->err_escape, 1);
	  }
	*where = n;
      }
      return;
    }
}






static int
rx_xml_eop (struct xmlre_parse_state * state)
{
  return state->pos >= state->len;
}


static int
rx_xml_scan_op (struct xmlre_parse_state * state, t_unicode c)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);
  if (scanned == c)
    {
      state->pos = pos;
      return 1;
    }
  else
    return 0;
}


static int
rx_xml_scan_digit (struct xmlre_parse_state * state, int * d)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);
  
  if (char_is_digit (scanned))
    {
      *d = scanned - '0';
      state->pos = pos;
      return 1;
    }
  else
    return 0;
}


static int
rx_xml_scan_op_ahead (struct xmlre_parse_state * state, t_unicode c)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);
  if (scanned == c)
    {
      return 1;
    }
  else
    return 0;
}


static int
rx_xml_scan_uint (struct xmlre_parse_state * state, t_uint * x)
{
  int d;

  if (!rx_xml_scan_digit (state, &d))
    return 0;

  *x = d;

  while (rx_xml_scan_digit (state, &d))
    {
      if (*x > (UINT_MAX / 10))
	return 0;
      *x *= 10;
      if (*x > (UINT_MAX - d))
	return 0;
      *x += d;
    }
  return 1;
}


static int
rx_xml_scan_opt_uint (struct xmlre_parse_state * state, t_uint * x)
{
  size_t saved_pos;
  int d;

  saved_pos = state->pos;

  if (!rx_xml_scan_digit (state, &d))
    return 0;

  *x = d;

  while (rx_xml_scan_digit (state, &d))
    {
      if (*x > (UINT_MAX / 10))
	{
	  state->pos = saved_pos;
	  return 0;
	}
      *x *= 10;
      if (*x > (UINT_MAX - d))
	{
	  state->pos = saved_pos;
	  return 0;
	}
      *x += d;
    }
  return 1;
}


static int
rx_xml_scan_single_character_escape (struct xmlre_parse_state * state, t_unicode * c)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);
  if (scanned != '\\')
    return 0;

  if (pos >= state->len)
    return 0;
  scanned = state->scan (state->source, &pos, state->len);
  switch (scanned)
    {
      {
      default:
	return 0;
      }
	
      /* single character escapes: */
      {
      case 'u':
      case 'v':
	{
	  int x;
	  int l;
	  t_unicode v;

	  if (!(state->syntax_options & rx_xml_syntax_unicode_escapes))
	    return 0;

	  v = 0;
	  l = ((scanned == 'u') ? 4 : 6);
	  for (x = 0; x < l; ++x)
	    {
	      if (pos >= state->len)
		return 0;
	      scanned = state->scan (state->source, &pos, state->len);
	      switch (scanned)
		{
		default:
		  return 0;

		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		  v *= 16;
		  v += 10 + scanned - 'a';
		  break;

		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
		  v *= 16;
		  v += 10 + scanned - 'A';
		  break;

		case '0': case '1': case '2': case '3': case '4': 
		case '5': case '6': case '7': case '8': case '9': 
		  v *= 16;
		  v += 10 + scanned - '0';
		  break;
		}
	    }

	  if (!bits_is_member (state->complete_cset, v))
	    return 0;

	  *c = v;
	  state->pos = pos;
	  return 1;
	}

      case 'n':
	*c = '\n';
	state->pos = pos;
	return 1;
      case 'r':
	*c = '\r';
	state->pos = pos;
	return 1;
      case 't':
	*c = '\t';
	state->pos = pos;
	return 1;
	    
      case '-': 
      case '|': case '^': case '.': case '\\': case '?': case '*':
      case '+': case '{': case '}': case '(': case ')': case '[': 
      case ']':

	*c = scanned;
	state->pos = pos;
	return 1;
      }
    }
}


static int
rx_xml_scan_normal_char (struct xmlre_parse_state * state, t_unicode * c)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);

  if (bits_is_member (state->complete_cset, scanned) && !xml_is_re_meta_char (scanned))
    {
      *c = scanned;
      state->pos = pos;
      return 1;
    }
  else
    return 0;
}


static int
rx_xml_scan_multi_character_escape (struct xmlre_parse_state * state, bits * b)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);

  if (scanned == '.')
    {
      state->pos = pos;
      *b = rx_xml_dot_set (state);
      return 1;
    }
  else if (scanned == '\\')
    {
      if (pos >= state->len)
	return 0;
      scanned = state->scan (state->source, &pos, state->len);
      switch (scanned)
	{
	default:
	  return 0;

	case 's':
	case 'S':
	  *b = rx_xml_space_set ();
	  break;
	case 'i':
	case 'I':
	  *b = rx_xml_name_initial_set ();
	  break;
	case 'c':
	case 'C':
	  *b = rx_xml_name_set ();
	  break;
	case 'd':
	case 'D':
	  *b = rx_xml_unicode_category_set (state, uni_general_category_Nd);
	  break;
	case 'w':
	case 'W':
	  *b = rx_xml_word_category_set (state);
	  break;
	}
      if (   !*b
	  || (   char_is_upper (scanned)
	      && bits_revdifference (*b, state->complete_cset)))
	{
	  bits_free (*b);
	  *b = 0;
	  return 0;
	}
      state->pos = pos;
      return 1;
    }
  return 0;
}


static int
block_name_cmp (t_uchar * source, t_uchar * canonical)
{
  while (1)
    {
      if (!*source && !*canonical)
	return 1;

      if (!*source || !*canonical)
	return 0;

      if (char_to_lower (*source) != char_to_lower (*canonical))
	{
	  while (char_is_space (*canonical))
	    ++canonical;
	  if (char_to_lower (*source) != char_to_lower (*canonical))
	    return 0;
	}
      ++source;
      ++canonical;
    }
}

static int
rx_xml_scan_category_escape (struct xmlre_parse_state * state, bits * b)
{
  size_t pos;
  t_unicode scanned;
  t_unicode op;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);

  if (scanned != '\\')
    return 0;

  if (pos >= state->len)
    return 0;
  op = state->scan (state->source, &pos, state->len);
  switch (op)
    {
    default:
      return 0;

    case 'p':
    case 'P':
      {
	t_uchar name[64];
	int x;

	if (   (pos >= state->len)
	    || ('{' != state->scan (state->source, &pos, state->len)))
	  {
	  bogus_category_escape:
	    state->err = rx_xml_recomp_BOGUS_CATEGORY_ESCAPE;
	    longjmp (*state->err_escape, 1);
	  }

	x = 0;
	while (1)
	  {
	    if (pos >= state->len)
	      return 0;
	    scanned = state->scan (state->source, &pos, state->len);
	    if (!char_is_ascii (scanned))
	      goto bogus_category_escape;
	    if (scanned == '}')
	      break;
	    name[x++] = (t_uchar)scanned;
	    if (x == (sizeof (name) - 1))
	      goto bogus_category_escape;
	  }
	name[x++] = 0;
	if ((x > 2) && (char_to_lower (name[0]) == 'i') && (char_to_lower (name[1]) == 's'))
	  {
	    bits set;
	    int x;
	    /* block escape */
	    for (x = 0; x < n_uni_blocks; ++x)
	      {
		if (block_name_cmp (name + 2, uni_blocks[x].name))
		  break;
	      }
	    if (x == n_uni_blocks)
	      goto bogus_category_escape;
	    set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
	    if (   !set
		|| bits_fill_range (set, uni_blocks[x].start, uni_blocks[x].end + 1))
	      {
		bits_free (set);
		state->err = rx_xml_recomp_OUT_OF_MEMORY;
		longjmp (*state->err_escape, 1);
	      }
	    *b = set;
	  }
	else
	  {
	    int cat;
	    /* category escape */
	    cat = uni_general_category_lookup (name);
	    if (cat < 0)
	      goto bogus_category_escape;

	    *b = rx_xml_unicode_category_set (state, cat);
	  }
	break;
      }
    }
  if (   !*b
      || (   char_is_upper (op)
	  && bits_revdifference (*b, state->complete_cset)))
    {
      bits_free (*b);
      *b = 0;
      return 0;
    }
  state->pos = pos;
  return 1;
}


static int
rx_xml_scan_character_class (struct xmlre_parse_state * state, bits * set)
{
  if (!rx_xml_scan_op (state, '['))
    return 0;
  
  *set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
  if (!*set)
    {
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  if (!rx_xml_scan_character_group (state, set))
    return 0;
  
  if ((state->syntax_options & rx_xml_syntax_carrot_set_difference) ? rx_xml_scan_op (state, '^') : rx_xml_scan_op (state, '-'))
    {
      jmp_buf * saved_escape;
      jmp_buf escape;
      bits set2;


      saved_escape = state->err_escape;
      state->err_escape = &escape;

      if (setjmp (*state->err_escape))
	{
	  state->err_escape = saved_escape;
	  bits_free (*set);
	  *set = 0;
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      if (!rx_xml_scan_character_class (state, &set2))
	{
	  bits_free (*set);
	  *set = 0;
	  state->err_escape = saved_escape;
	  return 0;
	}

      state->err_escape = saved_escape;
      if (bits_difference (*set, set2))
	{
	  bits_free (*set);
	  bits_free (set2);
	  *set = 0;
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      bits_free (set2);
    }

  if (!rx_xml_scan_op (state, ']'))
    {
      bits_free (*set);
      return 0;
    }

  return 1;
}


static int
rx_xml_scan_character_group (struct xmlre_parse_state * state, bits * set)
{
  int negated;

  negated = rx_xml_scan_op (state, '^');

  if (!rx_xml_scan_character_range (state, set))
    return 0;

  while (rx_xml_scan_character_range (state, set))
    ;

  if (negated && bits_revdifference (*set, state->complete_cset))
    {
      bits_free (*set);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  return 1;
}


static int
rx_xml_scan_character_range (struct xmlre_parse_state * state, bits * set)
{
  t_unicode first;
  t_unicode second;
  int is_range;

  if (rx_xml_scan_character_class_escape (state, set))
    return 1;

  if (!rx_xml_scan_range_char (state, &first))
    return 0;


  if (state->syntax_options & rx_xml_syntax_dot_dot_ranges)
    {
      if (!rx_xml_scan_op (state, '.'))
	is_range = 0;
      else if (rx_xml_scan_op (state, '.'))
	is_range = 1;
      else
	{
	  bits_free (*set);
	  state->err = rx_xml_recomp_BOGUS_CHAR;
	  longjmp (*state->err_escape, 1);
	}
    }
  else
    is_range = rx_xml_scan_op (state, '-');

  if (!is_range)
    {
      if (bits_adjoin (*set, first))
	{
	  bits_free (*set);
	  state->err = rx_xml_recomp_OUT_OF_MEMORY;
	  longjmp (*state->err_escape, 1);
	}

      return 1;
    }

  if (   !rx_xml_scan_range_char (state, &second)
      || (second < first))
    {
      bits_free (*set);
      state->err = rx_xml_recomp_BOGUS_CHARACTER_CLASS;
      longjmp (*state->err_escape, 1);
    }

  if (bits_fill_range (*set, first, second + 1))
    {
      bits_free (*set);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }

  return 1;
}


static int
rx_xml_scan_character_class_escape (struct xmlre_parse_state * state, bits * set)
{
  bits b;
  int stat;

  if (   !rx_xml_scan_category_escape (state, &b)
      && !rx_xml_scan_multi_character_escape (state, &b))
    return 0;

  stat = bits_union (*set, b);
  bits_free (b);
  if (stat)
    {
      bits_free (*set);
      state->err = rx_xml_recomp_OUT_OF_MEMORY;
      longjmp (*state->err_escape, 1);
    }
  return 1;
}


static int
rx_xml_scan_range_char (struct xmlre_parse_state * state, t_unicode * c)
{
  size_t pos;
  t_unicode scanned;

  if (rx_xml_eop (state))
    return 0;

  pos = state->pos;

  scanned = state->scan (state->source, &pos, state->len);

  if (scanned == '\\')
    {
      if (!rx_xml_scan_single_character_escape (state, c))
	return 0;
      if ((state->syntax_options & rx_xml_syntax_no_newlines) && (*c == '\n'))
	return 0;
      if ((state->syntax_options & rx_xml_syntax_no_cr) && (*c == '\r'))
	return 0;
      if ((state->syntax_options & rx_xml_syntax_no_linesep) && (*c == 0x2028))
	return 0;
      return 1;
    }

  if (scanned == rx_xml_syntax_consistent_metacharacters)
    {
      if (xml_is_re_meta_char (scanned))
	return 0;
    }
  else
    {
      /* Since we are not using consistent metachars syntax,
       * there is a special set of (excluded) metachars for
       * character range syntax.
       *
       * The XML spec has complicated and ambiguous rules for sometimes accepting
       * the metacharacters as valid range characters.   Because these rules
       * are ambiguous, we ignore them.
       */
      switch (scanned)
	{
	case '-':
	  if (!(   (state->syntax_options & rx_xml_syntax_dot_dot_ranges)
		&& (state->syntax_options & rx_xml_syntax_carrot_set_difference)))
	    return 0;
	  break;

	case '[': 
	case ']':
	case '^': 
	  return 0;

	default:
	  break;
	}
    }


  /* Not a metacharacter.  Is it a valid character?
   */
  if (!bits_is_member (state->complete_cset, scanned))
    return 0;

  if ((state->syntax_options & rx_xml_syntax_no_newlines) && (scanned == '\n'))
    return 0;
  if ((state->syntax_options & rx_xml_syntax_no_cr) && (scanned == '\r'))
    return 0;
  if ((state->syntax_options & rx_xml_syntax_no_linesep) && (scanned == 0x2028))
    return 0;

  *c = scanned;
  state->pos = pos;
  return 1;
}




static bits
rx_xml_dot_set (struct xmlre_parse_state * state)
{
  bits set;

  set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
  if (!set)
    return 0;

  if (   bits_union (set, state->complete_cset)
      || bits_remove (set, '\n')
      || bits_remove (set, '\r'))
    {
      bits_free (set);
      return 0;
    }
  return set;
}

static bits
rx_xml_space_set (void)
{
  bits set;

  set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);
  
  if (!set)
    return 0;

  if (   bits_adjoin (set, ' ')
      || bits_adjoin (set, '\t')
      || bits_adjoin (set, '\n')
      || bits_adjoin (set, '\r'))
    {
      bits_free (set);
      return 0;
    }
  return set;
}


static bits
rx_xml_name_initial_set (void)
{
  bits set;

  set = rx_xml_letter_set ();
  
  if (!set)
    return 0;

  if (   bits_adjoin (set, ':')
      || bits_adjoin (set, '_'))
    {
      bits_free (set);
      return 0;
    }
  return set;
}


static bits
rx_xml_name_set (void)
{
  bits set;

  set = rx_xml_name_initial_set ();
  
  if (!set)
    return 0;

  if (   bits_adjoin (set, '.')
      || bits_adjoin (set, '-')
      || bits_union (set, xml_digit_charset)
      || bits_union (set, xml_combining_charset)
      || bits_union (set, xml_extender_charset))
    {
      bits_free (set);
      return 0;
    }
  return set;
}


static bits
rx_xml_letter_set (void)
{
  bits set;

  set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);

  if (!set)
    return 0;

  if (   bits_union (set, xml_base_charset)
      || bits_union (set, xml_ideographic_charset))
    {
      bits_free (set);
      return 0;
    }
  return set;
}


static bits
rx_xml_unicode_category_set (struct xmlre_parse_state * state,
			     enum uni_general_category c)
{
  bits set;

  set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);

  if (!set)
    return 0;

  if (   bits_union (set, state->complete_cset)
      || bits_intersection (set, uni_general_category_bitset (c)))
    {
      bits_free (set);
      return 0;
    }
  return set;
}


static bits
rx_xml_word_category_set (struct xmlre_parse_state * state)
{
  bits set;

  set = bits_alloc (rx_nfa_cache_limits (), uni_bits_tree_rule);

  if (!set)
    return 0;

  if (   bits_union (set, state->complete_cset)
      || bits_difference (set, unidata_bitset_P)
      || bits_difference (set, unidata_bitset_Z)
      || bits_difference (set, unidata_bitset_C))
    {
      bits_free (set);
      return 0;
    }
  return set;
}
