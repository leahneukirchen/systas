/* tag: Tom Lord Tue Dec  4 14:41:43 2001 (re.h)
 */
/* re.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__RX_UNICODE__RE_H
#define INCLUDE__RX_UNICODE__RE_H



#include "hackerlab/unicode/unicode.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/nfa.h"



enum rx_xml_syntax_options
{
  rx_xml_syntax_xml = 0,
  rx_xml_syntax_unicode_escapes = 1,
  rx_xml_syntax_consistent_metacharacters = 2,
  rx_xml_syntax_dot_dot_ranges = 4,
  rx_xml_syntax_carrot_set_difference = 8,
  rx_xml_syntax_add_branch = 16,
  rx_xml_syntax_no_newlines = 32,
  rx_xml_syntax_no_cr = 64,
  rx_xml_syntax_no_linesep = 128,
  rx_xml_syntax_dot_star_prefix = 256,
};

#define rx_xml_syntax_unigrep (   rx_xml_syntax_unicode_escapes \
			       | rx_xml_syntax_consistent_metacharacters \
			       | rx_xml_syntax_dot_dot_ranges \
 			       | rx_xml_syntax_carrot_set_difference)


enum rx_xml_recomp_errno
{
  rx_xml_recomp_OK,
  rx_xml_recomp_OUT_OF_MEMORY,
  rx_xml_recomp_BAD_DUPLICATION_RANGE,
  rx_xml_recomp_GARBAGE_AFTER_REGEXP,
  rx_xml_recomp_MISSING_RPAREN,
  rx_xml_recomp_BOGUS_CHAR,
  rx_xml_recomp_MISSING_COMMA,
  rx_xml_recomp_MISSING_BRACE,
  rx_xml_recomp_BOGUS_CATEGORY_ESCAPE,
  rx_xml_recomp_BOGUS_CHARACTER_CLASS,
};


struct rx_xml_rebuf
{
  struct rx_exp_node * exp;
  struct rx_unfa * nfa;
};

typedef struct rx_xml_rebuf rx_xml_rebuf;




enum rx_xml_rematch_errno
{
  rx_xml_rematch_OK,
  rx_xml_rematch_BAD_ENCODING,
  rx_xml_rematch_ILLEGAL_STR,
  rx_xml_rematch_OUT_OF_MEMORY,
};

enum rx_xml_longest_status
{
  rx_xml_longest_error,
  rx_xml_longest_out_of_input_match,
  rx_xml_longest_out_of_input_nomatch,
  rx_xml_longest_found,
  rx_xml_longest_nomatch,
};


enum rx_xml_prefix_status
{
  rx_xml_prefix_error,
  rx_xml_prefix_out_of_input,
  rx_xml_prefix_found,
  rx_xml_prefix_nomatch,
};



/* automatically generated __STDC__ prototypes */
extern enum rx_xml_recomp_errno rx_xml_recomp (rx_xml_rebuf * re,
					       enum uni_encoding_scheme encoding,
					       uni_string source,
					       size_t length);
extern enum rx_xml_recomp_errno rx_xml_recomp_branch (rx_xml_rebuf * re,
						      enum uni_encoding_scheme encoding,
						      uni_string source,
						      size_t length);
extern enum rx_xml_recomp_errno rx_xml_recomp_opts (rx_xml_rebuf * re,
						    enum uni_encoding_scheme encoding,
						    uni_string src,
						    size_t len,
						    t_ulong syntax_options,
						    bits cset);
extern void rx_xml_free_re (rx_xml_rebuf * re);
extern int rx_xml_is_match (enum rx_xml_rematch_errno * errn,
			    rx_xml_rebuf * re,
			    enum uni_encoding_scheme encoding,
			    uni_string string,
			    size_t length);
extern enum rx_xml_longest_status rx_xml_longest_match (enum rx_xml_rematch_errno * errn,
							size_t * match_len,
							rx_xml_rebuf * re,
							enum uni_encoding_scheme encoding,
							uni_string string,
							size_t length);
extern enum rx_xml_prefix_status rx_xml_prefix_match (enum rx_xml_rematch_errno * errn,
						      rx_xml_rebuf * re,
						      enum uni_encoding_scheme encoding,
						      uni_string string,
						      size_t length);
#endif  /* INCLUDE__RX_UNICODE__RE_H */
