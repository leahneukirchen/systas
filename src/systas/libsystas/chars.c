/* chars.c - scheme characters
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <ctype.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "hackerlab/char/char-class.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/symbols.h"


/****************************************************************
 *(h0 "Characters")
 * 
 * Individual character constants are written:
 * 
 * 	#\<character-name>
 * 
 * For example, uppercase "A" is written:
 * 
 * 	#\A
 * 
 * lowercase:
 * 
 * 	#\a
 * 
 * Characters can be written in terms of their ASCII representation by
 * using octal numbers:
 * 
 * 	#\101
 * 	 => #\A
 * 
 * Some special characters are given mnemonic names:
 * 
 * 	#\space
 * 	#\newline
 * 	#\nl
 * 	#\tab
 * 	#\backspace
 * 	#\return
 * 	#\page
 * 	#\null
 * 	#\del
 * 
 * Other special character names (following ASCII) include:
 * 
 * 	nul soh stx etx eot enq ack bel bs ht vt np cr so
 * 	si dle dc1 dc2 dc3 dc4 nak syn etb can em sub esc
 * 	fs gs rs us
 */

/****************************************************************
 *(h1 "Implicit Integer->Character Conversion")
 * 
 * Most functions that except a character argument also accept an
 * immediate integer argument.  The integer `x' is interpreted as the
 * character `(integer->char x)'.
 */


SCM_PROCEDURE (proc_write_char, "write-char");



/* scm_char_names[0..32] correspond to characters 0..32.  Other
 * entries are unordered.  A single character value may occur more
 * than once in this array if the character has more than one symbolic
 * name.
 */
struct scm_char_name scm_char_names[] =
{
  {"nul", '\000'},
  {"soh", '\001'},
  {"stx", '\002'},
  {"etx", '\003'},
  {"eot", '\004'},
  {"enq", '\005'},
  {"ack", '\006'},
  {"bel", '\007'},
  {"bs", '\010'},
  {"ht", '\011'},
  {"nl", '\012'},
  {"vt", '\013'},
  {"np", '\014'},
  {"cr", '\015'},
  {"so", '\016'},
  {"si", '\017'},
  {"dle", '\020'},
  {"dc1", '\021'},
  {"dc2", '\022'},
  {"dc3", '\023'},
  {"dc4", '\024'},
  {"nak", '\025'},
  {"syn", '\026'},
  {"etb", '\027'},
  {"can", '\030'},
  {"em", '\031'},
  {"sub", '\032'},
  {"esc", '\033'},
  {"fs", '\034'},
  {"gs", '\035'},
  {"rs", '\036'},
  {"us", '\037'},
  {"space", ' '},
  {"newline", '\n'},
  {"tab", '\t'},
  {"backspace", '\b'},
  {"return", '\r'},
  {"page", '\f'},
  {"null", '\0'},
  {"del", '\177'}
};

int scm_n_char_names = sizeof (scm_char_names) / sizeof (struct scm_char_name);



/*(c char?)
 * (char? obj)
 * 
 * Return `#t' of `obj' is a character, `#f' otherwise.
 */
SCM_PROC(s_char_p, "char?", 1, 0, 0, scm_char_p);
SCM
scm_char_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  return scm_exact_is_char(x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/************************************************************************
 *(h1 "Case-sensative Character Comparisons")
 */


/*(c char=?)
 * (char=? . args)
 * 
 * Return `#t' if all of the arguments are equal when interpreted as
 * characters.  Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_eq_p, "char=?", scm_tc7_rpsubr, scm_char_eq_p);
SCM
scm_char_eq_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_eq_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_eq_p);
  return (scm_char_to_int(x) == scm_char_to_int(y)) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char<?)
 * (char<? . args)
 * 
 * Return `#t' if each argument is less than the next when interpreted
 * as characters.
 *
 * Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_lt_p, "char<?", scm_tc7_rpsubr, scm_char_lt_p);
SCM
scm_char_lt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_lt_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_lt_p);
  return (scm_char_to_int(x) < scm_char_to_int(y)) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char<=?)
 * (char<=? . args)
 * 
 * Return `#t' if each argument is less than or equal to the next when
 * interpreted as characters.
 *
 * Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_le_p, "char<=?", scm_tc7_rpsubr, scm_char_le_p);
SCM
scm_char_le_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_le_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_le_p);
  return (scm_char_to_int(x) <= scm_char_to_int(y)) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char>?)
 * (char>? . args)
 * 
 * Return `#t' if each argument is greater than the next when
 * interpreted as characters.
 *
 * Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_gt_p, "char>?", scm_tc7_rpsubr, scm_char_gt_p);
SCM
scm_char_gt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_gt_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_gt_p);
  return (scm_char_to_int(x) > scm_char_to_int(y)) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char>=?)
 * (char>=? . args)
 * 
 * Return `#t' if each argument is greater than or equal to the next
 * when interpreted as characters.
 *
 * Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_ge_p, "char>=?", scm_tc7_rpsubr, scm_char_ge_p);
SCM
scm_char_ge_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ge_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ge_p);
  return (scm_char_to_int(x) >= scm_char_to_int(y)) ? SCM_BOOL_T : SCM_BOOL_F;
}


/************************************************************************
 *(h1 "Case-insensative Character Comparisons")
 */

/*(c char-ci=?)
 * (char-ci=? . args)
 * 
 * Return `#t' if each argument is equal to the next when interpreted
 * as characters, disregarding the case of alphabetic characters.
 *
 * Arguments must be characters or integers.
 */
SCM_PROC1 (s_char_ci_eq_p, "char-ci=?", scm_tc7_rpsubr, scm_char_ci_eq_p);
SCM
scm_char_ci_eq_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ci_eq_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ci_eq_p);
  return (toupper(scm_char_to_int(x))==toupper(scm_char_to_int(y))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-ci<?)
 * (char-ci<? . args)
 * 
 * Return `#t' if each argument is less than the next when interpreted
 * as characters, disregarding the case of alphabetic characters.
 *
 * Arguments must be characters or integers.
 * 
 */
SCM_PROC1 (s_char_ci_lt_p, "char-ci<?", scm_tc7_rpsubr, scm_char_ci_lt_p);
SCM
scm_char_ci_lt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ci_lt_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ci_lt_p);
  return (toupper(scm_char_to_int(x)) < toupper(scm_char_to_int(y))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-ci<=?)
 * (char-ci<=? . args)
 * 
 * Return `#t' if each argument is less than or equal to the next when
 * interpreted as characters, disregarding the case of alphabetic
 * characters.
 *
 * Arguments must be characters or integers.
 */
SCM_PROC1 (s_char_ci_le_p, "char-ci<=?", scm_tc7_rpsubr, scm_char_ci_le_p);
SCM
scm_char_ci_le_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ci_le_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ci_le_p);
  return (toupper(scm_char_to_int(x)) <= toupper(scm_char_to_int(y))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-ci>?)
 * (char-ci>? . args)
 * 
 * Return `#t' if each argument is greater than the next when
 * interpreted as characters, disregarding the case of alphabetic
 * characters.
 *
 * Arguments must be characters or integers.
 */
SCM_PROC1 (s_char_ci_gt_p, "char-ci>?", scm_tc7_rpsubr, scm_char_ci_gt_p);
SCM
scm_char_ci_gt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ci_gt_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ci_gt_p);
  return (toupper(scm_char_to_int(x)) > toupper(scm_char_to_int(y))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-ci>=?)
 * (char-ci>=? . args)
 * 
 * Return `#t' if each argument is greater than or equal to the next
 * when interpreted as characters, disregarding the case of alphabetic
 * characters.
 *
 * Arguments must be characters or integers.
 */
SCM_PROC1 (s_char_ci_ge_p, "char-ci>=?", scm_tc7_rpsubr, scm_char_ci_ge_p);
SCM
scm_char_ci_ge_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(x), x, scm_arg1, s_char_ci_ge_p);
  SCM_ASSERT(scm_is_char(y), y, scm_arg2, s_char_ci_ge_p);
  return (toupper(scm_char_to_int(x)) >= toupper(scm_char_to_int(y))) ? SCM_BOOL_T : SCM_BOOL_F;
}

/************************************************************************
 *(h1 "Character Classes")
 */

/*(c char-alpha?)
 * (char-alpha? character) 
 * (char-alphabetic? character) ; aka
 * 
 * Return `#t' if `character' is an alphabetic charater, `#f'
 * otherwise.  The alphabetic charaters are "a-z" and "A-Z".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_alphabetic_p, "char-alphabetic?", 1, 0, 0, scm_char_alpha_p);
SCM_PROC(s_char_alpha_p, "char-alpha?", 1, 0, 0, scm_char_alpha_p);
SCM
scm_char_alpha_p(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_alpha_p);
  return (char_is_ascii(scm_char_to_int(chr)) && char_is_alpha(scm_char_to_int(chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-digit?)
 * (char-digit? character) 
 * (char-numeric? character) ; aka
 * 
 * Return `#t' if `character' is a decimal digit, `#f' otherwise.  The
 * decimal digits are "0-9".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_numeric_p, "char-numeric?", 1, 0, 0, scm_char_digit_p);
SCM_PROC(s_char_digit_p, "char-digit?", 1, 0, 0, scm_char_digit_p);
SCM
scm_char_digit_p(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_digit_p);
  return (char_is_ascii(scm_char_to_int(chr)) && char_is_digit(scm_char_to_int(chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-space?)
 * (char-space? character) 
 * (char-whitespace? character) ; aka
 * 
 * Return `#t' if `character' is a whitespace character, `#f'
 * otherwise.  The whitespace characters are:
 *
 *	#\ht #\nl #\vt #\np #\cr #\space
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_whitespace_p, "char-whitespace?", 1, 0, 0, scm_char_space_p);
SCM_PROC(s_char_space_p, "char-space?", 1, 0, 0, scm_char_space_p);
SCM
scm_char_space_p(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_space_p);
  return (char_is_ascii(scm_char_to_int(chr)) && char_is_space(scm_char_to_int(chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-upper?)
 * (char-upper? character) 
 * (char-upper-case? character) ; aka
 * 
 * Return `#t' if `character' is an uppercase character, `#f'
 * otherwise.  The upper case characters are "A-Z".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_upper_case_p, "char-upper-case?", 1, 0, 0, scm_char_upper_p);
SCM_PROC(s_char_upper_p, "char-upper?", 1, 0, 0, scm_char_upper_p);
SCM
scm_char_upper_p(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_upper_p);
  return (char_is_ascii(scm_char_to_int(chr)) && char_is_upper(scm_char_to_int(chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-lower?)
 * (char-lower? character) 
 * (char-lower-case? character) ; aka
 * 
 * Return `#t' if `character' is a lowercase character, `#f'
 * otherwise.  The upper case characters are "a-z".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_lower_case_p, "char-lower-case?", 1, 0, 0, scm_char_lower_p);
SCM_PROC(s_char_lower_p, "char-lower?", 1, 0, 0, scm_char_lower_p);
SCM
scm_char_lower_p(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_lower_p);
  return (char_is_ascii(scm_char_to_int(chr)) && char_is_lower(scm_char_to_int(chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-alphanumeric?)
 * (char-alphanumeric? character) 
 * 
 * Return `#t' if `character' is an alphanumeric character, `#f'
 * otherwise.  The alphanumeric characters are "a-z", "A-Z", and
 * "0-9".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_alphanumeric_p, "char-alphanumeric?", 1, 0, 0, scm_char_alphanumeric_p);
SCM
scm_char_alphanumeric_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_alphanumeric_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_alphanumeric (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(c char-graph?)
 * (char-graph? character) 
 * (char-graphic? character) ; aka
 * 
 * Return `#t' if `character' is a graphic character, `#f' otherwise.
 * The graphic characters are all characters which are not control
 * characters, whitespace characters, or delete.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_graphic_p, "char-graphic?", 1, 0, 0, scm_char_graph_p);
SCM_PROC (s_char_graph_p, "char-graph?", 1, 0, 0, scm_char_graph_p);
SCM
scm_char_graph_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_graph_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_graph (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(c char-printable?)
 * (char-printable? character) 
 * (char-printing? character) ; aka
 * 
 * Return `#t' if `character' is a printable character, `#f'
 * otherwise.  The printable characters are all characters which are
 * either space characters, or are not control characters, or delete.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_printing_p, "char-printing?", 1, 0, 0, scm_char_printable_p);
SCM_PROC (s_char_printable_p, "char-printable?", 1, 0, 0, scm_char_printable_p);
SCM
scm_char_printable_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_printable_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_printable (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(c char-blank?)
 * (char-blank? character) 
 * 
 * Return `#t' if `character' is a blankspace character, `#f'
 * otherwise.  The blankspace characters are space and tab.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_blank_p, "char-blank?", 1, 0, 0, scm_char_blank_p);
SCM
scm_char_blank_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_blank_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_blank (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-control?)
 * (char-control? character) 
 * 
 * Return `#t' if `character' is a control character, `#f' otherwise.
 * The control characters are character codes 0..31 (inclusive).
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_control_p, "char-control?", 1, 0, 0, scm_char_control_p);
SCM
scm_char_control_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_control_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_control (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-punct?)
 * (char-punct? character) 
 * (char-punctuation? character) ; aka
 * 
 * Return `#t' if `character' is a punctuation character, `#f'
 * otherwise.  The punctuation characters are all graphic characters
 * which are not alphanumeric and not whitespace.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_punctuation_p, "char-punctuation?", 1, 0, 0, scm_char_punct_p);
SCM_PROC (s_char_punct_p, "char-punct?", 1, 0, 0, scm_char_punct_p);
SCM
scm_char_punct_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_punct_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_punct (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(c char-xdigit?)
 * (char-xdigit? character) 
 * (char-hex-digit? character) ; aka
 * 
 * Return `#t' if `character' is a hexidecimal digit character, `#f'
 * otherwise.  The hexidecimal digits are "0..9", "a-f", and "A-F".
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_hex_digit_p, "char-hex-digit?", 1, 0, 0, scm_char_xdigit_p);
SCM_PROC (s_char_xdigit_p, "char-xdigit?", 1, 0, 0, scm_char_xdigit_p);
SCM
scm_char_xdigit_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_xdigit_p);
  return (char_is_ascii (scm_char_to_int (chr)) && char_is_xdigit (scm_char_to_int (chr))) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c char-ascii?)
 * (char-ascii? character) 
 * 
 * Return `#t' if `character' is a valid ascii character, `#f'
 * otherwise.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC (s_char_ascii_p, "char-ascii?", 1, 0, 0, scm_char_ascii_p);
SCM
scm_char_ascii_p (SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_char_ascii_p);
  return char_is_ascii (scm_char_to_int (chr)) ? SCM_BOOL_T : SCM_BOOL_F;
}

/************************************************************************
 *(h1 "Character Case Conversions")
 */


/*(c char-upcase)
 * (char-upcase character) 
 * 
 * If `character' is a lowercase letter, return the corresponding
 * uppercase letter.  Otherwise, return `character'.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_upcase, "char-upcase", 1, 0, 0, scm_char_upcase);
SCM
scm_char_upcase(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_upcase);
  return scm_int_to_char(toupper(scm_char_to_int(chr)));
}


/*(c char-downcase)
 * (char-downcase character) 
 * 
 * If `character' is an uppercase letter, return the corresponding
 * lowercase letter.  Otherwise, return `character'.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_downcase, "char-downcase", 1, 0, 0, scm_char_downcase);
SCM
scm_char_downcase(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_downcase);
  return scm_int_to_char(tolower(scm_char_to_int(chr)));
}

/************************************************************************
 *(h1 "Character <-> Integer Conversions")
 * 
 * 
 * 
 */

/*(c char->integer)
 * (char->integer character) 
 * 
 * Return a number which is the integer character code of `character'.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_char_to_integer, "char->integer", 1, 0, 0, scm_char_to_integer);
SCM
scm_char_to_integer(SCM chr)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(scm_is_char(chr), chr, scm_arg1, s_char_to_integer);
  return scm_ulong2num((unsigned long)scm_char_to_int(chr));
}


/*(c integer->char)
 * (integer->char integer) 
 * 
 * Return the character whose integer character code is `integer'.
 * 
 * `integer' is interpreted as a 32 bit unsigned integer and masked to
 * 24 bits.
 * 
 * `character' must be a character or integer.
 */
SCM_PROC(s_integer_to_char, "integer->char", 1, 0, 0, scm_integer_to_char);
SCM
scm_integer_to_char(SCM n)
{
  SCM_INTS_ENABLED;

  int ni;

  ni = 0xffffff & scm_num2ulong (n, scm_arg1, s_integer_to_char);
  return scm_int_to_char(ni);
}

/************************************************************************
 *(h1 "Printing Characters")
 */

/*(c write-char)
 * (write-char char :optional port)
 * 
 * Write the character `char' to the given port and return an
 * unspecified value.  This procedure writes the character itself, not
 * an external representation of the character, for example:
 *
 *	a
 * not
 *	#\a
 * 
 *
 * `port' defaults to `(current-output-port)'
 * 
 * If an I/O error occurs, an exception is thrown.
 */
SCM_PROC(s_write_char, "write-char", 1, 1, 0, scm_write_char);
SCM 
scm_write_char (SCM chr, SCM port)
{
  SCM_INTS_ENABLED;
  int errn;
  int status;

  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_is_port (port), port, scm_arg2, s_write_char);
  SCM_ASSERT (scm_is_char (chr), chr, scm_arg1, s_write_char);
  SCM_DEFER_INTS;
  status = scm_port_putc (&errn, port, (int) scm_char_to_int (chr));
  SCM_ALLOW_INTS;
  if (status < 0)
    scm_throw (scm_makerrno (errn), scm_listify (proc_write_char, chr, port, SCM_UNDEFINED));
  return SCM_UNSPECIFIED;
}



/************************************************************************
 *h1 "The C Interface to Characters"
 * 
 */


/*c scm_exact_is_char)
 * int scm_exact_is_char (SCM x);
 * 
 * Return 1 if `x' is a character, 0 otherwise.  Use this procedure if
 * you must distinguish between characters and immediate integers.
 * Otherwise, consider using `scm_is_char'.
 */
int
scm_exact_is_char (SCM x)
{
  return (SCM_ITAG8(x) == scm_tc8_char);
}


/*c scm_is_char
 * int scm_is_char (SCM x);
 * 
 * Return 1 if `x' is a character or immediate integera, 0 otherwise.
 * Use this procedure if you don't need to distinguish between
 * characters and immediate integers.  Otherwise, consider using
 * `scm_exact_is_char'. 
 */
int
scm_is_char (SCM x)
{
  return (scm_exact_is_char(x) || SCM_INUMP (x));
}


/*c scm_char_to_int
 * int scm_char_to_int (SCM x);
 * 
 * Return the integer character code of character `x'.
 * 
 * `x' may be a character or immediate integer.
 * 
 */
int
scm_char_to_int (SCM x)
{
  return (scm_exact_is_char(x)
	  ? ((unsigned int)SCM_ITAG8_DATA(x)) 
	  : (unsigned int)(t_uchar)SCM_INUM (x));
}


/*c scm_int_to_char
 * SCM scm_int_to_char (int x);
 * 
 * Return the character (Scheme object) whose character code is `x'.
 */
SCM 
scm_int_to_char (int x)
{
  return SCM_MAKE_ITAG8 (x, scm_tc8_char);
}



void
scm_init_chars (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/chars.x"
}



/************************************************************************
 *h1 "Character Internals"
 *
 * Characters are represented internally as an immediate object with
 * an 8-bit tag:
 *
 *		.........character..scm_tc8_char
 * 
 * Thus, on a 32-bit machine, a character is represented by an
 * unsigned 24-bit integer.
 *
 * The function `scm_exact_is_char' returns non-0 only for characters.
 * The function `scm_is_char' returns non-0 for both characters and
 * immediate integers.  Most functions should use `scm_is_char'.
 *
 * The function `scm_char_to_int' returns the integer value of a
 * character.  It automatically treats immediate integers as
 * characters.
 */

/****************************************************************
 *(h1 "Rationale -- Characters")
 * 
 * The eventual plan is to support Unicode, not just ASCII.
 * 
 * Mostly, Systas simply follows standard Scheme.
 * 
 * Accepting integers as characters is convenient and aids in
 * exchanging data with Emacs lisp.
 * 
 * For convenience, several procedures are given both the standard
 * scheme name and a name based on standard C libraries
 * (e.g. `char-upper-case?' and `char-upper?').
 * 
 * For convenience, several characters are given both the standard
 * scheme read name and a name based on ASCII (e.g. `#\newline' and
 * `#\nl').
 */
