/* tag: Tom Lord Tue Dec  4 14:41:46 2001 (db-inlines.c)
 */
/* db-inlines.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/uni/invariant.h"
#include "hackerlab/uni/unidata.h"
#include "hackerlab/unidata/bitsets.h"
#include "hackerlab/unidata/db.h"
#include "hackerlab/unidata/db-macros.h"



#ifndef UNIDATA_INLINE_QUALIFIERS
#define UNIDATA_INLINE_QUALIFIERS
#endif

#ifndef __GNUC__
#undef __attribute__
#define __attribute__(X)
#endif


/************************************************************************
 *(h1 "Unicode Character Properties"
 *    :include ("hackerlab/unicode/unidata.h"))
 * 
 * The functions and macros in this chapter present programs with an
 * interface to various properties extracted from the Unicode
 * Character Database as published by the Unicode consortium.
 * 
 * For information about the version of the database used and the
 * implications of using these functions on program size, see
 * xref:"Data Sheet for the Hackerlab Unicode Database".
 * 
 */

/*(menu)
 */


/************************************************************************
 *(h2 "Assigned Code Points")
 * 
 * 
 * 
 */


/*(c unidata_is_assigned_code_point)
 * int unidata_is_assigned_code_point (t_unicode c);
 * 
 * Return 1 if `c' is an assigned code point, 0 otherwise.
 * 
 * A code point is *assigned* if it has an entry in `unidata.txt'
 * or is part of a range of characters whose end-points are
 * defined in `unidata.txt'.
 */
UNIDATA_INLINE_QUALIFIERS int __attribute__((unused))
unidata_is_assigned_code_point (t_unicode c) 
{
  t_uint16 db;

  db = unidata__db_ref (unidata__db, c);

  return !!unidata__db_is_assigned_code_point (db);
}

/************************************************************************
 *(h2 "General Category")
 * 
 * 
 * 
 */

/*(c #s"enum unidata_general_category" :category type)
 * enum uni_general_category;
 * 
 * The General Category of a Unicode character is represented by
 * an enumerated value of this type.
 * 
 * The primary category values are:
 * 
 *	uni_general_category_Lu		Letter, uppercase
 *	uni_general_category_Ll		Letter, lowercase
 *	uni_general_category_Lt		Letter, titlecase
 *	uni_general_category_Lm		Letter, modifier
 *	uni_general_category_Lo		Letter, other"
 * 
 *	uni_general_category_Mn		Mark, nonspacing
 *	uni_general_category_Mc		Mark, spacing combining
 *	uni_general_category_Me		Mark, enclosing
 * 
 *	uni_general_category_Nd		Number, decimal digit
 *	uni_general_category_Nl		Number, letter
 *	uni_general_category_No		Number, other
 * 
 *	uni_general_category_Zs		Separator, space
 *	uni_general_category_Zl		Separator, line
 *	uni_general_category_Zp		Separator, paragraph
 * 
 *	uni_general_category_Cc		Other, control
 *	uni_general_category_Cf		Other, format
 *	uni_general_category_Cs		Other, surrogate
 *	uni_general_category_Co		Other, private use
 *	uni_general_category_Cn		Other, not assigned
 * 
 *	uni_general_category_Pc		Punctuation, connector
 *	uni_general_category_Pd		Punctuation, dash
 *	uni_general_category_Ps		Punctuation, open
 *	uni_general_category_Pe		Punctuation, close
 *	uni_general_category_Pi		Punctuation, initial quote
 *	uni_general_category_Pf		Punctuation, final quote
 *	uni_general_category_Po		Punctuation, other
 * 
 *	uni_general_category_Sm		Symbol, math
 *	uni_general_category_Sc		Symbol, currency
 *	uni_general_category_Sk		Symbol, modifier
 *	uni_general_category_So		Symbol, other
 * 
 * Seven additional synthetic categories are defined.  These are:
 * 
 * 	uni_general_category_L		Letter
 * 	uni_general_category_M		Mark
 * 	uni_general_category_N		Number
 * 	uni_general_category_Z		Separator
 * 	uni_general_category_C		Other
 * 	uni_general_category_P		Punctuation
 * 	uni_general_category_S		Symbol
 * 
 * No character is given a synthetic category as its general category.
 * Rather, the synthetic categories are used in some interfaces to
 * refer to all characters having a general category within one of
 * the synthetic categories.
 */


/*(c unidata_general_category)
 * enum uni_general_category unidata_general_category (t_unicode c);
 * 
 * Return the general category of `c'.
 * 
 * The category returned for unassigned code points is
 * `uni_general_category_Cn' (Other, Not Assigned).
 */
UNIDATA_INLINE_QUALIFIERS enum uni_general_category __attribute__((unused))
unidata_general_category (t_unicode c)
{
  t_uint16 db;

  db = unidata__db_ref (unidata__db, c);
  return(enum uni_general_category)unidata__db_general_category (db);
}


/************************************************************************
 *(h2 "Unicode Decimal Digit Values")
 * 
 * 
 * 
 */


/*(c unidata_decimal_digit_value)
 * int unidata_decimal_digit_value (t_unicode c);
 * 
 * If `c' is a decimal digit (regardless of script) return
 * its digit value.  Otherwise, return -1.
 */
UNIDATA_INLINE_QUALIFIERS int __attribute__((unused))
unidata_decimal_digit_value (t_unicode c)
{
  t_uint16 db;
  int val;

  db = unidata__db_ref (unidata__db, c);
  val = unidata__db_decimal_digit_value (db);
  return (val == 10) ? -1 : val;
}


/************************************************************************
 *(h2 "Unicode Bidirectional Properties")
 * 
 * 
 * 
 */

/*(c #s"enum unidata_bidi_category" :category type)
 * enum uni_bidi_category;
 * 
 * The Bidrectional Category of a Unicode character is represented by
 * an enumerated value of this type.
 * 
 * The bidi category values are:
 * 
 * 	uni_bidi_L	Left-to-Right
 *	uni_bidi_LRE	Left-to-Right Embedding
 *	uni_bidi_LRO	Left-to-Right Override
 *	uni_bidi_R	Right-to-Left
 *	uni_bidi_AL	Right-to-Left Arabic
 *	uni_bidi_RLE	Right-to-Left Embedding
 *	uni_bidi_RLO	Right-to-Left Override
 *	uni_bidi_PDF	Pop Directional Format
 *	uni_bidi_EN	European Number
 *	uni_bidi_ES 	European Number Separator
 *	uni_bidi_ET	European Number Terminator
 *	uni_bidi_AN	Arabic Number
 *	uni_bidi_CS	Common Number Separator
 *	uni_bidi_NSM	Non-Spacing Mark
 *	uni_bidi_BN	Boundary Neutral
 *	uni_bidi_B	Paragraph Separator
 *	uni_bidi_S	Segment Separator
 *	uni_bidi_WS	Whitspace
 *	uni_bidi_ON	Other Neutrals
 * 
 */

/*(c unidata_bidi_category)
 * enum uni_bidi_category unidata_bidi_category (t_unicode c);
 * 
 * Return the bidirectional category of `c'.
 * 
 * The category returned for unassigned code points is 
 * `uni_bidi_ON' (other neutrals).
 */
UNIDATA_INLINE_QUALIFIERS enum uni_bidi_category __attribute__((unused))
unidata_bidi_category (t_unicode c)
{
  t_uint16 db;

  db = unidata__db_ref (unidata__db, c);
  return (enum uni_bidi_category)unidata__db_bidi_category (db);
}


/*(c unidata_is_mirrored)
 * int unidata_is_mirrored (t_unicode c);
 * 
 * Return 1 if `c' is mirrored in bidirectional text, 0 
 * otherwise.
 */
UNIDATA_INLINE_QUALIFIERS int __attribute__((unused))
unidata_is_mirrored (t_unicode c)
{
  t_uint16 db;

  db = unidata__db_ref (unidata__db, c);
  return !!unidata__db_is_mirrored (db);
}


/*(include-documentation "combine-db-macros.h")
 */
/*(include-documentation "case-db-inlines.c")
 */
/*(include-documentation "decomp-db-macros.h")
 */
/*(include-documentation "blocks.h")
 */
/*(include-documentation "bitset-lookup.c")
 */

