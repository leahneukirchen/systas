/* tag: Tom Lord Tue Dec  4 14:41:43 2001 (charsets.h)
 */
/* xml-charsets.h - - decls for XML's standard character sets
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__XML__CHARSETS_H
#define INCLUDE__XML__CHARSETS_H


#include "hackerlab/bitsets/bits.h"
#include "hackerlab/bitsets/uni-bits.h"



/************************************************************************
 *(h1 "XML Character Sets"
 *    :includes ("hackerlab/xml/charsets.h"))
 * 
 * This section presents a collection of efficiently represented
 * sets of Unicode characters.  Sets are represented as shared 
 * bitset trees; see xref:"Shared Bitset Trees".  The tree structure
 * of these sets is defined by `uni_bits_tree_rule'; see xref:"Unicode
 * Character Bitsets".
 * 
 */



/*(c xml_charset :category variable)
 * 
 * The set of all Unicode characters which are valid
 * in an XML document.  This set is defined by the
 * production for `Char' in *XML 1.0*.
 */

/*(c xml_is_char :category macro)
 * #define xml_is_char(UNICODE)
 * 
 * Return 1 if `UNICODE' (a Unicode code point) is a valid
 * character in XML.  A code point is a valid character
 * if it matches the production for `Char' in *XML 1.0*, or
 * equivalently, if it is a member of the bitset `xml_charset'.
 */

/*(c xml_base_charset :category variable)
 * 
 * The set of all Unicode characters that match
 * the production for `BaseChar' in *XML 1.0*.
 */

/*(c xml_combining_charset :category variable)
 * 
 * The set of all Unicode characters that match
 * the production for `CombiningChar' in *XML 1.0*.
 */

/*(c xml_digit_charset :category variable)
 * 
 * The set of all Unicode characters that match
 * the production for `Digit' in *XML 1.0*.
 */

/*(c xml_extender_charset :category variable)
 * 
 * The set of all Unicode characters that match
 * the production for `Extender' in *XML 1.0*.
 */

/*(c xml_ideographic_charset :category variable)
 * 
 * The set of all Unicode characters that match
 * the production for `Ideographic' in *XML 1.0*.
 */

extern bits xml_charset;
extern bits xml_base_charset;
extern bits xml_combining_charset;
extern bits xml_digit_charset;
extern bits xml_extender_charset;
extern bits xml_ideographic_charset;


extern bits xml_re_dot_charset;

extern bits xml_re_meta_charset;

#define xml_is_char(X)				bits_is_member(xml_charset, (X))
#define xml_is_re_meta_char(X)			bits_is_member(xml_re_meta_charset, (X))


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__XML__CHARSETS_H */
