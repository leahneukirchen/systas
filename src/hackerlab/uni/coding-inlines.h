/* coding-inlines.h - declarations for inline encoding/decoding functions
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__CODING_INLINES_H
#define INCLUDE__UNI__CODING_INLINES_H


#undef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS


#ifndef __GNUC__
#undef __attribute__
#define __attribute__(X)
#endif


/* automatically generated __STDC__ prototypes */
extern UNI_INLINE_QUALIFIERS size_t __attribute__((unused)) uni_cset_size (enum uni_encoding_scheme encoding);
extern UNI_INLINE_QUALIFIERS size_t __attribute__((unused)) uni_code_unit_size (enum uni_encoding_scheme encoding);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_is_noncharacter (t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_is_high_surrogate (t_unicode c);
extern UNI_INLINE_QUALIFIERS int  __attribute__((unused)) uni_is_low_surrogate (t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_is_surrogate (t_unicode c);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_assemble_surrogates (t_unichar hi, t_unichar lo);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_length_in_iso8859_1 (t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_length_in_utf8 (t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_length_in_utf16 (t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_length_in_encoding (enum uni_encoding_scheme enc, t_unicode c);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_iso8859_1_iscan (t_uchar * str, size_t * pos, size_t len);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_utf8_iscan (t_uchar * str, size_t * pos, size_t len);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_utf16be_iscan (t_uchar * str, size_t * pos, size_t len);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_utf16le_iscan (t_uchar * str, size_t * pos, size_t len);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_utf16_iscan (t_uchar * str, size_t * pos, size_t len);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_iso8859_1_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_utf8_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_utf16be_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_utf16le_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_utf16_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c);
#endif  /* INCLUDE__UNI__CODING_INLINES_H */
