/* tag: Tom Lord Tue Dec  4 14:41:41 2001 (uni-str-inlines.h)
 */
/* uni-str-inlines.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNICODE__UNI_STR_INLINES_H
#define INCLUDE__UNICODE__UNI_STR_INLINES_H


#undef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS



/* automatically generated __STDC__ prototypes */
extern UNI_INLINE_QUALIFIERS uni_string  __attribute__((unused)) uni_str_offset (enum uni_encoding_scheme encoding,
		uni_string string,
		ssize_t x);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_str_iscan (enum uni_encoding_scheme encoding,
	       uni_string str,
	       size_t * pos,
	       size_t len);
extern UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused)) uni_str_scan (enum uni_encoding_scheme encoding,
	      uni_string * str);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_str_iput (enum uni_encoding_scheme encoding,
	      uni_string str,
	      size_t * pos,
	      size_t len,
	      t_unicode c);
extern UNI_INLINE_QUALIFIERS int __attribute__((unused)) uni_str_put (enum uni_encoding_scheme encoding,
	     uni_string * str,
	     t_unicode c);
#endif  /* INCLUDE__UNICODE__UNI_STR_INLINES_H */
