/* coding.h - coding system conversions
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__CODING_H
#define INCLUDE__UNI__CODING_H



#include "hackerlab/machine/types.h"



#define UNI_ENCODING_SCHEMES \
	UNI_ENCODING (iso8859_1, 1, 256)  /* must be first */ \
	UNI_ENCODING (utf8, 1, (1 << 21)) \
	UNI_ENCODING (utf16, 2, (1 << 21)) \
	UNI_ENCODING (utf16be, 2, (1 << 21)) \
	UNI_ENCODING (utf16le, 2, (1 << 21))

enum uni_encoding_scheme
{
#undef UNI_ENCODING
#define UNI_ENCODING(NAME, WIDTH, CSET_SIZE)	uni_ ## NAME,

  UNI_ENCODING_SCHEMES
  /* uni_utf16 means the byte order of the machine.
   */
};




enum uni_convert_errors
{
  /* These must be less than 0.
   */
  uni_input_truncated = -1,
  uni_input_invalid = -2,
};




typedef t_unicode (*uni_iscan_fn)(t_uchar * str, size_t * pos, size_t len);
typedef int (*uni_iput_fn)(t_uchar * str, size_t * pos, size_t len, t_unicode c);

#if defined(__GNUC__) && defined(UNI_CODING_INLINES)

#undef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS static inline
#include "hackerlab/uni/coding-inlines.c"

#else

#undef UNI_INLINE_QUALIFIERS
#include "hackerlab/uni/coding-inlines.h"

#endif


/* automatically generated __STDC__ prototypes */
extern int uni_utf8_to_utf16be (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len);
extern int uni_utf8_to_utf16le (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len);
extern int uni_utf8_to_utf16 (t_uchar * out,
		   size_t * outp, 
		   size_t out_len,
		   t_uchar * in,
		   size_t * inp,
		   size_t in_len);
extern int uni_utf16be_to_utf8 (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len);
extern int uni_utf16be_to_utf16le (t_uchar * out,
			size_t * outp, 
			size_t out_len,
			t_uchar * in,
			size_t * inp,
			size_t in_len);
extern int uni_utf16be_to_utf16 (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len);
extern int uni_utf16le_to_utf8 (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len);
extern int uni_utf16le_to_utf16be (t_uchar * out,
			size_t * outp, 
			size_t out_len,
			t_uchar * in,
			size_t * inp,
			size_t in_len);
extern int uni_utf16le_to_utf16 (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len);
extern int uni_utf16_to_utf8 (t_uchar * out,
		   size_t * outp, 
		   size_t out_len,
		   t_uchar * in,
		   size_t * inp,
		   size_t in_len);
extern int uni_utf16_to_utf16be (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len);
extern int uni_utf16_to_utf16le (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len);
extern uni_iscan_fn uni_encoding_iscan_fn (enum uni_encoding_scheme encoding);
extern uni_iput_fn uni_encoding_iput_fn (enum uni_encoding_scheme encoding);
#endif  /* INCLUDE__UNI__CODING_H */
