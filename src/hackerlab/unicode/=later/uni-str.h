/* tag: Tom Lord Tue Dec  4 14:41:42 2001 (uni-str.h)
 */
/* uni-str.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__UNI_STR_H
#define INCLUDE__UNI__UNI_STR_H


#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/uni/coding.h"



#if defined(__GNUC__) && defined(UNI_STR_INLINES)

#undef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS static inline
#include "hackerlab/unicode/uni-str-inlines.c"

#else

#undef UNI_INLINE_QUALIFIERS
#include "hackerlab/unicode/uni-str-inlines.h"

#endif



/* automatically generated __STDC__ prototypes */
extern size_t uni_str_length (enum uni_encoding_scheme encoding,
			      uni_string s);
extern size_t uni_str_length_for_encoding (enum uni_encoding_scheme out_enc,
					   enum uni_encoding_scheme encoding,
					   uni_string str);
extern size_t uni_str_length_for_encoding_n (enum uni_encoding_scheme out_enc,
					     enum uni_encoding_scheme encoding,
					     uni_string str,
					     size_t len);
extern uni_string uni_str_cpy (enum uni_encoding_scheme encoding,
			       uni_string to,
			       uni_string from);
extern uni_string uni_str_cpy_n (enum uni_encoding_scheme encoding,
				 uni_string to,
				 uni_string from,
				 size_t n);
extern uni_string uni_str_cpy_xfrm (enum uni_encoding_scheme to_enc,
				    uni_string to,
				    enum uni_encoding_scheme from_enc,
				    uni_string from);
extern uni_string uni_str_cpy_xfrm_n (enum uni_encoding_scheme to_enc,
				      uni_string to,
				      enum uni_encoding_scheme from_enc,
				      uni_string from,
				      size_t n);
extern uni_string uni_str_cat (enum uni_encoding_scheme encoding,
			       uni_string to,
			       uni_string from);
extern uni_string uni_str_cat_n (enum uni_encoding_scheme encoding,
				 uni_string to,
				 uni_string from,
				 size_t n);
extern uni_string uni_str_cat_xfrm (enum uni_encoding_scheme to_enc,
				    uni_string to,
				    enum uni_encoding_scheme from_enc,
				    uni_string from);
extern uni_string uni_str_cat_xfrm_n (enum uni_encoding_scheme to_enc,
				      uni_string to,
				      enum uni_encoding_scheme from_enc,
				      uni_string from,
				      size_t n);
extern unsigned long uni_str_hash_n (enum uni_encoding_scheme encoding,
				     uni_string s,
				     size_t len);
extern unsigned long uni_str_hash_codepoints_n (enum uni_encoding_scheme encoding,
						uni_string s,
						size_t len);
extern uni_string uni_str_chr_index (enum uni_encoding_scheme encoding,
				     uni_string s,
				     t_unicode c);
extern uni_string uni_str_chr_rindex (enum uni_encoding_scheme encoding,
				      uni_string s,
				      t_unicode c);
extern uni_string uni_str_chr_index_n (enum uni_encoding_scheme encoding,
				       uni_string s,
				       size_t n,
				       t_unicode c);
extern uni_string uni_str_chr_rindex_n (enum uni_encoding_scheme encoding,
					uni_string s,
					size_t n,
					t_unicode c);
extern int uni_str_cmp (enum uni_encoding_scheme encoding,
			uni_string a,
			uni_string b);
extern int uni_str_cmp_n (enum uni_encoding_scheme encoding,
			  uni_string a, size_t a_l,
			  uni_string b, size_t b_l);
extern int uni_str_cmp_prefix (enum uni_encoding_scheme encoding,
			       uni_string a,
			       uni_string b);
extern int uni_str_casecmp (enum uni_encoding_scheme encoding,
			    uni_string a,
			    uni_string b);
extern int uni_str_casecmp_n (enum uni_encoding_scheme encoding,
			      uni_string a, size_t a_l,
			      uni_string b, size_t b_l);
extern int uni_str_casecmp_prefix (enum uni_encoding_scheme encoding,
				   uni_string a,
				   uni_string b);
extern int uni_str_cmp_xfrm (enum uni_encoding_scheme a_enc,
			     uni_string a,
			     enum uni_encoding_scheme b_enc,
			     uni_string b);
extern int uni_str_cmp_xfrm_n (enum uni_encoding_scheme a_enc,
			       uni_string a, size_t a_l,
			       enum uni_encoding_scheme b_enc,
			       uni_string b, size_t b_l);
extern int uni_str_cmp_prefix_xfrm (enum uni_encoding_scheme a_enc,
				    uni_string a,
				    enum uni_encoding_scheme b_enc,
				    uni_string b);
extern int uni_str_casecmp_xfrm (enum uni_encoding_scheme a_enc,
				 uni_string a,
				 enum uni_encoding_scheme b_enc,
				 uni_string b);
extern int uni_str_casecmp_xfrm_n (enum uni_encoding_scheme a_enc,
				   uni_string a, size_t a_l,
				   enum uni_encoding_scheme b_enc,
				   uni_string b, size_t b_l);
extern int uni_str_casecmp_prefix_xfrm (enum uni_encoding_scheme a_enc,
					uni_string a,
					enum uni_encoding_scheme b_enc,
					uni_string b);
extern uni_string uni_str_save_xfrm (alloc_limits limits,
				     enum uni_encoding_scheme out_enc,
				     enum uni_encoding_scheme str_enc,
				     uni_string str);
extern uni_string uni_str_save_xfrm_n (alloc_limits limits,
				       enum uni_encoding_scheme out_enc,
				       enum uni_encoding_scheme str_enc,
				       uni_string str,
				       size_t len);
extern uni_string uni_str_alloc_cat_xfrm (alloc_limits limits,
					  enum uni_encoding_scheme out_enc,
					  enum uni_encoding_scheme str1_enc,
					  uni_string str1,
					  enum uni_encoding_scheme str2_enc,
					  uni_string str2);
#endif  /* INCLUDE__UNI__UNI_STR_H */
