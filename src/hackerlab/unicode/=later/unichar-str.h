/* tag: Tom Lord Tue Dec  4 14:41:42 2001 (unichar-str.h)
 */
/* unichar-str.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNICODE__UNICHAR_STR_H
#define INCLUDE__UNICODE__UNICHAR_STR_H


#include "hackerlab/machine/types.h"
#include "hackerlab/mem/alloc-limits.h"


/* automatically generated __STDC__ prototypes */
extern size_t unichar_str_length (const t_unichar * x);
extern size_t unichar_str_length_n (const t_unichar * x, size_t n);
extern unsigned long unichar_str_hash_n (const t_unichar * chr, size_t len);
extern t_unichar * unichar_str_chr_index (const t_unichar * s, int c);
extern t_unichar * unichar_str_chr_rindex (const t_unichar * s, int c);
extern t_unichar * unichar_str_chr_index_n (const t_unichar * s, size_t n, int c);
extern t_unichar * unichar_str_chr_rindex_n (const t_unichar * s, size_t n, int c);
extern int unichar_str_cmp (const t_unichar * a, const t_unichar * b);
extern int unichar_str_cmp_n (const t_unichar * a, size_t a_l, const t_unichar * b, size_t b_l);
extern int unichar_str_cmp_prefix (const t_unichar * prefix, const t_unichar * s);
extern int unichar_str_casecmp (const t_unichar * a, const t_unichar * b);
extern int unichar_str_casecmp_n (const t_unichar * a, size_t a_l,
				  const t_unichar * b, size_t b_l);
extern int unichar_str_casecmp_prefix (const t_unichar * prefix, const t_unichar * s);
extern t_unichar * unichar_str_cpy (t_unichar * to, const t_unichar * from);
extern t_unichar * unichar_str_cpy_n (t_unichar * to,
				      const t_unichar * from,
				      size_t n);
extern t_unichar * unichar_str_cat (t_unichar * to, const t_unichar * from);
extern t_unichar * unichar_str_cat_n (t_unichar * to,
				      const t_unichar * from,
				      size_t n);
extern t_unichar * unichar_str_save (alloc_limits limits, const t_unichar * str);
extern t_unichar * unichar_str_save_n (alloc_limits limits,
				       const t_unichar * str,
				       size_t len);
extern t_unichar * unichar_str_alloc_cat (alloc_limits limits,
					  const t_unichar * str1,
					  const t_unichar * str2);
extern t_unichar * unichar_str_alloc_cat_n (alloc_limits limits,
					    const t_unichar * str1,
					    const t_unichar * str2,
					    size_t n);;
#endif  /* INCLUDE__UNICODE__UNICHAR_STR_H */
