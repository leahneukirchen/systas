/* str.h - decls for string functions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__CHAR__STR_H
#define INCLUDE__CHAR__STR_H



#include "hackerlab/machine/types.h"
#include "hackerlab/mem/alloc-limits.h"


/* automatically generated __STDC__ prototypes */
extern size_t str_length (const t_uchar * x);
extern size_t str_length_n (const t_uchar * x, size_t n);
extern unsigned long str_hash_n (const t_uchar * chr, size_t len);
extern t_uchar * str_chr_index (const t_uchar * s, int c);
extern t_uchar * str_chr_rindex (const t_uchar * s, int c);
extern t_uchar * str_chr_index_n (const t_uchar * s, size_t n, int c);
extern t_uchar * str_chr_rindex_n (const t_uchar * s, size_t n, int c);
extern int str_cmp (const t_uchar * a, const t_uchar * b);
extern int str_cmp_n (const t_uchar * a, size_t a_l, const t_uchar * b, size_t b_l);
extern int str_cmp_prefix (const t_uchar * prefix, const t_uchar * s);
extern int str_casecmp (const t_uchar * a, const t_uchar * b);
extern int str_casecmp_n (const t_uchar * a, size_t a_l,
			  const t_uchar * b, size_t b_l);
extern int str_casecmp_prefix (const t_uchar * prefix, const t_uchar * s);
extern t_uchar * str_cpy (t_uchar * to, const t_uchar * from);
extern t_uchar * str_cpy_n (t_uchar * to,
			    const t_uchar * from,
			    size_t n);
extern t_uchar * str_cat (t_uchar * to, const t_uchar * from);
extern t_uchar * str_cat_n (t_uchar * to,
			    const t_uchar * from,
			    size_t n);
extern t_uchar * str_save (alloc_limits limits, const t_uchar * str);
extern t_uchar * str_save_n (alloc_limits limits,
			     const t_uchar * str,
			     size_t len);
extern t_uchar * str_alloc_cat (alloc_limits limits,
				const t_uchar * str1,
				const t_uchar * str2);
extern t_uchar * str_alloc_cat_n (alloc_limits limits,
				  const t_uchar * str1,
				  const t_uchar * str2,
				  size_t n);
extern t_uchar * str_realloc_cat (alloc_limits limits,
				  t_uchar * str1,
				  const t_uchar * str2);
extern t_uchar * str_realloc_cat_n (alloc_limits limits,
				    t_uchar * str1,
				    const t_uchar * str2,
				    size_t n);
#endif  /* INCLUDE__CHAR__STR_H */
