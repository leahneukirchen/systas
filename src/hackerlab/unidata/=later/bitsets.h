/* bitsets.h - decls for unicode category bitsets
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__hackerlab__UNI__CATEGORIES_H
#define INCLUDE__hackerlab__UNI__CATEGORIES_H


/* automatically generated __STDC__ prototypes */
extern bits uni_complete_bitset (void);
extern bits uni_general_category_bitset (enum uni_general_category c);
extern int uni_general_category_lookup_n (t_uchar * name, size_t len);
extern int uni_general_category_lookup (t_uchar * name);
#endif  /* INCLUDE__hackerlab__UNI__CATEGORIES_H */
