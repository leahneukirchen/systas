/* tag: Tom Lord Tue Dec  4 14:41:34 2001 (bitset-tree-print.h)
 */
/* bitset-tree-print.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__BITSETS__BITSET_TREE_PRINT_H
#define INCLUDE__BITSETS__BITSET_TREE_PRINT_H


#include "hackerlab/bitsets/bitset-tree.h"


/* automatically generated __STDC__ prototypes */
extern void bits_tree_print (int fd,
			     alloc_limits lim,
			     struct bits_tree_rule * rule,
			     bits_tree bt,
			     t_uchar * name,
			     t_uchar * stub,
			     int is_static,
			     int decls_only,
			     int is_nested);
#endif  /* INCLUDE__BITSETS__BITSET_TREE_PRINT_H */
