/* tag: Tom Lord Tue Dec  4 14:41:51 2001 (pow2-array-print.h)
 */
/* pow2-array-print.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__ARRAYS__POW2_ARRAY_PRINT_H
#define INCLUDE__ARRAYS__POW2_ARRAY_PRINT_H


#include "hackerlab/arrays/pow2-array.h"


/* automatically generated __STDC__ prototypes */
extern void pow2_array_print (int fd,
			      struct pow2_array * array,
			      t_uchar * name,
			      t_uchar * stub,
			      int decls_only,
			      t_uchar * ref_macro_name,
			      int is_static,
			      t_uchar * elt_type,
			      void (*print_elt_initializer) (int fd, void * elt));
#endif  /* INCLUDE__ARRAYS__POW2_ARRAY_PRINT_H */
