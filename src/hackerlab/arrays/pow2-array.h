/* tag: Tom Lord Tue Dec  4 14:41:51 2001 (pow2-array.h)
 */
/* pow2-array.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__ARRAYS__POW2_ARRAY_H
#define INCLUDE__ARRAYS__POW2_ARRAY_H



#include "hackerlab/mem/alloc-limits.h"



struct pow2_array_level_rule
{
  int addr_shift;
  size_t addr_mask;
};

struct pow2_array_rules
{
  void ** defaults;
  struct pow2_array_level_rule * levels;
  size_t elt_size;
};

typedef struct pow2_array_rules * pow2_array_rules;

struct pow2_array
{
  struct alloc_limits * limits;
  pow2_array_rules rules;
  void * root;
};

typedef struct pow2_array * pow2_array;



/* automatically generated __STDC__ prototypes */
extern pow2_array_rules make_pow2_array_rules (struct alloc_limits * limits,
					       size_t elt_size,
					       void * default_page,
					       int shift,
					       size_t mask,
					       ...);
extern pow2_array pow2_array_alloc (struct alloc_limits * limits,
				    pow2_array_rules rules);
extern void * pow2_array_rref (pow2_array array, size_t addr);
extern void * pow2_array_ref (pow2_array array, size_t addr);
#endif  /* INCLUDE__ARRAYS__POW2_ARRAY_H */
