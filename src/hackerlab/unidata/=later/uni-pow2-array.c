/* tag: Tom Lord Tue Dec  4 14:41:45 2001 (uni-pow2-array.c)
 */
/* uni-pow2-array.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <stddef.h>
#include "hackerlab/machine/types.h"
#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/arrays/pow2-array.h"
#include "hackerlab/uni/uni-pow2-array.h"



struct pow2_array_level_rule uni_pow2_array_levels_5_16[] = 
{
  {16, 0x1f},
  {0, 0xffff}
};

struct pow2_array_level_rule uni_pow2_array_levels_10_11[] = 
{
  {11, 0x3ff},
  {0, 0x7ff}
};

struct pow2_array_level_rule uni_pow2_array_levels_8_5_8[] = 
{
  {13, 0xff},
  {8, 0x1f},
  {0, 0xff}
};

