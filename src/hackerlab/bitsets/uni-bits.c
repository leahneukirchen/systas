/* tag: Tom Lord Tue Dec  4 14:41:35 2001 (uni-bits.c)
 */
/* uni-bits.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bitsets/uni-bits.h"


/************************************************************************
 *(h1 "Unicode Character Bitsets")
 * 
 * 
 * 
 */

/*(c uni_bits_tree_rule :category variable)
 * struct bits_tree_rule uni_bits_tree_rule[];
 * 
 * `uni_bits_tree_rule' defines a bitset tree branching structure
 * suitable for representing sparse sets of Unicode code points.
 * (See xref:"Bitset Tree Rules".)
 * 
 * Each set has {1 << 21} elements.  
 * 
 * This tree structure has been tuned to efficiently represent
 * sets corresponding to each of the Unicode General Categories
 * of characters.  (See xref:"Unicode Category Bitsets".)
 */

struct bits_tree_rule uni_bits_tree_rule[] = {{32, 1<<16, 16, 0xffff}, {16, 1<<12, 12, 0xfff}, {16, 256, 8, 0xff}, {0, 256, 0, 0}};

