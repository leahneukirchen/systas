/* tag: Tom Lord Tue Dec  4 14:41:43 2001 (blocks.h)
 */
/* blocks.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNIDATA__BLOCKS_H
#define INCLUDE__UNIDATA__BLOCKS_H


#include "hackerlab/machine/types.h"



/************************************************************************
 *(h2 "Unicode Blocks")
 * 
 */



/*(c #s"struct uni_block" :category type)
 * struct uni_block;
 * 
 * Structures of this type describe one of the standard blocks of
 * Unicode characters (`"Basic Latin"', `"Latin-1 Supplement"', etc.)
 * 
 insert*/

struct uni_block
{
  t_uchar * name;	/* name of the block */
  t_unichar start;	/* first character in the block */
  t_unichar end;	/* last character in the block */
};
/*end-insert
 * 
 * 
 */



/*(c uni_blocks :category variable)
 * extern struct uni_block uni_blocks[];
 * 
 * The names of the standard Unicode blocks.  This array is sorted 
 * in code-point order, from least to greatest.
 * 
 * `n_uni_blocks' is the number of blocks in `uni_blocks'.
 * 
 * 	uni_blocks[n_uni_blocks].name == 0
 *
 insert*/

extern const struct uni_block uni_blocks[];
extern const int n_uni_blocks;

/*end-insert
 */


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNIDATA__BLOCKS_H */

