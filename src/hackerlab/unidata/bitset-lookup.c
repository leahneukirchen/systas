/* bitset-lookup.c - lookup a unicode category bitset
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/unidata/bitsets.h"
#include "hackerlab/unidata/bitset-lookup.h"


/************************************************************************
 *(h2 "Unicode Category Bitsets")
 * 
 * 
 */


/*(c uni_universal_bitset)
 * bits uni_universal_bitset (void);
 * 
 * Return the set of all assigned code points which are not surrogate
 * code points and are not private use code points.  The set is
 * represented as a shared bitset tree.  (See xref:"Shared Bitset
 * Trees".)
 * 
 * The shared bitset tree returned by this function uses the tree
 * structure defined by `uni_bits_tree_rule'.  (See xref:"Unicode
 * Character Bitsets".)
 * 
 * Programs should not attempt to modify the set returned by
 * this function.
 */
bits
uni_universal_bitset (void)
{
   return unidata_bitset_universal;
}


/*(c uni_general_category_bitset)
 * bits uni_general_category_bitset (enum uni_general_category c);
 * 
 * Return the set of all assigned code points having the indicated
 * general category or synthetic general category.  The set is
 * represented as a shared bitset tree.  (See xref:"Shared Bitset
 * Trees".)
 * 
 * `c' indicates which category to return.  It may be a Unicode
 * general category or a synthetic general category.  (See
 * xref:"General Category".)
 * 
 * The shared bitset tree returned by this function uses the tree
 * structure defined by `uni_bits_tree_rule'.  (See xref:"Unicode
 * Character Bitsets".)
 * 
 * Programs should not attempt to modify the set returned by
 * this function.
 * 
 */
bits
uni_general_category_bitset (enum uni_general_category c)
{

  switch (c)
    {
    default:
      panic ("unrecognized general category in uni_general_category_bitset");

#define UNI_GENERAL_CATEGORY_SET(NAME, ALT_NAME, DESC) \
    case uni_general_category_ ## NAME: \
      return unidata_bitset_ ## NAME;

#define UNI_SYNTH_CATEGORY_SET(NAME, ALT_NAME, DESC) 		UNI_GENERAL_CATEGORY_SET(NAME, ALT_NAME, DESC)
	
      UNI_GENERAL_CATEGORIES_LIST(_SET)
	UNI_SYNTHETIC_CATEGORIES_LIST(_SET)
    }
}

