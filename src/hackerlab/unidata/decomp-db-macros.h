/* tag: Tom Lord Tue Dec  4 14:41:47 2001 (decomp-db-macros.h)
 */
/* decomp-db-macros.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNIDATA__DECOMP_DB_MACROS_H
#define INCLUDE__UNIDATA__DECOMP_DB_MACROS_H

#include "hackerlab/uni/unidata.h"


/************************************************************************
 *(h2 "Character Decomposition Mapping")
 * 
 */

/*(c #s"enum uni_decomposition_type" :category type)
 * enum uni_decomposition_type;
 * 
 * The decomposition mapping of a character is described by
 * values of this enumerated type:
 * 
 * 
 *	uni_decomposition_none
 *	uni_decomposition_canonical
 *	uni_decomposition_font
 *	uni_decomposition_noBreak
 *	uni_decomposition_initial
 *	uni_decomposition_medial
 *	uni_decomposition_final
 *	uni_decomposition_isolated
 *	uni_decomposition_circle
 *	uni_decomposition_super
 *	uni_decomposition_sub
 *	uni_decomposition_vertical
 *	uni_decomposition_wide
 *	uni_decomposition_narrow
 *	uni_decomposition_small
 *	uni_decomposition_square
 *	uni_decomposition_fraction
 *	uni_decomposition_compat
 * 
 * The value `uni_decomposition_none' indicates that a character
 * has no decomposition mapping.
 */

/*(c #s"struct uni_decomposition_mapping" :category type)
 * struct uni_decomposition_mapping;
 * 
 * A character's decomposition mapping is described by this
 * structure.  It has the fields:
 * 
 * 	enum uni_decomposition_type type;
 * 	t_unicode * decomposition;
 *
 * `type' is the type of decomposition.
 * 
 * If `type' is not `uni_decomposition_none', then `decomposition'
 * is a 0-termianted array of code points which are the decomposition
 * of the character.
 */


/*(c unidata_character_decomposition_mapping :category macro)
 * #define unidata_character_decomposition_mapping(C)
 * 
 * Return the decomposition mapping of `C'.  This macro returns
 * a pointer to a `struct uni_decomposition_mapping'.
 */
#define unidata_character_decomposition_mapping(C)	(&(unidata_decomposition_table[unidata__decomp_db_ref (unidata__decomp_db, (C))]))


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNIDATA__DECOMP_DB_MACROS_H */
