/* tag: Tom Lord Tue Dec  4 14:41:46 2001 (combine-db-macros.h)
 */
/* combine-db-macros.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNIDATA__COMBINE_DB_MACROS_H
#define INCLUDE__UNIDATA__COMBINE_DB_MACROS_H

/************************************************************************
 *(h2 "Canonical Combining Class")
 * 
 * 
 * 
 */

/*(c unidata_canonical_combining_class :category macro)
 * #define unidata_canonical_combining_class(C)
 * 
 * Return the canonical combining class of a Unicode
 * character.
 * 
 * Combining classes are represented as unsigned 8-bit integers.
 */
#define unidata_canonical_combining_class(C)	(unidata__combine_db_ref(unidata__combine_db, (C)))


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNIDATA__COMBINE_DB_MACROS_H */
