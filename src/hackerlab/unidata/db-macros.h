/* tag: Tom Lord Tue Dec  4 14:41:45 2001 (db-macros.h)
 */
/* db-macros.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNIDATA__DB_MACROS_H
#define INCLUDE__UNIDATA__DB_MACROS_H




#define unidata__assemble_db(ASSIGNED, DEC, M, BC, GC) \
  ((((ASSIGNED) & 1)  << 15) | (((DEC) & 0xf) << 11) | (((M) & 1) << 10) | (((BC) & 0x1f) << 5) | (((GC) & 0x1f) << 0))

#define unidata__db_is_assigned_code_point(DBV)		((DBV) & 0x8000)
#define unidata__db_decimal_digit_value(DBV)		(((DBV) >> 11) & 0xf)
#define unidata__db_is_mirrored(DBV)			(((DBV) >> 10) & 1)
#define unidata__db_bidi_category(DBV)			(((DBV) >> 5) & 0x1f)
#define unidata__db_general_category(DBV)		(((DBV) >> 0) & 0x1f)




/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNIDATA__DB_MACROS_H */
