/* tag: Tom Lord Tue Dec  4 14:41:44 2001 (db.h)
 */
/* db.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__DB_H
#define INCLUDE__UNI__DB_H


typedef t_uint16 uni__t_db0_page;
extern uni__t_db0_page * uni__db0[];

#define uni__db0_is_mirrored(V)		(((V) >> 15) & 1)
#define uni__db0_decimal_digit_value(V)	(((V) >> 10) & 0x1f)
#define uni__db0_bidi_categry(V)	(((V) >> 5) & 0x1f)
#define uni__db0_general_categry(V)	(((V) >> 0) & 0x1f)

#define uni__db0_page(C)		(uni__db0[(((C) >> 13) & 0xff)])
#define uni__db0_offset(C)		(uni__db0[((C) & 0x1fff)])
#define uni__db0_val(C)			(uni__db0[uni__db0_page(C)][uni__db0_offset(C)])

#define uni_is_mirrored(C)		uni__db0_is_mirrored (uni__db0_page (C))
#define uni_decimal_digit_value(C)	uni__db0_decimal_digit_value (uni__db0_page (C))
#define uni_bidi_categry(C)		uni__db0_bidi_categry (uni__db0_page (C))
#define uni_general_categry(C)		uni__db0_general_categry (uni__db0_page (C))



typedef t_uchar * uni__combining_class_db[];

#define uni__combining_class_db_page(C)		(uni__combining_class_db[(((C) >> 13) & 0xff)])
#define uni__combining_class_db_offset(C)	(uni__combining_class_db[((C) & 0x1fff)])
#define uni__combining_class_db_val(C)		(uni__combining_class_db[uni__combining_class_db_page(C)][uni__combining_class_db_offset(C)])

#define uni_canonical_combining_class(C)	uni__combining_class_db(C)




/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNI__DB_H */
