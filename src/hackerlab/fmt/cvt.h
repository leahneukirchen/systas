/* cvt.h - declarations for integer/string conversions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__FMT__CVT_H
#define INCLUDE__FMT__CVT_H



#include "hackerlab/machine/types.h"



/* automatically generated __STDC__ prototypes */
extern void cvt_ulong_to_decimal (t_uchar * nbuf, unsigned long n);
extern void cvt_long_to_decimal (t_uchar * nbuf, long n);
extern void cvt_ulong_to_octal (t_uchar * nbuf, unsigned long n);
extern void cvt_long_to_octal (t_uchar * nbuf, long n);
extern void cvt_ulong_to_HEX (t_uchar * nbuf, unsigned long n);
extern void cvt_long_to_HEX (t_uchar * nbuf, long n);
extern void cvt_ulong_to_hex (t_uchar * nbuf, unsigned long n);
extern void cvt_long_to_hex (t_uchar * nbuf, long n);
extern int cvt_decimal_to_ulong (int * errn, unsigned long * answerp, const t_uchar * text, size_t len);
extern int cvt_decimal_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len);
extern int cvt_decimal_to_long (int * errn, long * answerp, const t_uchar * text, size_t len);
extern int cvt_decimal_to_int (int * errn, int * answerp, const t_uchar * text, size_t len);
extern int cvt_hex_to_ulong (int * errn, unsigned long * answerp, const t_uchar * text, size_t len);
extern int cvt_hex_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len);
extern int cvt_hex_to_long (int * errn, long * answerp, const t_uchar * text, size_t len);
extern int cvt_hex_to_int (int * errn, int * answerp, const t_uchar * text, size_t len);
extern int cvt_octal_to_ulong (int * errn,
			       unsigned long * answerp,
			       const t_uchar * text,
			       size_t len);
extern int cvt_octal_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len);
extern int cvt_octal_to_long (int * errn, long * answerp, const t_uchar * text, size_t len);
extern int cvt_octal_to_int (int * errn, int * answerp, const t_uchar * text, size_t len);
#endif  /* INCLUDE__FMT__CVT_H */
