/* cvt-double.h - decls for converting floating point to and from strings
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__FMT__CVT_DOUBLE_H
#define INCLUDE__FMT__CVT_DOUBLE_H



#include "hackerlab/machine/types.h"


/* automatically generated __STDC__ prototypes */
extern int cvt_double_to_decimal (t_uchar * buffer,
				  int * base10_expt,
				  double k,
				  int precision,
				  char format);
extern int main (int argc, char * argv[]);
#endif  /* INCLUDE__FMT__CVT_DOUBLE_H */
