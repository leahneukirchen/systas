/* printfmt.h - decls for formatted printing
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__PRINTFMT_H
#define INCLUDE__VU__PRINTFMT_H


#include "hackerlab/vu/printfmt-va.h"

#if !defined(__GNUC__)
#  undef __attribute__
#  define __attribute__(X)
#endif

extern int printfmt (int * errn, int fd, char * fmt, ...)
     __attribute__((format (printf, 3, 4)));


     /* automatically generated __STDC__ prototypes */
     extern int printfmt (int * errn, int fd, char * fmt, ...);
#endif  /* INCLUDE__VU__PRINTFMT_H */
