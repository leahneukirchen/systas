/* tag: Tom Lord Tue Dec  4 14:41:39 2001 (safe-printfmt.h)
 */
/* safe-printfmt.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__VU__SAFE_PRINTFMT_H
#define INCLUDE__VU__SAFE_PRINTFMT_H


#include "hackerlab/vu/printfmt.h"

#if !defined(__GNUC__)
#  undef __attribute__
#  define __attribute__(X)
#endif

extern void safe_printfmt (int fd, char * fmt, ...)
     __attribute__((format (printf, 2, 3)));


/* automatically generated __STDC__ prototypes */
extern void safe_printfmt (int fd, char * fmt, ...);
extern void safe_printfmt_va_list (int fd, char * fmt, va_list ap);
#endif  /* INCLUDE__VU__SAFE_PRINTFMT_H */
