/* hash.h - decls for scheme hash functions
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__HASH_H
#define INCLUDE__LIBSYSTAS__HASH_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_hashq(SCM obj, SCM n);
extern SCM scm_hashv(SCM obj, SCM n);
extern SCM scm_hash(SCM obj, SCM n);
extern unsigned long scm_strhash (t_uchar *str, size_t len, unsigned long n);
extern unsigned int scm_ihashq (SCM obj, unsigned int n);
extern unsigned int scm_ihashv (SCM obj, unsigned int n);
extern unsigned int scm_ihash (SCM obj, unsigned int n);
extern void scm_init_hash (void);
#endif  /* INCLUDE__LIBSYSTAS__HASH_H */
