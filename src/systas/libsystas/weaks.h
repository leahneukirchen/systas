/* weaks.h - decls for weak vectors and hash tables
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.   
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__WEAKS_H
#define INCLUDE__LIBSYSTAS__WEAKS_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_weak_vector_p (SCM x);
extern SCM scm_weak_key_hash_table_p (SCM x);
extern SCM scm_weak_value_hash_table_p (SCM x);
extern SCM scm_doubly_weak_hash_table_p (SCM x);
extern SCM scm_make_weak_vector (SCM k, SCM fill, SCM multi);
extern SCM scm_weak_vector (SCM l);
extern SCM scm_list_to_weak_vector (SCM elts);
extern SCM scm_make_weak_key_hash_table (SCM k);
extern SCM scm_make_weak_value_hash_table (SCM k);
extern SCM scm_make_doubly_weak_hash_table (SCM k);
extern int scm_is_weak_vector (SCM x);
extern int scm_is_weak_key_hash_table (SCM x);
extern int scm_is_weak_value_hash_table (SCM x);
extern int scm_is_doubly_weak_hash_table (SCM x);
extern int scm_is_weak_hash_table (SCM x);
extern void scm_init_weaks (void);
#endif  /* INCLUDE__LIBSYSTAS__WEAKS_H */
