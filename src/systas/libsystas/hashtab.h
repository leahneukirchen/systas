/* hashtab.h - decls for hash tables
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__HASHTAB_H
#define INCLUDE__LIBSYSTAS__HASHTAB_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_hashq_get_handle (SCM table, SCM obj);
extern SCM scm_hashq_create_handle_x (SCM table, SCM obj, SCM init);
extern SCM scm_hashq_ref (SCM table, SCM obj, SCM dflt);
extern SCM scm_hashq_set_x (SCM table, SCM obj, SCM val);
extern SCM scm_hashq_remove_x (SCM table, SCM obj);
extern SCM scm_hashv_get_handle (SCM table, SCM obj);
extern SCM scm_hashv_create_handle_x (SCM table, SCM obj, SCM init);
extern SCM scm_hashv_ref (SCM table, SCM obj, SCM dflt);
extern SCM scm_hashv_set_x (SCM table, SCM obj, SCM val);
extern SCM scm_hashv_remove_x (SCM table, SCM obj);
extern SCM scm_hash_get_handle (SCM table, SCM obj);
extern SCM scm_hash_create_handle_x (SCM table, SCM obj, SCM init);
extern SCM scm_hash_ref (SCM table, SCM obj, SCM dflt);
extern SCM scm_hash_set_x (SCM table, SCM obj, SCM val);
extern SCM scm_hash_remove_x (SCM table, SCM obj);
extern SCM scm_hashx_get_handle (SCM hash, SCM assoc, SCM table, SCM obj);
extern SCM scm_hashx_create_handle_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM init);
extern SCM scm_hashx_ref (SCM hash, SCM assoc, SCM table, SCM obj, SCM dflt);
extern SCM scm_hashx_set_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM val);
extern SCM scm_hashx_remove_x (SCM hash, SCM assoc, SCM del, SCM table, SCM obj);
extern void scm_init_hashtree (void);
#endif  /* INCLUDE__LIBSYSTAS__HASHTAB_H */
