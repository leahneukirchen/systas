/* list.h - decls for scheme lists
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__LIST_H
#define INCLUDE__LIBSYSTAS__LIST_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_null_p(SCM x);
extern SCM scm_list_p(SCM x);
extern SCM scm_list(SCM objs);
extern SCM scm_list_append(SCM args);
extern SCM scm_append (SCM arguments);
extern SCM scm_list_append_x(SCM args);
extern SCM scm_append_x (SCM arguments);
extern SCM scm_list_copy (SCM lst);
extern SCM scm_copy_tree (SCM obj);
extern SCM scm_list_length(SCM x);
extern SCM scm_list_length_plus (SCM x);
extern SCM scm_soft_list_length(SCM x);
extern SCM scm_list_reverse (SCM lst, SCM res);
extern SCM scm_list_reverse_x (SCM lst, SCM newtail);
extern SCM scm_reverse_x (SCM list, SCM newtail);
extern SCM scm_list_head(SCM lst, SCM k);
extern SCM scm_list_tail(SCM lst, SCM k);
extern SCM scm_last_pair(SCM sx);
extern SCM scm_list_ref(SCM lst, SCM k);
extern SCM scm_list_set_x(SCM lst, SCM k, SCM val);
extern SCM scm_list_cdr_ref (SCM list, SCM k);
extern SCM scm_list_cdr_set_x(SCM lst, SCM k, SCM val);
extern SCM scm_memq(SCM x, SCM lst);
extern SCM scm_memv(SCM x, SCM lst);
extern SCM scm_member (SCM x, SCM lst, SCM pred);
extern SCM scm_delq_x (SCM item, SCM lst);
extern SCM scm_delq (SCM item, SCM lst);
extern SCM scm_delv_x (SCM item, SCM lst);
extern SCM scm_delv (SCM item, SCM lst);
extern SCM scm_delete_x (SCM item, SCM lst, SCM compare);
extern SCM scm_delete (SCM item, SCM lst, SCM compare);
extern SCM scm_map (SCM proc, SCM arg1, SCM args);
extern SCM scm_for_each (SCM proc, SCM arg1, SCM args);
extern SCM scm_listify (SCM elt, ...);
extern long scm_ilength (SCM sx);
extern long scm_sloppy_ilength (SCM sx);
extern long scm_eilength (SCM sx);
extern void scm_init_list (void);
#endif  /* INCLUDE__LIBSYSTAS__LIST_H */
