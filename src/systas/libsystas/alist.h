/* alist.h - decls for scheme association lists
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__ALIST_H
#define INCLUDE__LIBSYSTAS__ALIST_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_acons (SCM key, SCM value, SCM alist);
extern SCM scm_assq(SCM key, SCM alist);
extern SCM scm_assv(SCM x, SCM alist);
extern SCM scm_assoc (SCM x, SCM alist, SCM compare);
extern SCM scm_assq_ref (SCM alist, SCM key);
extern SCM scm_assv_ref (SCM alist, SCM key);
extern SCM scm_assoc_ref (SCM alist, SCM key, SCM compare);
extern SCM scm_assq_set_x (SCM alist, SCM key, SCM val);
extern SCM scm_assv_set_x (SCM alist, SCM key, SCM val);
extern SCM scm_assoc_set_x (SCM alist, SCM key, SCM val, SCM compare);
extern SCM scm_assq_remove_x (SCM alist, SCM key);
extern SCM scm_assv_remove_x (SCM alist, SCM key);
extern SCM scm_assoc_remove_x (SCM alist, SCM key, SCM compare);
extern void scm_init_alist (void);
#endif  /* INCLUDE__LIBSYSTAS__ALIST_H */
