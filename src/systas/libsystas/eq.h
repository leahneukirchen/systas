/* eq.h - decls for scheme tests for equality
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__EQ_H
#define INCLUDE__LIBSYSTAS__EQ_H

#include "systas/libsystas/scm.h"



/* automatically generated __STDC__ prototypes */
extern SCM scm_eq_p (SCM x, SCM y);
extern SCM scm_eqv_p (SCM x, SCM y);
extern SCM scm_equal_p (SCM x, SCM y);
extern SCM scm_generalized_equal_p (SCM pred, SCM a, SCM b);
extern void scm_init_eq (void);
#endif  /* INCLUDE__LIBSYSTAS__EQ_H */
