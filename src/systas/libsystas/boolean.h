/* boolean.h - decls for scheme boolean values
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__BOOLEAN_H
#define INCLUDE__LIBSYSTAS__BOOLEAN_H

#include "systas/libsystas/scm.h"



/* automatically generated __STDC__ prototypes */
extern SCM scm_not(SCM x);
extern SCM scm_boolean_p(SCM obj);
extern SCM scm_to_bool (SCM obj);
extern SCM scm_int_to_bool (int x);
extern void scm_init_boolean (void);
#endif  /* INCLUDE__LIBSYSTAS__BOOLEAN_H */
