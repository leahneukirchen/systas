/* promise.h - decls for delayed evaluation
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__PROMISE_H
#define INCLUDE__LIBSYSTAS__PROMISE_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_promise_p (SCM x);
extern SCM scm_make_promise (SCM thunk);
extern SCM scm_force (SCM x);
extern void scm_init_promise (void);
#endif  /* INCLUDE__LIBSYSTAS__PROMISE_H */
