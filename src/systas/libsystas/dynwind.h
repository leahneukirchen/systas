/* dynwind.h - decls for scheme dynamic-wind
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__DYNWIND_H
#define INCLUDE__LIBSYSTAS__DYNWIND_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern void scm_dowinds (SCM to, long delta);
extern SCM scm_dynamic_wind (SCM thunk1, SCM thunk2, SCM thunk3);
extern SCM scm_c_dynamic_wind (void (*guard)(int direction, void *),
			       SCM (*body)(void *),
			       void * closure);
extern void scm_init_dynwind (void);
#endif  /* INCLUDE__LIBSYSTAS__DYNWIND_H */
