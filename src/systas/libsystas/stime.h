/* stime.h - time-related procedure decls
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#ifndef INCLUDE__LIBSYSTAS__STIME_H
#define INCLUDE__LIBSYSTAS__STIME_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_sys_time ();
extern SCM scm_time(void);
extern SCM scm_sys_times (void);
extern SCM scm_times (void);
extern SCM scm_get_internal_run_time(void);
extern SCM scm_sys_gettimeofday ();
extern SCM scm_get_internal_real_time(void);
extern void scm_init_stime(void);
#endif  /* INCLUDE__LIBSYSTAS__STIME_H */
