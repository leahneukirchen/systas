/* system.h - system functions
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */

#ifndef INCLUDE__LIBSYSTAS__SYSTEM_H
#define INCLUDE__LIBSYSTAS__SYSTEM_H


#include "systas/libsystas/scm.h"



extern int scm_tc16_errno;

/* Is the X an errno object?
 */
#define SCM_ERRNOP(X)		(SCM_BOOL_F != scm_errno_p (X))

/* Access to the errno number of the errno object X.
 */
#define SCM_ERRNO_ERR(X)	(-SCM_INUM (X))


/* automatically generated __STDC__ prototypes */
extern SCM scm_makerrno (int x);
extern SCM scm_errno_p (SCM x);
extern SCM scm_errno_to_integer (SCM x);
extern SCM scm_strerror (SCM arg);
extern SCM scm_sys_getgroups(void);
extern SCM scm_getpwent (void);
extern SCM scm_setpwent (void);
extern SCM scm_endpwent (void);
extern SCM scm_getpwuid (SCM user);
extern SCM scm_getpwnam (SCM user);
extern SCM scm_getgrent (void);
extern SCM scm_getgrgid (SCM name);
extern SCM scm_getgrnam (SCM name);
extern SCM scm_setgrent (void);
extern SCM scm_endgrent (void);
extern SCM scm_signal_name_to_integer (SCM name);
extern SCM scm_sys_kill (SCM pid, SCM sig);
extern SCM scm_sys_killpg (SCM pgid, SCM sig);
extern SCM scm_wait_options_to_integer (SCM options);
extern SCM scm_sys_waitpid (SCM pid, SCM options);
extern SCM scm_getpid (void);
extern SCM scm_getppid (void);
extern SCM scm_getuid (void);
extern SCM scm_getgid (void);
extern SCM scm_geteuid (void);
extern SCM scm_getegid (void);
extern SCM scm_sys_setuid (SCM id);
extern SCM scm_sys_setgid (SCM id);
extern SCM scm_sys_seteuid (SCM id);
extern SCM scm_sys_setegid (SCM id);
extern SCM scm_getpgrp (void);
extern SCM scm_sys_setpgid (SCM pid, SCM pgid);
extern SCM scm_sys_getpgid (SCM pid);
extern SCM scm_sys_setsid (void);
extern SCM scm_sys_exec (SCM filename, SCM args, SCM env);
extern SCM scm_sys_fork(void);
extern SCM scm_sys_exit (SCM x);
extern SCM scm_sys_uname (void);
extern SCM scm_environ (SCM env);
extern SCM scm_sys_access_p (SCM path, SCM show);
extern SCM scm_getenv(SCM nam);
extern SCM scm_sys_setenv (SCM skey, SCM sval, SCM sover);
extern SCM scm_unsetenv (SCM skey);
extern SCM scm_sys_putenv (SCM str);
extern SCM scm_ctime (SCM stime);
extern SCM scm_localtime (SCM stime);
extern SCM scm_sys_nice(SCM incr);
extern SCM scm_sys_sync(void);
extern SCM scm_sys_truncate (SCM path, SCM len);
extern SCM scm_alarm (SCM i);
extern SCM scm_pause (void);
extern SCM scm_sleep (SCM i);
extern void scm_declare_integer_cpp_constant (SCM type, t_uchar * name, SCM value);
extern int scm_integer_cpp_constant (SCM type, SCM argument, SCM error_msg, SCM subr_name);
extern int scm_integer_logior_cpp_constants (SCM type, SCM argument, SCM error_msg, SCM subr_name);
extern void scm_init_system (void);
#endif  /* INCLUDE__LIBSYSTAS__SYSTEM_H */
