/* fdsocket.h - decls for socket functions operating on file descriptors
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998,1999 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__FDSOCKET_H
#define INCLUDE__LIBSYSTAS__FDSOCKET_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern int scm_scm2socktype (SCM type, SCM why, SCM caller);
extern int scm_scm2protocol (SCM proto, SCM why, SCM caller);
extern SCM scm_sys_socket (SCM family, SCM style, SCM proto);
extern SCM scm_sys_socketpair (SCM family, SCM style, SCM proto);
extern int scm_scm2sol (SCM sol, SCM why, SCM caller);
extern int scm_scm2so (SCM so, SCM why, SCM caller);
extern SCM scm_sys_getsockopt (SCM sfd, SCM level, SCM optname);
extern SCM scm_sys_setsockopt (SCM sfd, SCM level, SCM optname, SCM value);
extern SCM scm_sys_shutdown (SCM sfd, SCM how);
extern SCM scm_sys_connect (SCM sockfd, SCM family, SCM address, SCM args);
extern SCM scm_sys_bind (SCM sockfd, SCM family, SCM address, SCM args);
extern SCM scm_sys_listen (SCM sfd, SCM backlog);
extern SCM scm_sys_accept (SCM sockfd);
extern SCM scm_sys_getsockname (SCM sockfd);
extern SCM scm_sys_getpeername (SCM sockfd);
extern int scm_scm2sendrecv_flags (SCM flags, SCM why, SCM caller);
extern SCM scm_sys_recv (SCM sockfd, SCM buff, SCM flags);
extern SCM scm_sys_send (SCM sockfd, SCM message, SCM flags);
extern SCM scm_sys_recvfrom (SCM sockfd, SCM buff, SCM flags);
extern SCM scm_sys_sendto (SCM sockfd, SCM message, SCM family, SCM address, SCM args_and_flags);
extern void scm_init_fdsocket (void);
#endif  /* INCLUDE__LIBSYSTAS__FDSOCKET_H */
