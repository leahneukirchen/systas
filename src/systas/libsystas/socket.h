/* socket.h - decls for socket functions not operating on file descriptors
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998,1999 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__SOCKET_H
#define INCLUDE__LIBSYSTAS__SOCKET_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_inet_aton (SCM address);
extern SCM scm_inet_ntoa (SCM inetid);
extern SCM scm_inet_netof (SCM address);
extern SCM scm_inet_lnaof (SCM address);
extern SCM scm_inet_makeaddr (SCM net, SCM lna);
extern SCM scm_addrtype2scm (int n);
extern int scm_scm2addrtype (SCM s, SCM pos, SCM s_caller);
extern SCM scm_gethostent (void);
extern SCM scm_gethostbyname (SCM name);
extern SCM scm_gethostbyaddr (SCM name);
extern SCM scm_getnetent (void);
extern SCM scm_getnetbyname (SCM name);
extern SCM scm_getnetbyaddr (SCM name);
extern SCM scm_getprotoent (void);
extern SCM scm_getprotobyname (SCM name);
extern SCM scm_protobynumber (SCM name);
extern SCM scm_getservent (void);
extern SCM scm_getservbyname (SCM name, SCM proto);
extern SCM scm_getservbyport (SCM port, SCM proto);
extern SCM scm_sethostent (SCM arg);
extern SCM scm_endhost (void);
extern SCM scm_setnetent (SCM arg);
extern SCM scm_endnet (void);
extern SCM scm_setservent (SCM arg);
extern SCM scm_endserv (void);
extern SCM scm_setprotoent (SCM arg);
extern SCM scm_endproto (void);
extern SCM scm_gethostname (void);
extern void scm_init_socket (void);
#endif  /* INCLUDE__LIBSYSTAS__SOCKET_H */
