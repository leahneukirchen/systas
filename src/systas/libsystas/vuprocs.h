/* vuprocs.h - decls for vu procedures
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__VUPROCS_H
#define INCLUDE__LIBSYSTAS__VUPROCS_H


#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_sys_reserv (SCM sflags);
extern SCM scm_sys_reserv_pseudo (SCM sflags);
extern SCM scm_sys_virtual_null_fd (SCM sflags);
extern SCM scm_sys_vfdbuf_buffer_fd (SCM sfd, SCM sbufsize, SCM sacc_flags, SCM sbuffer_flags);
extern SCM scm_vfdbuf_is_buffered_p (SCM sfd);
extern SCM scm_sys_vfdbuf_set_dont_flush (SCM sfd, SCM setting);
extern SCM scm_sys_vfdbuf_flushing_disabled_p (SCM sfd);
extern SCM scm_sys_vfdbuf_shift (SCM sfd, SCM samt);
extern SCM scm_sys_vfdbuf_advance (SCM sfd, SCM samt);
extern SCM scm_sys_vfdbuf_retreat (SCM sfd, SCM samt);
extern SCM scm_sys_vfdbuf_flush (SCM sfd);
extern SCM scm_sys_vfdbuf_is_eof (SCM sfd);
extern SCM scm_sys_vfdbuf_return (SCM sfd, SCM str);
extern SCM scm_sys_vfdbuf_get_buffered (SCM sfd);
extern SCM scm_sys_vfdbuf_set_buffer (SCM sfd, SCM sbuffer, SCM sread_write_pos, SCM sbuffered);
extern SCM scm_sys_vfdbuf_more (SCM sfd, SCM samt);
extern SCM scm_sys_make_string_port (SCM sflags, SCM init);
extern SCM scm_string_port_to_string (SCM port);
extern SCM scm_sys_write_to_string (SCM obj, SCM kws);
extern void scm_init_vuprocs (void);
#endif  /* INCLUDE__LIBSYSTAS__VUPROCS_H */
