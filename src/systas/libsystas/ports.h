/* tag: Tom Lord Tue Dec  4 14:41:54 2001 (ports.h)
 */
/* ports.h -
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__PORTS_H
#define INCLUDE__LIBSYSTAS__PORTS_H



#include "systas/libsystas/scm.h"
#include "systas/libsystas/filesys.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_port_p (SCM x);
extern SCM scm_input_port_p (SCM x);
extern SCM scm_output_port_p (SCM x);
extern SCM scm_current_input_port (void);
extern SCM scm_current_output_port (void);
extern SCM scm_current_error_port (void);
extern SCM scm_current_load_port (void);
extern SCM scm_set_current_input_port (SCM port);
extern SCM scm_set_current_output_port (SCM port);
extern SCM scm_set_current_error_port (SCM port);
extern SCM scm_open_file (SCM name, SCM flags, SCM mode);
extern SCM scm_sys_open_buffered (SCM name, SCM sflags, SCM mode);
extern SCM scm_close_port (SCM port);
extern SCM scm_eof_object_p (SCM x);
extern SCM scm_read_char (SCM port);
extern SCM scm_force_output (SCM port);
extern SCM scm_peek_char (SCM port);
extern SCM scm_char_ready_p (SCM port);
extern int scm_is_port (SCM obj);
extern int scm_port_putc (int * errn, SCM port, int c);
extern int scm_port_getc (int * errn, SCM port);
extern int scm_port_ungetc (int * errn, SCM port, int c);
extern long scm_port_write (int * errn, SCM port, t_uchar * buf, long len);
extern long scm_port_puts (int * errn, SCM port, t_uchar * str);
extern int scm_port_flush (int * errn, SCM port);
extern void scm_init_ports (void);
#endif  /* INCLUDE__LIBSYSTAS__PORTS_H */
