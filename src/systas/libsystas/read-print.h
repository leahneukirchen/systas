/* read-print.h - decls for reading s-expressions
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__READ_PRINT_H
#define INCLUDE__LIBSYSTAS__READ_PRINT_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern void scm_swap_read_records (int fd0, int fd1);
extern void read_fn_ungetstr_n (t_uchar * str, int len, SCM port, SCM read_params);
extern void read_fn_read_to_column (int indent_width, SCM port, SCM read_params);
extern int scm_read_string_list_elt (SCM * elt,
				     SCM * tok_buf,
				     int indent_width,
				     SCM port,
				     int case_i,
				     SCM sharp,
				     SCM read_params,
				     int carrot_terminates);
extern SCM scm_read (SCM port, SCM case_insensative_p, SCM sharp, SCM kws);
extern void scm_iprin1 (SCM exp, SCM port, int writing);
extern void scm_intprint (long n, int radix, SCM port);
extern void scm_ipruk (char *hdr, SCM ptr, SCM port);
extern SCM scm_write (SCM obj, SCM port, SCM kws);
extern SCM scm_display (SCM obj, SCM port, SCM kws);
extern SCM scm_newline(SCM port);
extern void scm_init_read_print (void);
#endif  /* INCLUDE__LIBSYSTAS__READ_PRINT_H */
