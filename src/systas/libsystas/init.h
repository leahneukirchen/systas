/* init.h - decls for starting the interpreter
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__INIT_H
#define INCLUDE__LIBSYSTAS__INIT_H

#include "systas/libsystas/scm.h"



/* Status returns from scm_boot_systas.
 */
enum scm_boot_status
{
  scm_boot_ok = 0,
  scm_boot_error,
  scm_boot_emem,
  scm_boot_ereenter
};


/* automatically generated __STDC__ prototypes */
extern void scm_restart_stack (void * base);
extern int scm_boot_systas (char ** result,
			    int argc, char ** argv,
			    int in, int out, int err,
			    char * boot_cmd);
extern SCM scm_program_arguments (void);
extern void scm_init_init (void);
#endif  /* INCLUDE__LIBSYSTAS__INIT_H */
