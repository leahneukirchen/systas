/* struct.h - user defined data typesa
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__STRUCT_H
#define INCLUDE__LIBSYSTAS__STRUCT_H

#include "systas/libsystas/scm.h"


/* automatically generated __STDC__ prototypes */
extern SCM scm_structure_p (SCM obj);
extern SCM scm_make_structure (SCM type, SCM data);
extern SCM scm_structure_data (SCM private_type, SCM structure);
extern SCM scm_structure_public_type (SCM structure);
extern SCM scm_set_structure_print_function (SCM type, SCM print_function);
extern int scm_is_struct (SCM obj);
extern void scm_init_struct (void);
#endif  /* INCLUDE__LIBSYSTAS__STRUCT_H */
