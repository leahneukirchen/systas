/* macros.h - decls for constructing scheme macros
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation,Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__MACROS_H
#define INCLUDE__LIBSYSTAS__MACROS_H

#include "systas/libsystas/scm.h"



extern long scm_tc16_macro;


/* automatically generated __STDC__ prototypes */
extern SCM scm_procedure_to_syntax (SCM code);
extern SCM scm_procedure_to_macro (SCM code);
extern SCM scm_procedure_to_memoizing_macro (SCM code);
extern SCM scm_make_synt (char *name, SCM (*macroizer) (SCM), SCM (*fcn)());
extern void scm_init_macros (void);
#endif  /* INCLUDE__LIBSYSTAS__MACROS_H */
