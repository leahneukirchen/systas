/* kw.h - scheme keyword decls
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__KW_H
#define INCLUDE__LIBSYSTAS__KW_H

#include "systas/libsystas/scm.h"



/* automatically generated __STDC__ prototypes */
extern SCM scm_keyword_p (SCM obj);
extern SCM scm_kw_arg_ref (SCM args, SCM kw, SCM dflt);
extern SCM scm_kw_arg_set_x (SCM args, SCM kw, SCM value);
extern SCM scm_symbol_to_keyword (SCM symbol);
extern SCM scm_keyword_to_symbol (SCM kw);
extern int scm_is_keyword (SCM obj);
extern void scm_init_kw (void);
#endif  /* INCLUDE__LIBSYSTAS__KW_H */
