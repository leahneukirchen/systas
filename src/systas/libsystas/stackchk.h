/* stackchk.h - decls for checking for excessive stack depth
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__STACKCHK_H
#define INCLUDE__LIBSYSTAS__STACKCHK_H

#include "systas/libsystas/scm.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/error.h"



/* When not 0, stack depth checking occurs.
 */
extern int scm_check_stack_p;

/* The stack depth at which exceptions are thrown.
 */
extern unsigned long scm_stack_limit;

# define SCM_CHECK_STACK \
    { \
       SCM_STACKITEM stack; \
       if (scm_check_stack_p && SCM_STACK_OVERFLOW_P (&stack)) \
         scm_err_escape (0, scm_stack_depth, scm_stack_depth_exceeded, SCM_UNDEFINED); \
    }


#ifdef SCM_STACK_GROWS_UP
# define SCM_STACK_OVERFLOW_P(s) \
	  (  (unsigned long)(s - SCM_REGS (scm_rootcont)->base) \
	   > (scm_stack_limit * (unsigned int)sizeof (SCM_STACKITEM)))
#else
# define SCM_STACK_OVERFLOW_P(s) \
	  ((SCM_REGS (scm_rootcont)->base - s) > (scm_stack_limit * sizeof (SCM_STACKITEM)))
#endif



/* automatically generated __STDC__ prototypes */
extern SCM scm_check_stack_depth (SCM state);
extern SCM scm_stack_depth_checking_enabled_p (void);
extern SCM scm_stack_depth_limit (void);
extern SCM scm_set_stack_depth_limit (SCM amt);
extern void scm_init_stackchk (void);
#endif  /* INCLUDE__LIBSYSTAS__STACKCHK_H */
