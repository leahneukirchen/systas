/* continuations.h - decls for first-class continuations
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__CONTINUATIONS_H
#define INCLUDE__LIBSYSTAS__CONTINUATIONS_H

#include "systas/libsystas/scm.h"
#include "systas/libsystas/strings.h"



/* The C type representing a first-class continuation:
 *
 */
typedef struct 
{
  SCM throw_value;		/* A value being returned from the continuation */
  jmp_buf jmpbuf;		/* Machine registers including stack-pointer and PC */
  SCM dynwind;			/* The dynamic wind chain */
  struct scm_debug_frame * debug_info; /* The debugging chain */

  SCM_STACKITEM *base;		/* The top of the stack (for Scheme) */
  unsigned long seq;		/* See "Continuations Internals" in "continuations.c" */
} scm_regs;

/* Access to the scm_regs of continuation x
 */
#define SCM_REGS(x) ((scm_regs *)SCM_CDR(x))
#define SCM_STACK(x) ((t_uchar *)SCM_CDR(x))



/* automatically generated __STDC__ prototypes */
extern SCM scm_make_cont (SCM * answer);
extern void scm_call_continuation (SCM cont, SCM val);
extern void scm_init_continuations (void);
#endif  /* INCLUDE__LIBSYSTAS__CONTINUATIONS_H */
