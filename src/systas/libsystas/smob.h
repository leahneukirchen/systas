/* smob.h - small object extension types
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#ifndef INCLUDE__LIBSYSTAS__SMOB_H
#define INCLUDE__LIBSYSTAS__SMOB_H

#include "systas/libsystas/scm.h"



typedef struct scm_small_object_functions
{
  SCM (*mark) (SCM);
  size_t (*free) (SCM);
  int (*print) (SCM exp, SCM port, int writing);
  SCM (*equalp) (SCM, SCM);
} scm_small_object_functions;



#define SCM_SMOBNUM(x) (0x0ff & (SCM_CAR(x)>>8));

extern scm_small_object_functions *scm_smobs;
extern int scm_numsmob;


/* automatically generated __STDC__ prototypes */
extern long scm_newsmob (scm_small_object_functions *smob);
extern void scm_smob_prehistory (void);
#endif  /* INCLUDE__LIBSYSTAS__SMOB_H */
