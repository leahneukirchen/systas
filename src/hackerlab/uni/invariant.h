/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (invariant.h)
 */
/* invariant.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__INVARIANT_H
#define INCLUDE__UNI__INVARIANT_H


#include "hackerlab/bugs/panic.h"



#ifdef UNI_DANGEROUSLY
#define uni_invariant(EXP)
#else
#define uni_invariant(EXP)	invariant(EXP)
#endif


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNI__INVARIANT_H */
