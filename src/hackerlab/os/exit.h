/* tag: Tom Lord Tue Dec  4 14:41:29 2001 (exit.h)
 */
/* exit.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__OS__EXIT_H
#define INCLUDE__OS__EXIT_H



/* An alternative to <stdlib.h> for declaring `exit' and
 * `atexit'.
 */

extern int atexit (void (*func)(void));
extern void exit (int status);

#ifndef FOR_MAKEFILE_DEPENDENCIES
#include "hackerlab/os/exit-status.h"
#endif


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__OS__EXIT_H */
