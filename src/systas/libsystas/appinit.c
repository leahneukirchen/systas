/* appinit.c - default application-specific initialization for systas
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <stddef.h>
#include "systas/libsystas/appinit.h"


/************************************************************************
 *h0 "Application-Specific Initialization")
 * 
 * 
 * 
 */


/*(c scm_appinit)
 * void scm_appinit (void);
 * 
 * During initialization, the library calls `scm_appinit' to perform
 * application-specific initialization.  The default definition
 * of that function does nothing.  Applications can supply their
 * own version of this function.
 */
void
scm_appinit (void)
{
  SCM_INTS_DISABLED;
}

