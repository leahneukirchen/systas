/* stackchk.c - checking for excessive stack depth
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <stddef.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/boolean.h"


/************************************************************************
 *t: Stack Depth Checking
 *
 * Programs can impose a soft limit on the amount of stack used
 * by the interpreter.  If the limit is exceeded (at a time when
 * the interpreter checks), an exception is generated.
 */

/****************************************************************
 * Stack Depth Checking Rationale
 *
 * A program with run-away recursion interacts very poorly with
 * typical virtual memory systems.
 */


int scm_check_stack_p = 0;
unsigned long scm_stack_limit = 1048576;



/*s
 * check-stack-depth state
 * 
 * If state is not #f, enable stack checking.  
 * If state is #f, disable stack checking.
 *
 * When enabled, if the stack depth limit is exceeded,
 * throw an exception of type `stack-depth':
 *
 *	(throw 'stack-depth "maximum stack depth exceeded")
 *
 * See `set-stack-depth-limit'.
 */
SCM_PROC (s_check_stack_depth, "check-stack-depth", 1, 0, 0, scm_check_stack_depth);
SCM
scm_check_stack_depth (SCM state)
{
  scm_check_stack_p = (state != SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}

/*s
 * stack-depth-checking-enabled?
 * 
 * Return #t if stack depth checking is enabled, #f otherwise.
 * (See `check-stack-depth'.)
 */
SCM_PROC (s_stack_depth_checking_enabled_p, "stack-depth-checking-enabled?", 0, 0, 0, scm_stack_depth_checking_enabled_p);
SCM
scm_stack_depth_checking_enabled_p (void)
{
  return scm_int_to_bool (scm_check_stack_p);
}

/*s
 * stack-depth-limit
 * 
 * Return the current stack depth limit. 
 * (See `check-stack-depth'.)
 */
SCM_PROC (s_stack_depth_limit, "stack-depth-limit", 0, 0, 0, scm_stack_depth_limit);
SCM
scm_stack_depth_limit (void)
{
  return scm_long2num (scm_stack_limit);
}

/*s
 * set-stack-depth-limit amt
 * 
 * Set the current stack depth limit. 
 * (See `check-stack-depth'.)
 */
SCM_PROC (s_set_stack_depth_limit, "set-stack-depth-limit", 0, 0, 0, scm_set_stack_depth_limit);
SCM
scm_set_stack_depth_limit (SCM amt)
{
  scm_stack_limit = scm_long2num (amt);
  return SCM_UNSPECIFIED;
}



void
scm_init_stackchk (void)
{
  SCM_INTS_DISABLED;

#ifdef SCM_STACK_LIMIT
  scm_check_stack_p = 1;
#endif

#include "systas/libsystas/stackchk.x"
}

