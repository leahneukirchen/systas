/* limits.h - Corrections to <limits.h> for Rx.
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX_POSIX__LIMITS_H
#define INCLUDE__RX_POSIX__LIMITS_H


/* Posix.2 requires regexp implementations to define `RE_DUP_MAX'
 * but unfortunately specifies that it should be defined in "<limits.h>".
 * 
 * If Rx is not the default implementation on a system, the value
 * in "<limits.h>" may not be correct.
 * 
 * Applications substituting Rx for the native implementation can
 * use the following sequence of includes to obtain declarations
 * for both the Posix regex functions and RE_DUP_MAX:
 * 
 * 	#include <sys/types.h>
 * 	#include <limits.h>
 * 	#include "hackerlab/rx-posix/limits.h"
 * 	#include "hackerlab/rx-posix/regex.h"
 * 
 */

#include "hackerlab/rx-posix/dup-max.h"

#undef RE_DUP_MAX
#define RE_DUP_MAX	RX_DUP_MAX


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__RX_POSIX__LIMITS_H */
