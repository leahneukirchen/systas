/* scm.h - central declarations for systas scheme
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


#ifndef INCLUDE__LIBSYSTAS__SCM_H 
#define INCLUDE__LIBSYSTAS__SCM_H


#ifdef MDEBUG
#include "hackerlab/libpiw-malloc/piw-malloc.h"
#define PIW_CHECK_WRITES	1
#define PIW_WATCHPOINTS		1
#include "hackerlab/libpiw-malloc/piw-write-barriers.h"
#endif

#include "hackerlab/machine/types.h"
#include "hackerlab/os/setjmp.h"

/************************************************************************
 * Configuration Options
 */

#include "systas/libsystas/scmconfig.h"

#define SCM_CAUTIOUS		/* Always check arity? */
#undef SCM_RECKLESS		/* Never check arity? (Default: check once.) */
#define SCM_FLOATS
#define SCM_BIGNUMS
#define SCM_GC_FREE_SEGMENTS
#undef SCM_ENGNOT		/* How to write numbers */

#ifdef sparc
# define SCM_FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define SCM_FLUSH_REGISTER_WINDOWS /* empty */
#endif

#ifdef SCM_SHORT_ALIGN_STACK
typedef short SCM_STACKITEM;
#else
typedef long SCM_STACKITEM;
#endif


/************************************************************************
 * Interrupts
 */

extern unsigned int scm_ints_disabled;

extern unsigned int scm_mask_ints;

#define SCM_MASK_INTS	do { ++scm_mask_ints; } while (0)
#define SCM_UNMASK_INTS	do { if (scm_mask_ints > 0) --scm_mask_ints; } while (0)

extern unsigned int scm_async_clock;

#define SCM_ASYNC_TICK		\
do				\
{				\
  if (1 == scm_async_clock)	\
    scm_async_click ();		\
  else				\
    --scm_async_clock;		\
} 				\
while (0)

#define SCM_CHECK_NOT_DISABLED
#define SCM_CHECK_NOT_ENABLED

#define SCM_DEFER_INTS		\
do				\
{				\
  SCM_CHECK_NOT_DISABLED;	\
  scm_ints_disabled = 1;	\
}				\
while (0)


#define SCM_ALLOW_INTS_ONLY	\
do				\
{				\
  scm_ints_disabled = 0;	\
}				\
while (0)


#define SCM_ALLOW_INTS		\
do				\
{				\
  SCM_CHECK_NOT_ENABLED;	\
  scm_ints_disabled = 0;	\
  SCM_ASYNC_TICK;		\
}				\
while (0)


#define SCM_REDEFER_INTS	\
do				\
{				\
  ++scm_ints_disabled;		\
}				\
while (0)


#define SCM_REALLOW_INTS	\
do				\
{				\
  SCM_CHECK_NOT_ENABLED;	\
  --scm_ints_disabled;		\
  if (!scm_ints_disabled)	\
    SCM_ASYNC_TICK;		\
}				\
while (0)


/* Ints must be active.  longjmp possible. */
#define SCM_INTS_ENABLED 		char * scm_int_flavor __attribute__((unused)) = "enabled" 

/* Ints may or may not be active.  Longjmp not possible if ints disabled.
 * If ints are active on entry, lonjmp is possible (via in interrupt).
 */
#define SCM_INTS_NESTED			char * scm_int_flavor __attribute__((unused)) = "nested"

/* Ints must not be active.  Longjmp not possible. */
#define SCM_INTS_DISABLED		char * scm_int_flavor __attribute__((unused)) = "disabled"

/* Ints may or may not be active.  Longjmp not possible if ints disabled.
 * If ints are active on entry, lonjmp is possible (via a signal).
 */
#define SCM_INTS_INDIFFERENT		char * scm_int_flavor __attribute__((unused)) = "indifferent"

/* Not declared yet. */
#define SCM_INTS_UNKNOWN		char * scm_int_flavor __attribute__((unused)) = "unknown"



/************************************************************************
 * Macros for Generating .x Files
 */

#ifndef SCM_MAGIC_SNARFER
#define SCM_PROC(SYMBOL_NAME, STR, REQ, OPT, VAR, CFN)  \
	static SCM SYMBOL_NAME;
#define SCM_PROC1(SYMBOL_NAME, STR, TYPE, CFN)  \
	static SCM SYMBOL_NAME;
#else
#define SCM_PROC(SYMBOL_NAME, STR, REQ, OPT, VAR, CFN)  \
%%%	(SYMBOL_NAME = \
	 SCM_CAR (scm_intern_symhash ((STR), \
				 (scm_make_gsubr ((STR), \
						  (REQ), (OPT), (VAR), (SCM(*)())(CFN), 1))))) @@@
#define SCM_PROC1(SYMBOL_NAME, STR, TYPE, CFN)  \
%%%	(SYMBOL_NAME = \
	 SCM_CAR (scm_intern_symhash ((STR), \
				 (scm_make_subr ((STR), (TYPE), (SCM(*)())(CFN), 1))))) @@@
#endif

#ifndef SCM_MAGIC_SNARFER2
#define SCM_PROCEDURE(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_PROCEDURE(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (SCM_CDR (scm_intern_symhash ((SCHEME_NAME), SCM_UNDEFINED))) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_SYMBOL(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_SYMBOL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (SCM_CAR (scm_intern_symhash ((SCHEME_NAME), SCM_UNDEFINED))) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_KEYWORD(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_KEYWORD(C_NAME, SCHEME_NAME) \
%%%	C_NAME = \
	  scm_permanent_object \
	    (scm_symbol_to_keyword (SCM_CAR (scm_intern_symhash ((SCHEME_NAME), SCM_UNDEFINED)))) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_EXTERN_SYMBOL(c_name, scheme_name) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_EXTERN_SYMBOL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (SCM_CAR (scm_intern_symhash ((SCHEME_NAME), SCM_UNDEFINED))) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_STRING(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_STRING(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_makfromstr0 (SCHEME_NAME)) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_EXTERN_STRING(c_name, scheme_name) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_EXTERN_STRING(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_makfromstr0 (SCHEME_NAME)) @@@
#endif


#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_GLOBAL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_intern_symhash ((SCHEME_NAME), SCM_BOOL_F)) @@@
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_EXTERN_GLOBAL(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_EXTERN_GLOBAL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_intern_symhash ((SCHEME_NAME), SCM_BOOL_F)) @@@
#endif


#ifndef SCM_MAGIC_SNARFER
#define SCM_CONST_LONG(C_NAME, SCHEME_NAME,VALUE) \
	static SCM C_NAME = SCM_BOOL_F
#else
#define SCM_CONST_LONG(C_NAME, SCHEME_NAME,VALUE) \
%%%	(C_NAME = \
	 scm_permanent_object (scm_intern_symhash ((SCHEME_NAME), scm_long2num ((VALUE))))) @@@
#endif


#ifndef SCM_MAGIC_SNARFER
#define SCM_CONST_INUM(C_NAME, SCHEME_NAME,VALUE) \
	static SCM C_NAME = SCM_BOOL_F
#else
#define SCM_CONST_INUM(C_NAME, SCHEME_NAME,VALUE) \
%%%	(C_NAME = \
	 scm_permanent_object (scm_intern_symhash ((SCHEME_NAME), SCM_MAKINUM ((VALUE))))) @@@
#endif



#include "systas/libsystas/tags.h"
#include "systas/libsystas/async.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/tags.h"



#endif  /* INCLUDE__LIBSYSTAS__SCM_H */

