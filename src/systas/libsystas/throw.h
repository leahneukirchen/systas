/* throw.h - scheme exception handling decls
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#ifndef INCLUDE__LIBSYSTAS__THROW_H
#define INCLUDE__LIBSYSTAS__THROW_H

#include "systas/libsystas/scm.h"



extern int scm_tc16_jmpbuffer;

struct scm_jmp_buf_and_retval
{
  jmp_buf buf;
  SCM throw_tag;
  SCM retval;
  SCM wind_position;
};

/* Is it a jump buffer?
 */
#define SCM_JMPBUFP(O) (SCM_TYP16(O) == scm_tc16_jmpbuffer)

/* Is it safe to longjmp to this jump buffer?
 */
#define SCM_JBACTIVE(O) (SCM_CAR (O) & (1L << 16L))

/* It is safe to longjmp to this jump buffer.
 */
#define SCM_ACTIVATEJB(O)  (SCM_CAR (O) |= (1L << 16L))

/* It is not safe to longjmp to this jump buffer.
 */
#define SCM_DEACTIVATEJB(O)  (SCM_CAR (O) &= ~(1L << 16L))

/* The "struct scm_jmp_buf_and_retval" object associated with 
 * this Scheme jump buffer:
 */
#define SCM_JBJMPBUF(O) ((struct scm_jmp_buf_and_retval *)SCM_CDR (O) )


/* Local variable declarations for a function that catches 
 * exceptions.
 */
#define SCM_CATCH_LOCALS   struct scm_jmp_buf_and_retval scm_jbr; \
			   SCM scm_catch_jmpbuf; \
			   SCM scm_throw_tag; \
			   SCM scm_throw_args

/* Initialization for a function that catches exceptions.
 */
#define SCM_CATCH_INIT(TAG)  { \
				 scm_catch_jmpbuf = scm_make_jmpbuf (&scm_jbr); \
				 (scm_dynwinds \
				  = (scm_jbr.wind_position \
				     = scm_acons (TAG, scm_catch_jmpbuf, scm_dynwinds))); \
			     }

/* How to setjmp in a function that catches exceptions.
 * If this setjmp returns a non-0 value, an exception
 * happened.
 */
#define SCM_CATCH_SETJMP() setjmp (scm_jbr.buf)

/* Begin the protected code with this, after 
 * calling SCM_CATCH_SETJMP.
 */
#define SCM_CATCH_BODY_BEGIN SCM_ACTIVATEJB (scm_catch_jmpbuf);

/* End the protected code with this.
 */
#define SCM_CATCH_BODY_END  { \
			      SCM_REDEFER_INTS; \
			      SCM_DEACTIVATEJB (scm_catch_jmpbuf); \
			      scm_dynwinds = SCM_CDR (scm_dynwinds); \
			      SCM_REALLOW_INTS; \
			    }

/* If an exception occurs, begin the exception handling
 * code with this.
 */
#define SCM_UNWIND 	      { \
				  SCM_REDEFER_INTS; \
				  SCM_DEACTIVATEJB (scm_catch_jmpbuf); \
				  scm_dynwinds = SCM_CDR (scm_dynwinds); \
				  SCM_REALLOW_INTS; \
				  scm_throw_args = scm_jbr.retval; \
				  scm_throw_tag = scm_jbr.throw_tag; \
				  scm_jbr.throw_tag = SCM_EOL; \
				  scm_jbr.retval = SCM_EOL; \
			      }

/* This returns the tag that was thrown.
 */
#define SCM_THROW_TAG		(scm_throw_tag)

/* This returns a list of arguments to the exception.
 */
#define SCM_THROW_ARGS		(scm_throw_args)


/* Local variable declarations for a function that posts
 * an exception handler that returns from a call to `handle'
 */
#define SCM_HANDLES_LOCALS \
			   SCM scm_handler; \
			   SCM scm_acons_pair

/* Initialization for a function that posts a `handles'
 * exception handler.
 */
#define SCM_HANDLES_INIT(TAG,HANDLER) \
			       { \
				   scm_handler = HANDLER; \
				   scm_acons_pair = scm_cons (TAG, SCM_BOOL_F); \
				   scm_dynwinds = scm_cons (scm_acons_pair, scm_dynwinds); \
			       }

/* Begin the code protected by a `handles' handler, after 
 * calling SCM_HANDLES_INIT
 */
#define SCM_HANDLES_BODY_BEGIN		SCM_CDR (scm_acons_pair) = scm_handler

/* End the protected code with this.
 */
#define SCM_HANDLES_BODY_END { \
				 SCM_REDEFER_INTS; \
				 SCM_CDR (scm_acons_pair) = SCM_BOOL_F; \
				 scm_dynwinds = SCM_CDR (scm_dynwinds); \
				 SCM_REALLOW_INTS; \
			     }



/* automatically generated __STDC__ prototypes */
extern SCM scm_make_jmpbuf (struct scm_jmp_buf_and_retval * jmpbuf);
extern SCM scm_catch (SCM tag, SCM thunk, SCM handler);
extern SCM scm_handles (SCM tag, SCM thunk, SCM handler);
extern SCM scm_ithrow (SCM key, SCM args, int noreturn);
extern SCM scm_throw (SCM key, SCM args);
extern SCM scm_handle (SCM key, SCM args);
extern void scm_init_throw (void);
#endif  /* INCLUDE__LIBSYSTAS__THROW_H */
