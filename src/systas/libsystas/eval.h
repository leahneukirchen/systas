/* eval.h - decls for scheme evaluation
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__EVAL_H
#define INCLUDE__LIBSYSTAS__EVAL_H

#include "systas/libsystas/scm.h"



enum scm_debug_frame_type
{
  scm_eval_frame,
  scm_eval_step_frame,
  scm_eval_tailcall_frame,
  scm_eval_exception_frame,
  scm_eval_return_frame,
  scm_callout_frame,
  scm_callout_step_frame,
  scm_callout_exception_frame,
  scm_callout_return_frame,
};

struct scm_debug_frame
{
  /* both ceval and apply
   */
  enum scm_debug_frame_type type;
  struct scm_debug_frame * prev;
  int prev_is_direct;
  int * retbrk_flag_ptr;
  int can_catch_errors;
  SCM value;
  SCM throw_tag;
  SCM throw_args;

  /* for ceval only
   */
  int * stepbrk_flag_ptr;
  SCM exp;
  SCM env;
  SCM debug_return;

  /* for apply only
   */
  SCM proc;
  SCM arg1st;
  SCM args;
};



/* ilocs
 *
 * Ilocs are relative pointers into local environments.  In code, a
 * reference to a local variable is replaced by an iloc.
 */

/* Is the non-immediate n a cons pair with an iloc in the car?
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)

/* The depth, in frames, of the variable referenced by this iloc.
 */
#define SCM_IFRAME(n) 		((int)((SCM_ICDR-SCM_IFRINC)>>8) & ((int)(n)>>8))

/* The offset within its frame, in cons pairs, of the variable
 * referenced by this iloc.
 */
#define SCM_IDIST(n) 		(((unsigned long)(n))>>20)

/* The iloc for the first variable in the innermost environment 
 * frame.
 */
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)

/* What to add to an iloc to obtain the iloc for the next variable
 * in the same frame.
 */
#define SCM_IDINC		(0x00100000L)

/* What to add to an iloc to obtain the iloc for the next frame.
 */
#define SCM_IFRINC		(0x00000100L)

/* A mask that selects the bits that are the address of a variable
 * within its frame.
 */
#define SCM_IDSTMSK		(-SCM_IDINC)

/* The bit within an iloc that indicates that the addressed
 * variable is a `rest' variable -- that its value is the
 * entire list of values beginning at its IDINC within its 
 * frame.
 */
#define SCM_ICDR		(0x00080000L)

/* Is this iloc a `rest' variable?
 */
#define SCM_ICDRP(n) 		(SCM_ICDR & (n))

/* Return 1 if ENV is a top-level environment, 0 otherwise.
 */
#define SCM_IS_TOP_LEVEL(ENV)	(   (SCM_EOL == (ENV)) \
				 || (SCM_BOOL_T == scm_procedure_p (SCM_CAR (ENV))))

/* glocs
 *
 * glocs are absolute pointers to global variable vcells.
 * A vcell is a cons-pair which holds the name of the variable
 * in the car, and the value in the cdr.
 *
 * A resolved global variable reference in the car position
 * of a list in code is replace by a gloc.
 */

/* Is the non-immediate n a cons pair with an gloc in the car?
 */
#define SCM_GLOCP(X)	(SCM_TYP3(x) == 1)

/* Access to the value of the global variable addressed by a gloc.
 */
#define SCM_GLOC_VAL(x) (SCM_CDR((x)-1L))

/* Access to the variable name hint of the global variable addressed
 * by a gloc.  This is not necessarily the variable name (that is
 * stored in the key of a hash-table) but usually is.
 */
#define SCM_GLOC_SYM(x) (SCM_CAR((x)-1L))



/* automatically generated __STDC__ prototypes */
extern SCM scm_denoted_variable (SCM name, SCM env);
extern SCM scm_eval_x (SCM obj);
extern SCM scm_eval (SCM obj);
extern SCM scm_eval2 (SCM obj, SCM env_thunk);
extern SCM scm_eval3 (SCM obj, int copyp, SCM env);
extern SCM scm_eval_environment_x (SCM exp, SCM env);
extern SCM scm_evalcar (SCM x, SCM env);
extern SCM scm_apply (SCM fn, SCM args);
extern SCM scm_apply_eval_helper (SCM fn, SCM arg1st, SCM args);
extern SCM scm_apply3 (SCM proc, SCM arg1st, SCM args, struct scm_debug_frame * prev);
extern SCM scm_nconc2last (SCM lst);
extern SCM scm_call_stepping (SCM thunk);
extern SCM scm_raw_backtrace (void);
extern SCM scm_set_stepbrk_flag (SCM frame_id, SCM value);
extern SCM scm_set_retbrk_flag (SCM frame_id, SCM value);
extern void scm_init_eval (void);
#endif  /* INCLUDE__LIBSYSTAS__EVAL_H */
