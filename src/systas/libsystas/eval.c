/* eval.c - scheme evaluation
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
#include "hackerlab/bugs/panic.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/async.h"
#include "systas/libsystas/scheme.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/macros.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/variable.h"



/* Are two objects eqv?  This macro is for use by eval and
 * so should be fast.
 */
#ifdef SCM_FLOATS
#define SCM_CHECK_EQVISH(A,B) 	(((A) == (B)) || ((SCM_BOOL_F != scm_eqv_p ((A), (B)))))
#else
#define SCM_CHECK_EQVISH(A,B) 	((A) == (B))
#endif

/************************************************************************
 *(h0 "Evaluation")
 *
 * "Source forms" are list structures which obey the rules of Systas
 * Scheme syntax.
 * 
 * "Evaluation" is the operation by which source forms are interpreted
 * by the system to make a series of calls to primitive functions.
 * 
 * Evaluation has the job of resolving variable references, interpreting
 * special forms, and carrying out function calls.   This section
 * describes the interface to those capabilities.
 * 
 * An introduction to Scheme or the current "Revised Report on Scheme"
 * would be a good place to read about evaluation in more detail.  This
 * overview is intended just as an informal reference.
 */
/*(menu)
 */


/************************************************************************
 *(h1 "Understanding How Evaluation Works in Systas Scheme")
 *
 * When an expression is read for evaluation, the result is an
 * ordinary structure of lists, symbols and other objects.  Evaluation
 * begins by making a copy of the list and structure of the expression
 * and the copy is then interpreted directly.  The source form is
 * copied for two reasons.  First, so that if the original source form
 * is later modified, the modifications have no effect on evaluation.
 * Second, so that the evaluator is free to modify the copy without
 * effecting the original source form.  It is possible to write
 * self-modifying code in Systas Scheme (by using low-level macros),
 * but not by modifying the source forms passed to the evaluator.
 *
 * As evaluation proceeds, the copied source form is modified by the
 * evaluator.  Calls to macros are replaced by their expansion and
 * variable names (symbols) are replaced by absolute or relative
 * pointers to the variables they address (absolute pointers for
 * global variables, relative pointers for local variables).  When the
 * same expression is evaluated more than once (for example, the
 * expressions in the body of a procedure), these modifications speed
 * up subsequent evaluations of the same source forms.
 *
 * Basic Scheme syntax (`define', `lambda', `let', `set!', etc.) is
 * implemented by macros that expand to a kind of list-structured
 * virtual machine code which the evaluator can interpret rapidly.
 *
 * Environments are represented as a list of lists.  The environment
 * is a list of frames (lexical scopes) and each frame is a list of
 * the variables and values of the variables in that scope.  For
 * example, in the body of the function:
 *
 *	(lambda (v1 v2 . rest) . body)
 *
 * The lexical environment is:
 *
 *	( ;; One frame of the environment:
 *	  ;; 
 *	  (   (v1 v2 . rest)		;; The variables in this frame.
 *	      value-of-v1		;; The values of those variables.
 *	      value-of-v2 
 *	   . value-of-rest)
 *
 *	 ;; Frames for the enclosing scopes.
 *	 ;;
 *	 . outer-frames-of-environment)
 *
 * The outermost frame of an environment may be a procedure instead of
 * a list.  In that case, the procedure is a top-level lookup function
 * that maps free variable names to first-class variable objects.
 */

/************************************************************************
 *(h1 "Specifying Top-Level Environments for Evaluation")
 *
 * The procedure `eval2' permits programs to specify the mapping
 * between the names of free variables (also known as "global
 * variables" and first-class variable objects.  This is done by means
 * of a procedure called the "top-level environment mapping", as in
 * this example:
 *
 *   ;; tiny-top-level defines a top-level environment.
 *   ;; It maps variable names to the variables they 
 *   ;; stand for.
 *   ;;
 *   (define tiny-top-level
 *     (let ((+-var (make-variable + '+))
 *           (x-var (make-variable 1 'x))
 *           (y-var (make-variable 2000 'y))
 *           (define-var (make-variable define 'define))
 *           (quote-var (make-variable quote 'quote))
 *           (extras ()))
 *       (lambda (name defining?)
 *         (case name
 *           ((+)       +-var)
 *           ((x)       x-var)
 *           ((y)       y-var)
 *           ((define)  define-var)
 *           ((quote)   quote-var)
 *           (else      (or (assq-ref extras name)
 *                          (if (not defining?)
 *                              (error "undefined variable" name)
 *                              (begin
 *                                (set! extras
 *                                      (cons
 *                                        (cons name
 *                                              (make-variable name #f))
 *                                            extras))
 *                                (assq-ref extras name)))))))))
 *                                           
 *   
 *   (eval2 '(+ x y) tiny-top-level) 
 *   => 2001
 *   
 *   (eval2 'q tiny-top-level)
 *   ;;; ERROR: undefined variable q
 *   ;;; ABORT
 *   ;;;
 *   
 *   (eval2 '(define q 420) tiny-top-level)
 *   => ()
 *   
 *   (eval2 'q tiny-top-level)
 *   => 420
 *
 * It is important to understand that the environment function passed
 * to `eval2' is not called every time a particular global variable
 * reference is evaluated.  Within a closure, each global variable
 * reference is normally resolved at most once, the first time it is
 * evaluated.  At that time, the resolution is cached.  If the same
 * variable reference is evaluated in the future, the costly lookup
 * using an environment function is avoided.  (See xref:"Understanding
 * How Evaluation Works in Systas Scheme".)
 * 
 * This can lead to some behaviors that might seem odd at first:
 * 
 * 	;; Create two variables with the same name, but
 * 	;; different values:
 * 	;;
 * 	(define var1 (make-variable 'var1-value 'xyzzy))
 * 	(define var2 (make-variable 'var2-value 'xyzzy))
 * 	
 * 	(define the-var var1)
 * 	
 * 	;; This trivial env-fn maps all "xyzzy" to
 * 	;; whichever variable is bound to the-var:
 * 	;;
 * 	(define (an-env-fn name define?)
 * 	  (if (eq? name 'xyzzy)
 * 	      the-var
 * 	      ;; otherwise, use the normal binding:
 * 	      ;;
 * 	      (module-variable (current-module) name)))
 * 	
 * 	;; Use eval2 to capture that env-fn in some closures:
 * 	;;
 * 	(define sample-fn (eval2 '(lambda () xyzzy) an-env-fn))
 * 	(define sample-fn2 (eval2 '(lambda () xyzzy) an-env-fn))
 * 	
 * 	;; Evaluating the resulting function resolves xyzzy to var1,
 * 	;; and caches that result:
 *	;;
 * 	(sample-fn)
 * 	=> 'var1-value
 * 	
 * 	;; Even if `the-var' is changed, since `sample-fn' has
 * 	;; executed at least once, var1 is still used:
 * 	;;
 * 	(set! the-var var2)
 * 	(sample-fn)
 * 	=> 'var1-value
 * 	
 * 	;; On the other hand, sample-fn2 has never been executed.
 * 	;; If it is run now, xyzzy will be looked up and resolved to
 * 	;; var2:
 * 	;;
 * 	(sample-fn2)
 * 	=> 'var2-value
 * 
 * You can flush the cache for any particular variable name using this
 * function `unhash-names':
 *
 *	(unhash-names '(xyzzy))
 *	(sample-fn)
 *	=> 'var2-value
 *
 */


SCM_SYMBOL (sym_eval, "eval");
SCM_SYMBOL (s_apply, "apply");
SCM_SYMBOL (s_magic_ceval_symbol, " !#@$?!!magic-ceval-symbol");
static SCM s_eval;
SCM_SYMBOL (s_eval_step, "eval-step");
SCM_SYMBOL (s_eval_tailcall, "eval-tailcall");
SCM_SYMBOL (s_eval_exception, "eval-exception");
SCM_SYMBOL (s_eval_return, "eval-return");
SCM_SYMBOL (s_callout, "callout");
SCM_SYMBOL (s_callout_step, "callout-step");
SCM_SYMBOL (s_callout_exception, "callout-exception");
SCM_SYMBOL (s_callout_return, "callout-return");

SCM_GLOBAL (g_step_breakpoint, "step-breakpoint");
SCM_GLOBAL (g_tail_breakpoint, "tail-breakpoint");
SCM_GLOBAL (g_exception_breakpoint, "exception-breakpoint");
SCM_GLOBAL (g_return_breakpoint, "return-breakpoint");
SCM_GLOBAL (g_callout_breakpoint, "callout-breakpoint");
SCM_GLOBAL (g_callout_exception_breakpoint, "callout-exception-breakpoint");
SCM_GLOBAL (g_callout_return_breakpoint, "callout-return-breakpoint");
SCM_GLOBAL (g_debug_on_exception_flag, "debug-on-exception");

/* stepbrk_flag
 * 
 * When set, ceval begins evaluation in single step mode.
 */
static int stepbrk_flag = 0;


static SCM scm_i_apply;
#if 0
static SCM scm_tailbrk (void);
static SCM scm_stepbrk (void);
static SCM scm_retbrk (struct scm_debug_frame * debug_info);
static SCM scm_errbrk (struct scm_debug_frame * debug_info);
static SCM scm_apply_stepbrk (void);
static SCM scm_apply_retbrk (struct scm_debug_frame * debug_info);
static SCM scm_apply_errbrk (struct scm_debug_frame * debug_info);
#endif


/* Signal an error of type eval-error with arguments:
 *
 * 	message subr argument-value expression environment
 */
static void
scm_everr (SCM exp, SCM env __attribute__ ((unused)), SCM arg, SCM msg, SCM subr)
{
  scm_err_escape (0, scm_eval_error,
		  msg,
		  (subr ? subr : scm_unknown_procedure), 
		  arg, 
		  exp, 
		  SCM_UNDEFINED);
}




/****************************************************************
 * Support Routines for Evaluating Variable References
 */

/* scm_ilookup_f
 * 
 * Return a pointer to the heap cell addressed by iloc.
 */
static SCM *
scm_ilookup_f (SCM iloc, SCM env)
{
  SCM_INTS_ENABLED;
  int ir;

  for (ir = SCM_IFRAME (iloc); 0 != ir; --ir)
    env = SCM_CDR (env);

  env = SCM_CAR (env);

  for (ir = SCM_IDIST (iloc); 0 != ir; --ir)
    env = SCM_CDR (env);

  if (SCM_ICDRP (iloc))
    return &SCM_CDR (env);

  return &SCM_CAR (SCM_CDR (env));
}


/* scm_ilookup_f
 * 
 * Return a pointer to the heap cell addressed by iloc.
 *
 * Supposedly this optimizes scm_ilookup_f.
 */
static SCM *
scm_ilookup (SCM iloc, SCM env)
{
  SCM_INTS_ENABLED;
  SCM saved_env;

  saved_env = env;
  switch (SCM_IFRAME (iloc))
    {
    default:
      return scm_ilookup_f (iloc, env);

    case 6:
      env = SCM_CDR (env);
    case 5:
      env = SCM_CDR (env);
    case 4:
      env = SCM_CDR (env);
    case 3:
      env = SCM_CDR (env);
    case 2:
      env = SCM_CDR (env);
    case 1:
      env = SCM_CDR (env);
    case 0:
      break;
    }

  env = SCM_CAR (env);

  switch (SCM_IDIST (iloc))
    {
    default:
      return scm_ilookup_f (iloc, saved_env);

    case 6:
      env = SCM_CDR (env);
    case 5:
      env = SCM_CDR (env);
    case 4:
      env = SCM_CDR (env);
    case 3:
      env = SCM_CDR (env);
    case 2:
      env = SCM_CDR (env);
    case 1:
      env = SCM_CDR (env);
    case 0:
      break;
    }

  if (SCM_ICDRP (iloc))
    return &SCM_CDR (env);

  return &SCM_CAR (SCM_CDR (env));
}


/* scm_lookupcar
 * 
 * Given a cons pair with a symbol in the car, return a pointer to the
 * location it addresses.
 *
 * Modify the cons pair to contain an iloc or gloc that addresses the
 * same location so that subsequent lookups at the same point in the
 * code being evaluated will run faster.
 */
static SCM *
scm_lookupcar (SCM vloc, SCM genv, int no_unbound_error)
{
  SCM_INTS_ENABLED;
  SCM env;
  SCM *al;
  SCM fl;
  SCM var;
  SCM iloc;

  iloc = SCM_ILOC00;
  var = SCM_CAR (vloc);

  for (env = genv; !SCM_IS_IMMEDIATE (env); env = SCM_CDR (env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR (env)))
	break;
      al = &SCM_CAR (env);
      for (fl = SCM_CAR (*al); !SCM_IS_IMMEDIATE (fl); fl = SCM_CDR (fl))
	{
	  if (SCM_NCONSP (fl))
	    {
	      if (fl == var)
		{
		  SCM_CAR (vloc) = iloc + SCM_ICDR;
		  return &SCM_CDR (*al);
		}
	      else
		break;
	    }
	  al = &SCM_CDR (*al);
	  if (SCM_CAR (fl) == var)
	    {
#ifndef SCM_RECKLESS		/* letrec inits to SCM_UNDEFINED */
	      if (SCM_UNBNDP (SCM_CAR (*al)))
		{
		  env = SCM_EOL;
		  goto errout;
		}
#endif
	      SCM_CAR (vloc) = iloc;
	      return &SCM_CAR (*al);
	    }
	  iloc += SCM_IDINC;
	}
      iloc = (~SCM_IDSTMSK) & (iloc + SCM_IFRINC);
    }

  {
    SCM top_thunk;
    SCM vcell;

    if (!SCM_IS_IMMEDIATE(env))
      {
	top_thunk = SCM_CAR(env);	/* env now refers to a top level env thunk */
	env = SCM_CDR (env);
      }
    else
      top_thunk = SCM_BOOL_F;

    {
      int was_stepbrk_flag;
      was_stepbrk_flag = stepbrk_flag;
      stepbrk_flag = 0;
      vcell = scm_symbol_to_vcell (var, top_thunk, SCM_BOOL_F);
      stepbrk_flag = was_stepbrk_flag;
    }
      
    if (vcell == SCM_BOOL_F)
      goto errout;
    else
      var = vcell;
  }

#ifndef SCM_RECKLESS
  if ((SCM_EOL != env) || SCM_UNBNDP (SCM_CDR (var)))
    {
      var = SCM_CAR (var);
    errout:
      if (no_unbound_error)
	return 0;
      scm_everr (vloc, genv, var,
		      ((SCM_EOL == env)
		       ? scm_unbound_variable
		       : scm_damaged_environment),
		      sym_eval);
    }
#endif
  SCM_CAR (vloc) = var + 1;
  return &SCM_CDR (var);
}


/* scm_unmemocar
 * 
 * Given a cons pair which contains an iloc or gloc in the car, replace
 * the car with the name-hint of the same variable.
 */
static SCM 
scm_unmemocar (SCM form, SCM env)
{
  SCM_INTS_UNKNOWN;
  SCM c;

  if (SCM_IS_IMMEDIATE (form))
    return form;
  c = SCM_CAR (form);
  if (1 == (c & 7))
    SCM_CAR (form) = SCM_CAR (c - 1);
  else if (SCM_ILOCP (c))
    {
      SCM ir;
      for (ir = SCM_IFRAME (c); ir != 0; --ir)
	env = SCM_CDR (env);
      env = SCM_CAR (SCM_CAR (env));
      for (ir = SCM_IDIST (c); ir != 0; --ir)
	env = SCM_CDR (env);
      SCM_CAR (form) = SCM_ICDRP (c) ? env : SCM_CAR (env);
    }
  return form;
}


/************************************************************************
 *(h1 "Eval-Related Procedures")
 * 
 * 
 * 
 */

/*(c denoted-variable)
 * (denoted-variable name env)
 * 
 * Return the first-class variable denoted by `name' in the
 * environment `env'.
 * 
 */
SCM_PROC (s_denoted_variable, "denoted-variable", 2, 0, 0, scm_denoted_variable);
SCM
scm_denoted_variable (SCM name, SCM env)
{
  SCM_INTS_ENABLED;
  SCM c;
  SCM * lloc;

  SCM_ASSERT (scm_is_symbol (name), name, scm_arg1, s_denoted_variable);

  SCM_NEWCELL (c);
  SCM_DEFER_INTS;
  SCM_CAR (c) = name;
  SCM_CDR (c) = SCM_EOL;
  SCM_ALLOW_INTS;

  lloc = scm_lookupcar (c, env, 1);
  if (!lloc)
    return scm_builtin_variable (name);
  switch (SCM_TYP3 (c))
    {
    default:
      panic ("lookupcar oddity in scm_denoted_variable");
      return SCM_BOOL_F;
    case 1:
      return scm_construct_variable (scm_global_variable, name, SCM_CAR (c) - 1);
    case 4:
      {
	SCM iloc;
	SCM frame;
	SCM pair;
	enum scm_variable_type type;

	iloc = SCM_CAR (c);
	frame = SCM_CAR (scm_list_tail (env, SCM_MAKINUM (SCM_IFRAME (iloc))));
	pair = scm_list_tail (frame, SCM_MAKINUM (SCM_IDIST (iloc)));
	if (SCM_ICDRP (iloc))
	  type = scm_indirect_d_variable;
	else
	  {
	    pair = SCM_CDR (pair);
	    type = scm_indirect_a_variable;
	  }
	return scm_construct_variable (type, name, pair);
      }
    }
}


/****************************************************************
 * The Heart of `eval'.
 */

/* Add a frame to an environment.
 *
 * VARS		- a list of variables bound here, in reverse order.
 */
#define SCM_EXTEND_ENV(VARS,VALUES,ENV) 	scm_acons ((VARS), (VALUES), (ENV))

/* Eval x and return its value.
 */
#define SCM_EVAL(x, env, db) 	(SCM_IS_IMMEDIATE(x)?(x):scm_ceval((x), (env), (db)))

/* Evaluate immediate value x; return its value.
 */ 
#define SCM_EVALIMP(x, env)	(SCM_ILOCP(x)?*scm_ilookup((x), (env)):x)

/* Evaluate the car of x, possibly modifying it;
 * return its value.
 */ 
#define SCM_EVALCAR(x, env, db) (SCM_NCELLP (SCM_CAR(x)) \
				 ? (SCM_IS_IMMEDIATE(SCM_CAR(x)) \
				    ? SCM_EVALIMP(SCM_CAR(x), env) \
				    : SCM_GLOC_VAL(SCM_CAR(x))) \
				 : (SCM_SYMBOLP (SCM_CAR(x)) \
				    ? *scm_lookupcar((x), (env), 0) \
				    : scm_ceval(SCM_CAR(x), (env), (db))))



/* EVAL_ARGS
 * 
 * Evaluate a list of expressions in "x",  constructing
 * a reverse-order list of values in "args"
 */
#define EVAL_ARGS do {  \
			  SCM * lloc;  \
			  \
			  args = SCM_EOL;  \
			  lloc = &args;  \
			  \
			  while (!SCM_IS_IMMEDIATE (x))  \
			  {  \
			       *lloc = scm_cons (SCM_EVALCAR (x, env, &debug_info), SCM_EOL);  \
				 x = SCM_CDR (x);  \
				   lloc = &SCM_CDR (*lloc);  \
			  }  \
		      } while (0)



/* Perhaps call the break-point handler for returns from ceval.
 *
 * The value about to be returned is in x.
 */
#define SCM_RETBRK_CHECK \
	do \
	  { \
	    if (!local_retbrk_flag) \
	      { \
		if (local_errbrk_flag) \
		  SCM_CATCH_BODY_END; \
		scm_root->debug_info = debug_info.prev; \
	      } \
	    else \
	      { \
		  SCM answer; \
		  debug_info.type = scm_eval_return_frame; \
		  /* debug_info.exp already set */ \
		  /* debug_info.env already set */ \
		  answer = scm_retbrk (&debug_info); \
		  if (local_errbrk_flag) \
		    SCM_CATCH_BODY_END; \
		  scm_root->debug_info = prev; \
		  return answer; \
	      } \
	  } while (0)


/* scm_make_top_level_env
 * 
 * Construct a new top-level environment.
 *
 * If thunk is not nil, it should be a top-level environment mapping.
 * (See "Understanding How Evaluation Works in Systas Scheme" in
 * "eval.c".)
 */
static SCM
scm_make_top_level_env (SCM thunk)
{
  SCM_INTS_UNKNOWN;

  if (SCM_IS_IMMEDIATE(thunk))
    return SCM_EOL;
  else
    return scm_cons(thunk, (SCM)SCM_EOL);
}


/* scm_ceval
 * 
 * Evaluate "x" in environment "env".  Return its value.
 */
static SCM
scm_ceval (SCM x, SCM env, struct scm_debug_frame * prev)
{
  SCM_INTS_ENABLED;
#if 0
  SCM_CATCH_LOCALS;
  int local_stepbrk_flag;
  int local_retbrk_flag;
  int local_errbrk_flag;
#endif
  struct scm_debug_frame debug_info;
  SCM proc;
  SCM arg1st;
  SCM arg2nd;

  SCM_CHECK_STACK;

#if 0
  /* On entry, we link debug_info to prev and set scm_root->debug_info.
   * On exit, that has to be undone (RETBRK_CHECK, and returns from errbrk, tailbrk, stepbrk).
   *
   * On entry, we may set a catch handler.
   * On exit, we may have to SCM_CATCH_BODY_END
   *     (RETBRK_CHECK, and returns from tailbrk, stepbrk).
   */

  local_errbrk_flag = (SCM_BOOL_F != SCM_CDR (g_debug_on_exception_flag)) || stepbrk_flag;
  local_stepbrk_flag = stepbrk_flag;
  /* local_retbrk_flag is set below */

  if (prev)
    {
      debug_info.prev = prev;
      debug_info.prev_is_direct = 1;
      scm_root->debug_info = &debug_info;
    }
  else
    {
      debug_info.prev = scm_root->debug_info;
      debug_info.prev_is_direct = 0;
      scm_root->debug_info = &debug_info;
    }

  debug_info.stepbrk_flag_ptr = &local_stepbrk_flag;
  debug_info.retbrk_flag_ptr = &local_retbrk_flag;
  debug_info.type = scm_eval_frame;
  debug_info.exp = x;
  debug_info.env = env;

  if (!local_errbrk_flag)
    {
      debug_info.can_catch_errors = 0;
    }
  else if (1)
    {
      debug_info.can_catch_errors = 1;
      SCM_CATCH_INIT(SCM_BOOL_T);
      if (SCM_CATCH_SETJMP())
	{
	  SCM answer;
	  SCM_UNWIND;
	  debug_info.type = scm_eval_exception_frame;
	  /* debug_info.exp already set */
	  /* debug_info.env = env already set */
	  debug_info.value = SCM_BOOL_F;
	  debug_info.debug_return = SCM_BOOL_F;
	  debug_info.throw_tag = SCM_THROW_TAG;
	  debug_info.throw_args = SCM_THROW_ARGS;
	  answer = scm_errbrk (&debug_info);
	  scm_root->debug_info = prev;
	  return answer;
	}
      SCM_CATCH_BODY_BEGIN;
    }
#endif
  /* see not_tail_call for: retbrk_flag = scm_stepbrk; */
  goto not_tail_call;

 tail_call_ceval:
  /* x		-- an expression to evaluate; return the value.
   */


#if 0
  debug_info.type = scm_eval_frame;
  debug_info.exp = x;
  debug_info.env = env;
#endif

  /* decrement the count-down timer.
   */
  SCM_ASYNC_TICK;

#if 0
  if (local_retbrk_flag)
    {
      SCM debugger_val;
      debug_info.type = scm_eval_tailcall_frame;
      /* debug_info.exp already set */
      /* debug_info.env already set */
      debug_info.value = SCM_BOOL_F;
      debug_info.debug_return = SCM_BOOL_F;
      debugger_val = scm_tailbrk ();
      if (!SCM_IS_IMMEDIATE (debugger_val) && SCM_CONSP (debugger_val))
	{
	  if (local_errbrk_flag)
	    SCM_CATCH_BODY_END;
	  scm_root->debug_info = prev;
	  return SCM_CAR (debugger_val);
	}
      debug_info.type = scm_eval_frame;
    }
  else
    {
    not_tail_call:
      /* offer to step
       */
      if (local_stepbrk_flag)
	{
	  SCM debugger_val;
	  local_retbrk_flag = 1;
	  debug_info.type = scm_eval_step_frame;
	  /* debug_info.exp already set */
	  /* debug_info.env already set */
	  debug_info.value = SCM_BOOL_F;
	  debug_info.debug_return = SCM_BOOL_F;
	  debugger_val = scm_stepbrk ();
	  if (!SCM_IS_IMMEDIATE (debugger_val) && SCM_CONSP (debugger_val))
	    {
	      if (local_errbrk_flag)
		SCM_CATCH_BODY_END;
	      scm_root->debug_info = prev;
	      return SCM_CAR (debugger_val);
	    }
	  debug_info.type = scm_eval_frame;
	}
      else
	local_retbrk_flag = 0;
    }
#else
 not_tail_call:
#endif

  /****************************************************************
   * Evaluating a non-immediate object.
   */
  switch (SCM_TYP7 (x))
    {
    default:
      proc = x;
    badfun:
      scm_everr (x, env, proc, scm_wrong_type_to_apply, sym_eval);

      /****************************************************************
       * NEXT SEVERAL switch CASES:
       *
       * Evaluating a non-immediate object.
       * Which is not a cons pair.
       *
       * We are dispatching on the 7-bit tag in the car of its header.
       ****************************************************************
       */
    case scm_tc7_smob:
      if (SCM_VARIABLEP (x))
	{
	  /* first-class variables
	   *
	   * x	-- the variable whose value to return.
	   */
	  if (SCM_UD_VARIABLEP (x))
	    scm_everr (x, env, x, scm_unbound_variable, sym_eval);
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  return SCM_VARIABLE_VALUE (x);
	}

      /* else fall through
       */
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_string:
    case scm_tc7_substring:
    case scm_tc7_subsymbol:
    case scm_tc7_static_string:
    case scm_tc7_static_substring:
    case scm_tc7_static_subsymbol:
    case scm_tc7_cclo:
    case scm_tcs_closures:
    case scm_tcs_subrs:
      /* self-evaluating objects
       *
       * x	-- the value to return.
       */
#if 0
      SCM_RETBRK_CHECK;
#endif
      return x;

    case scm_tcs_symbols:
      {
	/* variable reference, not in code
	 *
	 * x	-- the variable.
	 */
	x = scm_cons (x, SCM_BOOL_F);
	x = *scm_lookupcar (x, env, 0);

	/* x	-- the variable value to return.
	 */
#if 0
	SCM_RETBRK_CHECK;
#endif
	return x;
      }


      /****************************************************************
       * Evaluating a non-immediate object.
       * Which is a cons pair.  
       * With an immediate value in the CAR.
       *
       * We are dispatching on the 7-bit tag of the immediate value.
       ****************************************************************
       */

    case (127 & SCM_ILOC00):

      /* cached local variable reference in the car 
       * of an application
       *
       * x	-- (iloc ...)
       */
      proc = *scm_ilookup (SCM_CAR (x), env);

      /* cached local variable reference in the car 
       * of an application
       *
       * x	-- (iloc ...)
       * proc	-- the value of the variable; procedure to apply
       */
      if (SCM_IS_IMMEDIATE (proc))
	goto badfun;
#ifndef SCM_RECKLESS
#ifdef SCM_CAUTIOUS
      goto checkargs;
#endif
#endif
      break;


    case scm_tcs_cons_gloc:
      {
	/* cached global variable reference in the car 
	 * of an application
	 *
	 * x	-- (gloc ...)
	 */
	proc = SCM_GLOC_VAL (SCM_CAR (x));

	/* cached global variable reference in the car 
	 * of an application
	 *
	 * x	-- (gloc ...)
	 * proc	-- the value of the variable; procedure to apply
	 */
	if (SCM_IS_IMMEDIATE (proc))
	  goto badfun;

#ifndef SCM_RECKLESS
#ifdef SCM_CAUTIOUS
	goto checkargs;
#endif
#endif
	break;
      }

    case (127 & SCM_IM_QUOTE):
      /* already expanded application of `quote'
       *
       * x	-- the application
       */
      x = SCM_CAR (SCM_CDR (x));

      /* x	-- the quoted value to return.
       */
#if 0
      SCM_RETBRK_CHECK;
#endif
      return x;

    case (127 & SCM_IM_LAMBDA):
      /* already expanded application of `lambda'
       *
       * x	-- the application
       */
      x = scm_make_closure (SCM_CDR (x), env);

      /* x	-- the closure to return.
       */
#if 0
      SCM_RETBRK_CHECK;
#endif
      return x;

    case (127 & SCM_IM_SET):
      {
	SCM * lloc;

	/* already expanded application of `set!'
	 *
	 * x	-- the application
	 */
      set_some_more:
	/* arguments to `set!', plus some junk
	 *
	 * x	-- a list: (junk variable expression ...)
	 */
	x = SCM_CDR (x);
	proc = SCM_CAR (x);

	/* arguments to `set!'
	 *
	 * x	-- a list: (variable expression ...)
	 * proc -- the symbol, gloc, or iloc for the variable to set
	 */
	switch (7 & (int)proc)
	  {
	  case 0:
	    /* arguments to `set!'
	     *
	     * x	-- a list: (variable expression ...)
	     * proc 	-- the symbol name of the variable to set
	     */
	    if (SCM_VARIABLEP (SCM_CAR (x)))
	      {
		if (SCM_UD_VARIABLEP (SCM_CAR (x)))
		  scm_everr (x, env, x, scm_unbound_variable, sym_eval);
		lloc = &SCM_VARIABLE_VALUE (SCM_CAR (x));
	      }
	    else
	      lloc = scm_lookupcar (x, env, 0);
	    break;
	  case 1:
	    /* arguments to `set!'
	     *
	     * x	-- a list: (variable expression ...)
	     * proc 	-- the gloc of the variable to set
	     */
	    lloc = &SCM_GLOC_VAL (proc);
	    break;

	  case 4:
	    /* arguments to `set!'
	     *
	     * x	-- a list: (variable expression ...)
	     * proc 	-- the iloc of the variable to set
	     */
	    lloc = scm_ilookup (proc, env);
	    break;
	  }

	/* arguments to `set!'
	 *
	 * x	-- a list: (variable expression ...)
	 * proc	-- the variable, gloc, or iloc of the variable to set
	 * lloc -- address in which to store the value
	 */
	x = SCM_CDR (x);
	*lloc = SCM_EVALCAR(x, env, &debug_info);
	if (SCM_EOL != SCM_CDR (x))
	  goto set_some_more;

	x = *lloc;
	/* end of `set!'
	 *
	 * x	-- the value to return (value of the last variable set)
	 * proc	-- the variable, gloc, or iloc of the last variable to set
	 * lloc -- address of the value of the last variable
	 */
#if 0
	SCM_RETBRK_CHECK;
#endif
	return x;
      }

    case (127 & SCM_IM_BEGIN):
      {
	/* already expanded application of `begin'
	 *
	 * x	-- (#@begin exp1 exp2 ...)
	 */

      cdrxbegin:
	/* x	-- (junk exp_n exp_n+1 ...)
	 */

	x = SCM_CDR (x);

      begin:
	/* x	-- (exp_n exp_n+1 ...)
	 */

	arg2nd = x;
	/* x	-- (exp_n exp_n+1 ...)
	 * arg2nd	-- (exp_n exp_n+1 ...)
	 */
	while (1)
	  {
	    arg2nd = SCM_CDR (arg2nd);
	    /* x	-- (exp_n exp_n+1 ...)
	     * arg2nd	-- (exp_n+1 ...)
	     */
	    if (SCM_EOL == arg2nd)
	      break;

	    arg1st = x;
	    SCM_EVALCAR (arg1st, env, &debug_info);
	    x = arg2nd;
	    /* x	-- (exp_n+1 ...)
	     * arg2nd	-- (exp_n+1 ...)
	     */
	  }
	
      tail_call_evalcar:
	/* eval the car of a list and return its value.
	 *
	 * x	-- (last-exp)
	 */

	if (SCM_NCELLP (SCM_CAR (x)))
	  {
	    /* last-exp is a cached variable reference or self-evaluating immediate
	     */
	    x = SCM_CAR (x);
	    /* x	-- iloc, gloc, or immediate value
	     */
	    x = (SCM_IS_IMMEDIATE (x) ? SCM_EVALIMP (x, env) : SCM_GLOC_VAL (x));

	    /* x	-- the value to return.
	     */
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    return x;
	  }
	
	if (SCM_SYMBOLP (SCM_CAR (x)))
	  {
	    /* last-exp is an uncached variable reference (a symbol)
	     *
	     * x	-- (variable-name)
	     */
	    x = *scm_lookupcar (x, env, 0);

	    /* x	-- the value to return.
	     */
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    return x;
	  }

	/* last-exp is not a variable reference or self-evaluating immediate.
	 *
	 * x	-- (last-exp)
	 */
	x = SCM_CAR (x);
	goto tail_call_ceval;
      }


    case (127 & SCM_IM_AND):
      {
	/* already expanded application of `and'
	 *
	 * x	-- (#@and exp1 exp2 ...)
	 */
	x = SCM_CDR (x);
	arg1st = x;
	while (1)
	  {
	    arg1st = SCM_CDR (arg1st);
	    /* x	-- (exp1 exp2 ...)
	     * arg1st	-- (exp2 ...)
	     */
	    if (SCM_EOL == arg1st)
	      break;
	    if (SCM_BOOL_F == SCM_EVALCAR(x, env, &debug_info))
	      {
		x = SCM_BOOL_F;
		/* x	-- the value to return.
		 */
#if 0
		SCM_RETBRK_CHECK;
#endif
		return x;
	      }
	    else
	      x = arg1st;
	  }
	/* x	-- (last-exp)
	 */
	goto tail_call_evalcar;
      }
      
    case (127 & SCM_IM_OR):
      {
	/* already expanded application of `or'
	 *
	 * x	-- (#@or exp1 exp2 ...)
	 */
	x = SCM_CDR (x);
	arg1st = x;
	while (1)
	  {
	    arg1st = SCM_CDR (arg1st);
	    /* x	-- (exp_n exp_n+1 ...)
	     * arg1st	-- (exp_n+1 ...)
	     */
	    if (SCM_EOL == arg1st)
	      break;
	    x = SCM_EVALCAR(x, env, &debug_info);
	    if (SCM_BOOL_F != x)
	      {
		/* x	-- the value to return.
		 */
#if 0
		SCM_RETBRK_CHECK;
#endif
		return x;
	      }
	    x = arg1st;
	    /* x	-- (exp_n+1 ...)
	     * arg1st	-- (exp_n+1 ...)
	     */
	  }
	/* x	-- (last-exp)
	 */
	goto tail_call_evalcar;
      }

    case (127 & SCM_IM_IF):
      {
	/* already expanded application of `if'
	 *
	 * x	-- (#@if cond exp1 exp2)
	 */
	x = SCM_CDR (x);
	if (SCM_BOOL_F != SCM_EVALCAR(x, env, &debug_info))
	  x = SCM_CDR (x);
	else
	  {
	    x = SCM_CDR (SCM_CDR (x));
	    if (SCM_IS_IMMEDIATE (x))
	      {
		x = SCM_BOOL_F;
		/* x	-- the value to return.
		 */
#if 0
		SCM_RETBRK_CHECK;
#endif
		return x;
	      }
	  }
	/* `if', after cond checked.
	 *
	 * x	-- exp1 or exp2
	 */
	goto tail_call_evalcar;
      }

    case (127 & SCM_IM_CASE):
      {
	/* already expanded application of `case'
	 *
	 * x	-- (#@case exp clause1 clause2 ...)
	 */
	x = SCM_CDR (x);
	arg1st = SCM_EVALCAR(x, env, &debug_info);
	/* `case'
	 *
	 * x	-- (exp clause1 clause2 ...)
	 * arg1st -- value of exp
	 */
	while (!SCM_IS_IMMEDIATE (x = SCM_CDR (x)))
	  {
	    proc = SCM_CAR (x);
	    /* `case'
	     *
	     * x	-- (clause_n clause_n+1 ...)
	     * arg1st 	-- value of exp
	     * proc	-- clause_n
	     */	    
	    if (scm_i_else == SCM_CAR (proc))
	      {
		x = SCM_CDR (proc);
		/* x	-- body of else clause: (exp1 exp2 ...)
		 */
		goto begin;
	      }
	    proc = SCM_CAR (proc);
	    /* proc	-- list of values for clause_n
	     */
	    while (!SCM_IS_IMMEDIATE (proc))
	      {
		if (SCM_CHECK_EQVISH (SCM_CAR (proc), arg1st))
		  {
		    x = SCM_CDR (SCM_CAR (x));
		    /* x	-- body of else clause: (exp1 exp2 ...)
		     */
		    goto begin;
		  }
		proc = SCM_CDR (proc);
	      }
	  }
	x = SCM_BOOL_F;
	/* x	-- the value to return.
	 */
#if 0
	SCM_RETBRK_CHECK;
#endif
	return x;
      }

    case (127 & SCM_IM_COND):
      {
	/* already expanded application of `cond'
	 *
	 * x	-- (#@cond clause1 clause2 ...)
	 */
	while (!SCM_IS_IMMEDIATE (x = SCM_CDR (x)))
	  {
	    proc = SCM_CAR (x);
	    arg1st = SCM_EVALCAR (proc, env, &debug_info);
	    /* `cond'
	     *
	     * x	-- (clause_n clause_n+1 ...)
	     * proc	-- condition expression of clause_n
	     * arg1st	-- value of proc
	     */
	    if (SCM_BOOL_F != arg1st)
	      {
		x = SCM_CDR (proc);
		/* `cond'
		 *
		 * x		-- body of clause_n, whose condition is not #f
		 * proc		-- condition expression of clause_n
		 * arg1st	-- value of proc
		 */
		if (SCM_EOL == x)
		  {
		    x = arg1st;
		    /* x	-- the value to return.
		     */
#if 0
		    SCM_RETBRK_CHECK;
#endif
		    return x;
		  }
		if (scm_i_arrow != SCM_CAR (x))
		  /* x		-- body of cond clause: (exp1 exp2 ...)
		   */
		  goto begin;


		/* `cond' where clause_n is (condition => procedure-expression)
		 *
		 * x	-- (=> procedure-expression)
		 * proc	-- condition expression of clause_n
		 * arg1st	-- value of proc
		 */
		proc = SCM_CDR (x);

		/* proc	-- (procedure-expression)
		 */
		proc = SCM_EVALCAR (proc, env, &debug_info);
		if (SCM_IS_IMMEDIATE (proc))
		  goto badfun;
		/* proc	-- => procedure to apply.
		 * arg1st -- value to apply proc to.
		 */
		goto evap1;
	      }
	  }
	x = SCM_BOOL_F;
	/* x	-- the value to return.
	 */
#if 0
	SCM_RETBRK_CHECK;
#endif
	return x;
      }

    case (127 & SCM_IM_LET):
      {
	/* already expanded application of `let'
	 *
	 * x	-- (#@let (... v3 v2 v1) (e1 e2 e3 ...) . body)
	 */
	x = SCM_CDR (x);
	proc = SCM_CAR (SCM_CDR (x));
	arg1st = SCM_EOL;

	/* `let'
	 *
	 * x	-- ((... v3 v2 v1) (e1 e2 e3 ...) . body)
	 * proc --  (e1 e2 e3 ...)
	 * arg1st --  ()
	 */
	do
	  {
	    arg1st = scm_cons (SCM_EVALCAR (proc, env, &debug_info), arg1st);
	  }
	while (!SCM_IS_IMMEDIATE (proc = SCM_CDR (proc)));

	/* `let'
	 *
	 * x	-- ((... v3 v2 v1) (e1 e2 e3 ...) . body)
	 * proc -- ()
	 * arg1st -- values of varriables being bound -- in reverse order
	 */
	env = SCM_EXTEND_ENV (SCM_CAR (x), arg1st, env);
	x = SCM_CDR (x);

	/* `let'
	 *
	 * x	-- ((e1 e2 e3 ...) . body)
	 */
	goto cdrxbegin;
      }

    case (127 & SCM_IM_LETREC):
      {
	/* already expanded application of `letrec'
	 *
	 * x	-- (#@letrec (... v3 v2 v1) (e1 e2 e3 ...) . body)
	 */
	x = SCM_CDR (x);
	env = SCM_EXTEND_ENV (SCM_CAR (x), scm_undefineds, env);
	x = SCM_CDR (x);
	proc = SCM_CAR (x);
	arg1st = SCM_EOL;
	/* `letrec'
	 *
	 * x	-- ((... v3 v2 v1) (e1 e2 e3 ...) . body)
	 * proc --  (e1 e2 e3 ...)
	 * arg1st --  ()
	 */
	do
	  {
	    arg1st = scm_cons (SCM_EVALCAR (proc, env, &debug_info), arg1st);
	  }
	while (!SCM_IS_IMMEDIATE (proc = SCM_CDR (proc)));
	SCM_CDR (SCM_CAR (env)) = arg1st;

	/* `let'
	 *
	 * x	-- ((e1 e2 e3 ...) . body)
	 */
	goto cdrxbegin;
      }

    case (127 & SCM_IM_LETSTAR):
      {
	/* `let*'
	 *
	 * x	-- (#@let* (v1 e1 v2 e2 v3 e3 ...) . body)
	 */
	x = SCM_CDR (x);
	proc = SCM_CAR (x);
	if (SCM_IS_IMMEDIATE (proc))
	  {
	    env = SCM_EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	    /* `let*'
	     *
	     * x	-- (() . body)
	     */
	    goto cdrxbegin;
	  }
	/* `let*'
	 *
	 * x	-- ((v1 e1 v2 e2 v3 e3 ...) . body)
	 * proc	-- (v1 e1 v2 e2 v3 e3 ...)
	 */
	do
	  {
	    arg1st = SCM_CAR (proc);
	    proc = SCM_CDR (proc);
	    /* `let*'
	     *
	     * x	-- ((v1 e1 v2 e2 v3 e3 ...) . body)
	     * proc	-- (en vn+1 en+2 ...)
	     * arg1st	-- vn
	     */
	    env = SCM_EXTEND_ENV (arg1st, SCM_EVALCAR (proc, env, &debug_info), env);
	  }
	while (!SCM_IS_IMMEDIATE (proc = SCM_CDR (proc)));
	/* `let*'
	 *
	 * x	-- ((v1 e1 v2 e2 v3 e3 ...) . body)
	 */
	goto cdrxbegin;
      }

    case (127 & SCM_IM_DEFINE):
      /* This is only for internal defines. 
       * Top-level defines are caught by "case scm_tcs_symbols:".
       */
      {
	/* #@define in code
	 *
	 * x	-- (#@define variable value)
	 */
	x = SCM_CDR (x);
	proc = SCM_CAR (x);
	x = SCM_CDR (x);
	x = SCM_EVALCAR(x, env, &debug_info);
	env = SCM_CAR (env);
	SCM_DEFER_INTS;
	SCM_CAR (env) = scm_cons (proc, SCM_CAR (env));
	SCM_CDR (env) = scm_cons (x, SCM_CDR (env));
	SCM_ALLOW_INTS;
	x = SCM_BOOL_F;
	/* x	-- the value to return (#f).
	 */
#if 0
	SCM_RETBRK_CHECK;
#endif
	return x;
      }

    case (127 & SCM_IM_DO):
      {
	/* An already expanded `do'
	 *
	 * x	-- (#@do (...v3 v2 v1) (i1 i2 i3 ...) (c . ret-body) body . (s1 s2 s3 ...))
	 */
	x = SCM_CDR (x);
	proc = SCM_CAR (SCM_CDR (x));
	arg1st = SCM_EOL;

	/* `do'; evaluating the inits
	 *
	 * x	-- ((...v3 v2 v1) (i1 i2 i3 ...) (c . ret-body) body . (s1 s2 s3 ...))
	 * proc -- (i1 i2 i3 ...)
	 * arg1st -- ()
	 */
	while (!SCM_IS_IMMEDIATE (proc))
	  {
	    arg1st = scm_cons (SCM_EVALCAR (proc, env, &debug_info), arg1st);
	    proc = SCM_CDR (proc);
	  }
	/* `do'; evaluating the inits
	 *
	 * x	  -- ((...v3 v2 v1) (i1 i2 i3 ...) (c . ret-body) body . (s1 s2 s3 ...))
	 * proc   -- (...v3 v2 v1)
	 * arg1st -- (...i3 i2 i1)
	 */
	env = SCM_EXTEND_ENV (SCM_CAR (x), arg1st, env);
	x = SCM_CDR (SCM_CDR (x));

	/* `do'; evaluating the inits
	 *
	 * x	-- ((c . ret-body) body . (s1 s2 s3 ...))
	 */
	while (proc = SCM_CAR (x), (SCM_BOOL_F == SCM_EVALCAR (proc, env, &debug_info)))
	  {
	    for (proc = SCM_CAR (SCM_CDR (x)); !SCM_IS_IMMEDIATE (proc); proc = SCM_CDR (proc))
	      {
		/* `do'; evaluating the body
		 *
		 * x	-- ((c . ret-body) (b1 b2 b2 ...) . (s1 s2 s3 ...))
		 * proc -- (b_n b_n+1 ...)
		 */
		arg1st = proc;
		SCM_EVALCAR (arg1st, env, &debug_info);
	      }

	    for (arg1st = SCM_EOL, proc = SCM_CDR (SCM_CDR (x));
		 !SCM_IS_IMMEDIATE (proc);
		 proc = SCM_CDR (proc))
	      {
		/* `do'; evaluating the steps
		 *
		 * x	  -- ((c . ret-body) (b1 b2 b2 ...) . (s1 s2 s3 ...))
		 * proc   -- (s1 s2 s3 ...)
		 * arg1st -- (... value_of_s3 value_of_s2 value_of_s1)
		 */
		arg1st = scm_cons (SCM_EVALCAR (proc, env, &debug_info), arg1st); /* steps */
	      }
	    /* `do'; evaluating the steps
	     *
	     * x      -- ((c . ret-body) (b1 b2 b2 ...) . (s1 s2 s3 ...))
	     * proc   -- ()
	     * arg1st -- (... value_of_s3 value_of_s2 value_of_s1)
	     *
	     * Note we are replacing an existing environment frame.
	     */
	    env = SCM_EXTEND_ENV (SCM_CAR (SCM_CAR (env)), arg1st, SCM_CDR (env));
	  }


	/* `do'; evaluating the return body
	 *
	 * x	-- ((c . ret-body) (b1 b2 b2 ...) . (s1 s2 s3 ...))
	 * proc -- (c . ret-body)
	 */
	x = SCM_CDR (proc);
	if (SCM_EOL == x)
	  {
	    x = SCM_BOOL_F;
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    return x;
	  }
	/* `do'; evaluating the return body
	 *
	 * x	-- ret-body
	 */
	goto begin;
      }

    case (127 & SCM_MAKISYM (0)):
      {
	/****************************************************************
	 * Evaluating a non-immediate object.
	 * Which is a cons pair.  
	 * With an immediate value in the CAR.
	 *
	 * We got here by dispatching on the type of the value in its CAR,
	 * looking at the low-order 7 bits of that value.
	 *
	 * Those bits indicate that the CAR holds an isym or an iflag.
	 * If its an isym, it might be an extended instruction.
	 *
	 * Here, we are dispatching on the 7 bit extended tag of that
	 * value (bits 9-15).
	 */
	proc = SCM_CAR (x);
	if (!SCM_ISYMP (proc))
	  goto badfun;
	switch (SCM_ISYMNUM (proc))
	  {
	  case (SCM_ISYMNUM (SCM_IM_APPLY)):
	    /* An already expanded call to @apply
	     *
	     * x	-- (#@apply fn list-of-args-to-apply)
	     */
	    proc = SCM_CDR (x);
	    proc = SCM_EVALCAR (proc, env, &debug_info);
	    if (SCM_IS_IMMEDIATE (proc))
	      goto badfun;
	    if (SCM_CLOSUREP (proc))
	      {
		arg1st = SCM_CDR (SCM_CDR (x));
		arg1st = SCM_EVALCAR (arg1st, env, &debug_info);
#ifndef SCM_RECKLESS
		if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), arg1st))
		  goto umwrongnumargs;
#endif
		/* An already expanded call to @apply
		 *
		 * x		-- (#@apply fn expression-yielding-list-of-args-to-apply)
		 * proc 	-- a closure being applied
		 * arg1st	-- the list of arguments to apply
		 */
		env = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (proc)), arg1st, SCM_ENV (proc));
		x = SCM_CODE (proc);
		goto cdrxbegin;
	      }
	    proc = scm_i_apply;
	    goto evapply;
	    
	  case (SCM_ISYMNUM (SCM_IM_EVAL)):
	    /* An already expanded call to `@eval':
	     *
	     * x	-- (#@eval form)
	     */
	    x = SCM_CDR (x);
	    x = SCM_EVALCAR (x, env, &debug_info);
	    x = scm_copy_tree (x);
	    env = scm_make_top_level_env(SCM_CDR(scm_top_level_lookup_thunk_var));
	    /* x	-- value of form
	     * env	-- the current top-level environment
	     */
	    goto tail_call_ceval;

	  case (SCM_ISYMNUM (SCM_IM_EVAL_X)):
	    /* An already expanded call to `@eval!':
	     *
	     * x	-- (#@eval form)
	     */
	    x = SCM_CDR (x);
	    x = SCM_EVALCAR (x, env, &debug_info);
	    env = scm_make_top_level_env(SCM_CDR(scm_top_level_lookup_thunk_var));
	    /* x	-- value of form
	     * env	-- the current top-level environment
	     */
	    goto tail_call_ceval;

	  case (SCM_ISYMNUM (SCM_IM_EVAL2)):
	    /* An already expanded call to `@eval2':
	     *
	     * x	-- (#@eval2 form env)
	     */
	    arg1st = SCM_CDR (x);
	    arg2nd = SCM_CDDR (x);
	    x = SCM_EVALCAR (arg1st, env, &debug_info);
	    x = scm_copy_tree (x);
	    env = SCM_EVALCAR (arg2nd, env, &debug_info);
	    /* x	-- value of form
	     * env	-- value of env
	     */
	    goto tail_call_ceval;

	  case (SCM_ISYMNUM (SCM_IM_EVAL2_X)):
	    /* An already expanded call to `@eval2!':
	     *
	     * x	-- (#@eval2! form env)
	     */
	    arg1st = SCM_CDR (x);
	    arg2nd = SCM_CDDR (x);
	    x = SCM_EVALCAR (arg1st, env, &debug_info);
	    env = SCM_EVALCAR (arg2nd, env, &debug_info);
	    /* x	-- value of form
	     * env	-- value of env
	     */
	    goto tail_call_ceval;

	  case (SCM_ISYMNUM (SCM_IM_THE_ENV)):
	    return env;

	  case (SCM_ISYMNUM (SCM_IM_CONT)):
	    /* An already expanded call to @call-with-current-continuation.
	     *
	     * x	-- (#@call-with-current-continuation e)
	     */
	    scm_make_cont (&arg1st);
	    if (setjmp (SCM_REGS (arg1st)->jmpbuf))
	      {
		/* The continuation was called:
		 *
		 * x		-- (#@call-with-current-continuation e)
		 * arg1st	-- the new continuation.
		 */
		x = SCM_REGS (arg1st)->throw_value; /* The value passed to the continuation. */
#if 0
		SCM_RETBRK_CHECK;
#endif
		/* x	-- the value to return.
		 */
		return x;
	      }
	    proc = SCM_CDR (x);
	    proc = SCM_EVALCAR (proc, env, &debug_info);
	    if (SCM_IS_IMMEDIATE (proc))
	      goto badfun;

	    /* Calling with a new continuation.
	     *
	     * x	-- (#@call-with-current-continuation e)
	     * arg1st	-- the new continuation.
	     * proc	-- the value of e
	     */
	    goto evap1;
	    
	  default:
	    goto badfun;
	  }
      }


      /****************************************************************
       * Evaluating a non-immediate object.
       * Which is a cons pair.  
       * With a non-immediate value in the CAR.
       ****************************************************************
       */
    case scm_tcs_cons_nimcar:
      {
	/* An application of some non-immediate value.
	 *
	 * x	-- (non-immediate e1 e2 ...)
	 */
	if (!SCM_SYMBOLP (SCM_CAR (x)))
	  {
	    proc = scm_ceval (SCM_CAR (x), env, &debug_info);
	    /* An application of some non-immediate value which
	     * is not a symbol and which is now evaluated.
	     *
	     * x	-- (non-immediate e1 e2 ...)
	     * proc	-- value of non-immediate
	     */
	    if (SCM_IS_IMMEDIATE (proc))
	      goto badfun;
	    /* equivalent to "goto checkargs;" */
	  }
	else
	  {
	    /* An application of some non-immediate value which
	     * is a symbol:
	     *
	     * x	-- (symbol e1 e2 ...)
	     */
	    proc = *scm_lookupcar (x, env, 0);
	    /* x	-- (iloc-or-gloc-for-symbol e1 e2 ...)
	     * proc	-- value of symbol
	     */
	    if (SCM_IS_IMMEDIATE (proc))
	      {
		scm_unmemocar (x, env);
		/* x	-- (iloc-or-gloc-for-symbol e1 e2 ...)
		 * proc	-- value of symbol
		 */
		goto badfun;
	      }

	    if (scm_tc16_macro == SCM_TYP16 (proc))
	      {
		scm_unmemocar (x, env);
		/* x	-- (symbol e1 e2 ...)
		 * proc	-- macro which is the value of symbol
		 */
	      handle_a_macro:
		arg1st = scm_apply3 (SCM_CDR (proc), x, scm_cons (env, scm_listofnull), &debug_info);
		switch ((int) (SCM_CAR (proc) >> 16))
		  {
		  case 2:
		    /* x	-- (symbol e1 e2 ...)
		     * proc	-- memoizing macro which is the value of symbol
		     * arg1st	-- macro expansion of x
		     */
		    if (scm_ilength (arg1st) <= 0)
		      arg1st = scm_cons2 (SCM_IM_BEGIN, arg1st, SCM_EOL);
		    SCM_DEFER_INTS;
		    SCM_CAR (x) = SCM_CAR (arg1st);
		    SCM_CDR (x) = SCM_CDR (arg1st);
		    SCM_ALLOW_INTS;
		    /* x	-- the now-memoized macro expansion
		     */
		    goto tail_call_ceval;
		  case 1:
		    /* x	-- (symbol e1 e2 ...)
		     * proc	-- ordinary macro which is the value of symbol
		     * arg1st	-- expansion
		     */
		    if (!SCM_IS_IMMEDIATE (arg1st))
		      {
			x = arg1st;
			/* x	-- non-immediate expansion for further evaluation
			 */
			goto tail_call_ceval;
		      }
		  case 0:
		    /* x	-- (symbol e1 e2 ...) 
		     * arg1st	-- expansion of x which is also the value 
		     *		   of the expression
		     */
		    x = arg1st;
#if 0
		    SCM_RETBRK_CHECK;
#endif
		    /* x	-- the value to return.
		     * env	-- the environment for evaluation.
		     */
		    return x;
		  }
	      }
	  }

      checkargs:
	{
	  /* x 		-- an application
	   * proc	-- the value of the car
	   */
	  if (SCM_CLOSUREP (proc))
	    {
#ifndef SCM_RECKLESS
	      arg2nd = SCM_CAR (SCM_CODE (proc));
	      arg1st = SCM_CDR (x);
	      while (!SCM_IS_IMMEDIATE (arg2nd))
		{
		  if (SCM_NCONSP (arg2nd))
		    {
		      /* arg2nd -- rest argument
		       * arg1st -- provided arguments
		       *
		       * x	-- an application
		       * proc	-- the value of the car
		       */
		      if (0 > scm_eilength (arg1st))
			goto umwrongnumargs;
		      goto evapply;
		    }
		  if (SCM_IS_IMMEDIATE (arg1st) || SCM_NECONSP (arg1st))
		    {
		      /* arg2nd -- rest argument
		       * arg1st -- bogus provided argument
		       *
		       * x	-- an application
		       * proc	-- the value of the car
		       */
		      goto umwrongnumargs;
		    }
		  arg2nd = SCM_CDR (arg2nd);
		  arg1st = SCM_CDR (arg1st);
		}
	      if (SCM_EOL != arg1st)
		{
		  /* x		-- an application
		   * proc	-- the value of the car
		   */
		  goto umwrongnumargs;
		}
#endif
	      /* x	-- an application
	       * proc	-- the value of the car
	       */
	      goto evapply;
	    }
	  else if (scm_tc16_macro == SCM_TYP16 (proc))
	    {
	      /* x	-- an application
	       * proc	-- the value of the car
	       */
	      goto handle_a_macro;
	    }
	  else
	    {
	      /* x	-- an application
	       * proc	-- the value of the car
	       */
	      goto evapply;
	    }
	}
      }
    }


  /****************************************************************
   * Evaluating a Procedure Application
   *
   * Macros have been expanded and the procedure position of
   * the application has been evaluated.  That value is a 
   * non-immediate, but not necessarily a procedure.
   ****************************************************************
   */
  
 evapply:
  /* x		-- an application
   * proc	-- the value of the car of x
   */
  
  if (SCM_EOL == SCM_CDR (x))
    {
      /****************************************************************
       * A Procedure Call With No Arguments Provided
       *
       * x	-- (fn)
       * proc	-- the value of the fn
       */
      switch (SCM_TYP7 (proc))
	{
	case scm_tc7_subr_1o:
	  x = (SCM_SUBRF (proc) (SCM_UNDEFINED));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	case scm_tc7_lsubr:
	  x = (SCM_SUBRF (proc) (SCM_EOL));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	case scm_tc7_rpsubr:
	  x = (SCM_BOOL_T);
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	case scm_tc7_asubr:
	  x = (SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	case scm_tc7_cclo:
	  arg1st = proc;
	  proc = SCM_CCLO_SUBR (proc);
	  /* x		-- (fn)
	   * arg1st	-- the value of fn; a cclo 
	   * proc	-- the subr that implements the cclo
	   */
	  goto evap1;
	case scm_tcs_closures:
	  x = SCM_CODE (proc);
	  /* x		-- (() . body) of proc
	   * proc	-- a closure
	   */
	  env = SCM_EXTEND_ENV (SCM_CAR (x), SCM_EOL, SCM_ENV (proc));
	  /* x		-- ((formals) . body) of proc
	   */
	  goto cdrxbegin;
	case scm_tc7_contin:
	case scm_tc7_subr_2:
	case scm_tc7_cxr:
	case scm_tc7_subr_3:
	case scm_tc7_lsubr_2:
	umwrongnumargs:
	  scm_unmemocar (x, env);
	/* wrongnumargs: */
	  scm_everr (x, env, proc, scm_wna, sym_eval);
	  /* not reached. */
	default:
	  /* x		-- (fn)
	   * proc	-- the value of fn; a wrong type to apply
	   */
	  goto badfun;
	}
    }



  /****************************************************************
   * A Procedure Call With Arguments Provided
   *
   * x		-- (fn . args)
   * proc	-- the value of fn
   */
  x = SCM_CDR (x);

#ifdef SCM_CAUTIOUS
  if (SCM_IS_IMMEDIATE (x) || SCM_NECONSP (x))
    goto umwrongnumargs;
#endif

  arg1st = SCM_EVALCAR(x, env, &debug_info);
  x = SCM_CDR (x);

  if (SCM_EOL == x)
    {
      /* A procedure call with one argument
       * 
       * x	-- ()
       * proc	-- the object being applied
       * arg1st	-- the value of arg.
       */
    evap1:
      switch (SCM_TYP7 (proc))
	{
	case scm_tc7_subr_1o:
	  x = (SCM_SUBRF (proc) (arg1st));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	case scm_tc7_cxr:
	  /* A procedure call with one argument
	   *
	   * proc	-- the object being applied
	   * arg1st	-- the value of arg.
	   *
	   * proc is a c_r function or a function that
	   * takes and returns a "double".
	   */
#ifdef SCM_FLOATS
	  if (SCM_SUBRF (proc))
	    {
	      /* proc is a function that takes and returns a "double".
	       */
	      if (SCM_INUMP (arg1st))
		{
		  x = (scm_makdbl (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1st)), 0.0));
#if 0
		  SCM_RETBRK_CHECK;
#endif
		  /* x	-- the value to return.
		   */
		  return x;
		}
	      if (SCM_IS_IMMEDIATE (arg1st))
		{
		  /* proc	-- the object being applied
		   * arg1st	-- the value of arg.
		   */
		  goto floerr;
		}
	      if (SCM_REALP (arg1st))
		{
		  x = (scm_makdbl (SCM_DSUBRF (proc) (SCM_REALPART (arg1st)), 0.0));
#if 0
		  SCM_RETBRK_CHECK;
#endif
		  /* x	-- the value to return.
		   */
		  return x;
		}
#ifdef SCM_BIGDIG
	      if (SCM_BIGP (arg1st))
		{
		  x = (scm_makdbl (SCM_DSUBRF (proc) (scm_big2dbl (arg1st)), 0.0));
#if 0
		  SCM_RETBRK_CHECK;
#endif
		  /* x	-- the value to return.
		   */
		  return x;
		}
#endif
	    floerr:
	      /* proc	-- the object being applied
	       * arg1st	-- the value of arg.
	       */
	      scm_wta (arg1st, scm_arg1, SCM_SNAME (proc));
	    }
#endif
	  /* proc	-- a c_r function
	   * arg1st	-- the argument to that function
	   */
	  proc = (SCM) SCM_SNAME (proc);
	  {
	    char * chrs;
	    chrs = SCM_RO_CHARS (proc) + SCM_LENGTH (proc) - 1;
	    while ('c' != *--chrs)
	      {
		SCM_ASSERT (!SCM_IS_IMMEDIATE (arg1st) && SCM_CONSP (arg1st),
			    arg1st, scm_arg1, proc);
		arg1st = ('a' == *chrs) ? SCM_CAR (arg1st) : SCM_CDR (arg1st);
	      }
	    x = (arg1st);
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    return x;
	  }
	case scm_tc7_rpsubr:
	  /* proc	-- a relational predicate
	   * arg1st	-- the argument to that function
	   */
	  /* Typecheck the argument:
	   */
	  (void) (SCM_SUBRF (proc) (arg1st, arg1st));
	  /* One argument always returns #t.
	   */
	  x = SCM_BOOL_T;
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  return x;
	case scm_tc7_asubr:
	  /* proc	-- an associative function
	   * arg1st	-- the argument to that function
	   */
	  x = (SCM_SUBRF (proc) (arg1st, SCM_UNDEFINED));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  return x;
	case scm_tc7_lsubr:
	  /* proc	-- a function that takes any number of arguments
	   * arg1st	-- the argument to that function
	   */
	  x = (SCM_SUBRF (proc) (scm_cons (arg1st, SCM_EOL)));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  return x;
	case scm_tc7_cclo:
	  /* proc	-- a cclosure; it gets itself as an implicit argument
	   * arg1st	-- the argument to that function
	   */
	  arg2nd = arg1st;
	  arg1st = proc;
	  proc = SCM_CCLO_SUBR (proc);
	  goto evap2;
	case scm_tcs_closures:
	  /* proc	-- a closure of one argument
	   * arg1st	-- the evaluated argument
	   */
	  x = SCM_CODE (proc);
	  env = SCM_EXTEND_ENV (SCM_CAR (x), scm_cons (arg1st, SCM_EOL), SCM_ENV (proc));
	  /* x		-- (formals . body)
	   */
	  goto cdrxbegin;
	case scm_tc7_contin:
	  scm_call_continuation (proc, arg1st);
	case scm_tc7_subr_2:
	case scm_tc7_subr_3:
	case scm_tc7_lsubr_2:
	  /* proc	-- the object being applied
	   */
	  goto umwrongnumargs;
	default:
	  /* proc	-- the object being applied
	   */
	  goto badfun;
	}
    }


#ifdef SCM_CAUTIOUS
  if (SCM_IS_IMMEDIATE (x) || SCM_NECONSP (x))
    goto umwrongnumargs;
#endif

  {				
    /* A procedure call with more than one argument
     * 
     * x	-- (arg2-expression ...)
     * proc	-- the object being applied
     * arg1st	-- the value of arg1.
     */
    arg2nd = SCM_EVALCAR(x, env, &debug_info);
    x = SCM_CDR (x);
    if (SCM_EOL == x)
      {
	/* Exactly two arguments.
	 * 
	 * x		-- ()
	 * proc		-- the object being applied
	 * arg1st	-- the value of arg1.
	 * arg2nd	-- the value of arg2.
	 */
      evap2:
	switch (SCM_TYP7 (proc))
	  {			
	  case scm_tc7_subr_2:
	    x = (SCM_SUBRF (proc) (arg1st, arg2nd));
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    /* x	-- the value to return.
	     */
	    return x;
	  case scm_tc7_lsubr:
	    x = (SCM_SUBRF (proc) (scm_cons2 (arg1st, arg2nd, SCM_EOL)));
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    /* x	-- the value to return.
	     */
	    return x;
	  case scm_tc7_lsubr_2:
	    x = (SCM_SUBRF (proc) (arg1st, arg2nd, SCM_EOL));
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    /* x	-- the value to return.
	     */
	    return x;
	  case scm_tc7_rpsubr:
	  case scm_tc7_asubr:
	    x = (SCM_SUBRF (proc) (arg1st, arg2nd));
#if 0
	    SCM_RETBRK_CHECK;
#endif
	    /* x	-- the value to return.
	     */
	    return x;
	  case scm_tc7_cclo:
	    {
	      SCM args;
	    cclon:
	      /* This code is used for more 2 or more arguments.
	       *
	       * proc	-- the cclo being applied; it gets itself as an implicit argument
	       * arg1st	-- the value of arg1.
	       * arg2nd	-- the value of arg2.
	       *
	       * When reached by "case scm_tc7_cclo"
	       * x	-- ()
	       *
	       * When reached by "goto cclon;"
	       * x	-- (arg3-expression arg4-expression ...)
	       */
	      if (0 > scm_eilength (x))
		goto umwrongnumargs;
	      EVAL_ARGS;	/* this code handles more than two arguments, too */
	      x = (scm_apply3 (SCM_CCLO_SUBR (proc), proc,
			      scm_cons2 (arg1st, arg2nd, scm_cons (args, SCM_EOL)), &debug_info));
#if 0
	      SCM_RETBRK_CHECK;
#endif
	      /* x	-- the value to return.
	       */
	      return x;
	    }
	  case scm_tc7_cxr:
	  case scm_tc7_subr_1o:
	  case scm_tc7_subr_3:
	  case scm_tc7_contin:
	    goto umwrongnumargs;
	  default:
	    goto badfun;
	  case scm_tcs_closures:
	    /* proc	-- the closure being applied
	     * arg1st	-- the value of arg1st.
	     * arg2nd	-- the value of arg2nd.
	     */
	    env = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
				  scm_cons2 (arg1st, arg2nd, SCM_EOL), SCM_ENV (proc));
	    x = SCM_CODE (proc);
	    /* x	-- (formals . body)
	     */
	    goto cdrxbegin;
	  }
      }


    /* A procedure call with more than two arguments
     * 
     * x	-- (arg3rd-exp arg4th-exp ...)
     * proc	-- the object being applied
     * arg1st	-- the value of arg1st.
     * arg2nd	-- the value of arg2nd.
     */
    switch (SCM_TYP7 (proc))
      {
      case scm_tc7_subr_3:
	if (SCM_EOL != SCM_CDR (x))
	  goto umwrongnumargs;
	x = (SCM_SUBRF (proc) (arg1st, arg2nd, SCM_EVALCAR(x, env, &debug_info)));
#if 0
	SCM_RETBRK_CHECK;
#endif
	/* x	-- the value to return.
	 */
	return x;
      case scm_tc7_asubr:
      case scm_tc7_rpsubr:
	{
	  SCM args;
	  if (0 > scm_eilength (x))
	    goto umwrongnumargs;
	  EVAL_ARGS;
	  x = (scm_apply3 (proc, arg1st, scm_acons (arg2nd, args, SCM_EOL), &debug_info));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	}
      case scm_tc7_lsubr_2:
	{
	  SCM args;
	  if (0 > scm_eilength (x))
	    goto umwrongnumargs;
	  EVAL_ARGS;
	  x = (SCM_SUBRF (proc) (arg1st, arg2nd, args));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	}
      case scm_tc7_lsubr:
	{
	  SCM args;
	  if (0 > scm_eilength (x))
	    goto umwrongnumargs;
	  EVAL_ARGS;
	  x = (SCM_SUBRF (proc) (scm_cons2 (arg1st, arg2nd, args)));
#if 0
	  SCM_RETBRK_CHECK;
#endif
	  /* x	-- the value to return.
	   */
	  return x;
	}
      case scm_tc7_cclo:
	/* x		-- (arg3rd-exp arg4th-exp ...)
	 * proc		-- the cclo being applied; it gets itself as an implicit argument
	 * arg1st	-- the value of arg1st.
	 * arg2nd	-- the value of arg2nd.
	 */
	goto cclon;
      case scm_tcs_closures:
	{
	  SCM args;
	  /* x		-- (arg3rd-exp arg4th-exp ...)
	   * proc	-- the closure being applied
	   * arg1st	-- the value of arg1st.
	   * arg2nd	-- the value of arg2nd.
	   */
	  if (0 > scm_eilength (x))
	    goto umwrongnumargs;
	  EVAL_ARGS;
	  env = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
				scm_cons2 (arg1st, arg2nd, args),
				SCM_ENV (proc));
	  x = SCM_CODE (proc);
	  goto cdrxbegin;
	}
      case scm_tc7_subr_2:
      case scm_tc7_subr_1o:
      case scm_tc7_cxr:
      case scm_tc7_contin:
	goto umwrongnumargs;
      default:
	goto badfun;
      }
  }
}


/****************************************************************
 * eval Procedures
 */


/* scm_eval_x
 * 
 * Evaluate obj at the top level without copying it first.
 */
SCM
scm_eval_x (SCM obj)
{
  SCM_INTS_ENABLED;

  return
    scm_eval3(obj, 0,
	      scm_make_top_level_env (SCM_CDR (scm_top_level_lookup_thunk_var)));
}


/*(c eval)
 * (eval exp)
 * 
 * Evaluate `exp' at the top-level.
 *
 * The list structure of `exp' is copied before it is evaluated.
 */
SCM_PROC(s_eval, "eval", 1, 0, 0, scm_eval);
SCM
scm_eval (SCM obj)
{
  return scm_eval3 (obj, 1, scm_make_top_level_env(SCM_CDR(scm_top_level_lookup_thunk_var)));
}


/*(c eval2)
 * (eval2 exp proc)
 * 
 * Evaluate `exp' at the top-level.  Free variables are 
 * resolved by calling `proc' with two arguments:
 *
 *	(proc variable-name define?)
 *
 * `variable-name' is a symbol whose name is the name of the
 * free variable.
 *
 * `define?' is #t if the variable is being defined by `define' or
 * something equivalent.  
 *
 * If `define?' is #f, and there is no binding for `variable-name',
 * then `proc' should return #f.  Otherwise (if there is a binding
 * of if `define?' is not #f) then `proc' should return a variable
 * object. (*Note: First-Class Variables.)
 *
 * The list structure of `exp' is copied before it is evaluated.
 */
SCM_PROC(s_eval2, "eval2", 2, 0, 0, scm_eval2);
SCM
scm_eval2 (SCM obj, SCM env_thunk)
{
  SCM_INTS_ENABLED;

  return scm_eval3 (obj, 1, scm_make_top_level_env(env_thunk));
}


/* scm_eval3
 * 
 * Evaluate obj and return its value.
 *
 * If copyp, then copy the expression before evaluating it (but after
 * tranforming it with `system-transformer').
 */
SCM 
scm_eval3 (SCM obj, int copyp, SCM env)
{
  SCM_INTS_ENABLED;

  if (copyp)
    obj = scm_copy_tree (obj);
  return SCM_EVAL (obj, env, 0);
}


/*(c eval-environment!)
 * (eval-environment! expression environment)
 * 
 * Evaluate `expression' in `environment'.  The evaluator may modify
 * `expression'.
 */
SCM_PROC (s_eval_environment_x, "eval-environment!", 2, 0, 0, scm_eval_environment_x);
SCM
scm_eval_environment_x (SCM exp, SCM env)
{
  SCM_INTS_ENABLED;

  if (SCM_IS_TOP_LEVEL (env))
    {
      return SCM_EVAL (exp, env, 0);
    }
  else
    {
      SCM list;

      /* Use EVALCAR here, not EVAL, to be sure that ceval interprets
       * exp as an interior -- not a top-level form.
       */
      list = scm_cons (exp, SCM_EOL);
      return SCM_EVALCAR (list, env, 0);
    }
}



/* scm_evalcar
 * 
 * Evaluate the car of cons pair x and return its value.
 */
SCM 
scm_evalcar (SCM x, SCM env)
{
  SCM_INTS_ENABLED;

  return SCM_EVALCAR (x, env, 0);
}



/****************************************************************
 * The Heart of `apply'
 */


/* scm_apply
 * 
 * This is not the Scheme procedure `apply', though it is a C function
 * that works similarly.  Scheme's `apply' is coded in Scheme and
 * implemented in `eval' to assure that it is properly tail recursive.
 */
SCM
scm_apply (SCM fn, SCM args)
{
  return scm_apply3 (fn, scm_nconc2last (args), SCM_EOL, 0);
}


SCM
scm_apply_eval_helper (SCM fn, SCM arg1st, SCM args)
{
  return scm_apply3 (fn, arg1st, args, 0);
}


/* Perhaps call the break-point handler for returns from apply3.
 *
 * The value about to be returned is in x.
 */
#define SCM_APPRETBRK_CHECK \
  do \
    { \
      if (!local_retbrk_flag) \
	{ \
	  if (local_errbrk_flag) \
	    SCM_CATCH_BODY_END; \
	  scm_root->debug_info = debug_info.prev; \
        } \
      else \
        { \
          SCM answer; \
	    debug_info.type = scm_callout_return_frame; \
	  /* debug_info.arg1st already set */ \
	  /* debug_info.args already set */ \
          debug_info.value = a; \
          answer = scm_apply_retbrk (&debug_info); \
	  if (local_errbrk_flag) \
	    SCM_CATCH_BODY_END; \
	  scm_root->debug_info = debug_info.prev; \
	  return answer; \
        } \
    } while (0)


/* scm_apply3
 * 
 * Apply proc to arg1st and to the arguments in the list `args'
 * following the usual rule for interpreting arguments to apply.
 */
SCM 
scm_apply3 (SCM proc, SCM arg1st, SCM args, struct scm_debug_frame * prev)
{
  SCM_INTS_ENABLED;
#if 0
  SCM_CATCH_LOCALS;
  struct scm_debug_frame debug_info;
  int local_retbrk_flag;
  int local_errbrk_flag;
#endif
  SCM a;

  /* On entry, we link debug_info to prev and set scm_root->debug_info.
   * On exit, that has to be undone (RETBRK_CHECK, and returns from errbrk and stepbrk).
   *
   * On entry, we may set a catch handler.
   * On exit, we may have to SCM_CATCH_BODY_END
   *     (RETBRK_CHECK, and returns from stepbrk).
   */

  SCM_CHECK_STACK;

#if 0
  local_errbrk_flag = (SCM_BOOL_F != SCM_CDR (g_debug_on_exception_flag)) || stepbrk_flag;
  local_retbrk_flag = stepbrk_flag;

  if (prev)
    {
      debug_info.prev = prev;
      debug_info.prev_is_direct = 1;
      scm_root->debug_info = &debug_info;
    }
  else
    {
      debug_info.prev = scm_root->debug_info;
      debug_info.prev_is_direct = 0;
      scm_root->debug_info = &debug_info;
    }

  debug_info.retbrk_flag_ptr = &local_retbrk_flag;
#endif

  if (SCM_EOL == args)
    {
      if (SCM_EOL == arg1st)
	arg1st = SCM_UNDEFINED;
      else
	{
	  args = scm_list_copy (SCM_CDR (arg1st));
	  arg1st = SCM_CAR (arg1st);
	}
    }
  else
    {
      args = scm_nconc2last (args);
    }

#if 0
  debug_info.type = scm_callout_frame;
  debug_info.proc = proc;
  debug_info.arg1st = arg1st;
  debug_info.args = args;

  if (!local_errbrk_flag)
    {
      debug_info.can_catch_errors = 0;
    }
  else if (1)
    {
      debug_info.can_catch_errors = 1;
      SCM_CATCH_INIT(SCM_BOOL_T);
      if (SCM_CATCH_SETJMP())
	{
	  SCM answer;
	  SCM_UNWIND;
	  debug_info.type = scm_callout_exception_frame;
	  debug_info.proc = proc;
	  /* debug_info.arg1st already set */
	  /* debug_info.args already set */
	  debug_info.value = SCM_BOOL_F;
	  debug_info.debug_return = SCM_BOOL_F;
	  debug_info.throw_tag = SCM_THROW_TAG;
	  debug_info.throw_args = SCM_THROW_ARGS;
	  answer = scm_apply_errbrk (&debug_info);
	  scm_root->debug_info = prev;
	  return answer;
	}
      SCM_CATCH_BODY_BEGIN;
    }
#endif

  if (SCM_IS_IMMEDIATE (proc))
    goto badproc;


  SCM_ASYNC_TICK;

#if 0
  /* offer to step
   */
  if (stepbrk_flag)
    {
      SCM debugger_val;
      local_retbrk_flag = 1;
      debug_info.type = scm_callout_step_frame;
      /* debug_info.arg1st already set */
      /* debug_info.args already set */
      debug_info.value = SCM_BOOL_F;
      debug_info.debug_return = SCM_BOOL_F;
      debugger_val = scm_apply_stepbrk ();
      if (!SCM_IS_IMMEDIATE (debugger_val) && SCM_CONSP (debugger_val))
	{
	  if (local_errbrk_flag)
	    SCM_CATCH_BODY_END;
	  scm_root->debug_info = prev;
	  return SCM_CAR (debugger_val);
	}
      debug_info.type = scm_callout_frame;
    }
  else
    local_retbrk_flag = 0;
#endif

 tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2:
      if (SCM_EOL == args)
	goto wrongnumargs;
      if (SCM_EOL != SCM_CDR (args))
	goto wrongnumargs;
      args = SCM_CAR (args);
      a = (SCM_SUBRF (proc) (arg1st, args));
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_subr_1o:
      if (SCM_EOL != args)
	goto wrongnumargs;
      a = (SCM_SUBRF (proc) (arg1st));
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_cxr:
      if (SCM_EOL != args)
	goto wrongnumargs;
#ifdef SCM_FLOATS
      if (SCM_SUBRF (proc))
	{
	  if (SCM_INUMP (arg1st))
	    {
	      a = (scm_makdbl (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1st)), 0.0));
#if 0
	      SCM_APPRETBRK_CHECK;
#endif
	      return a;
	    }
	  if (SCM_IS_IMMEDIATE (arg1st))
	    goto floerr;
	  if (SCM_REALP (arg1st))
	    {
	      a = (scm_makdbl (SCM_DSUBRF (proc) (SCM_REALPART (arg1st)), 0.0));
#if 0
	      SCM_APPRETBRK_CHECK;
#endif
	      return a;
	    }
#ifdef SCM_BIGDIG
	  if (SCM_BIGP (arg1st))
	    {
	      a = (scm_makdbl (SCM_DSUBRF (proc) (scm_big2dbl (arg1st)), 0.0));
#if 0
	      SCM_APPRETBRK_CHECK;
#endif
	      return a;
	    }
#endif
	floerr:
	  scm_wta (arg1st, scm_arg1, SCM_SNAME (proc));
	}
#endif
      proc = (SCM) SCM_SNAME (proc);
      {
	char * chrs;
	chrs = SCM_RO_CHARS (proc) + SCM_LENGTH (proc) - 1;
	while ('c' != *--chrs)
	  {
	    SCM_ASSERT (!SCM_IS_IMMEDIATE (arg1st) && SCM_CONSP (arg1st),
			arg1st, scm_arg1, proc);
	    arg1st = ('a' == *chrs) ? SCM_CAR (arg1st) : SCM_CDR (arg1st);
	  }
	a = (arg1st);
#if 0
	SCM_APPRETBRK_CHECK;
#endif
	return a;
      }
    case scm_tc7_subr_3:
      {
	SCM arg2nd;
	SCM arg3rd;

	if (SCM_UNBNDP (arg1st))
	  goto wrongnumargs;
	if (SCM_IS_IMMEDIATE (args) || !SCM_CONSP (args))
	  goto wrongnumargs;
	else
	  {
	    arg2nd = SCM_CAR (args);
	    args = SCM_CDR (args);
	  }
	if (SCM_IS_IMMEDIATE (args) || !SCM_CONSP (args))
	  goto wrongnumargs;
	else
	  {
	    arg3rd = SCM_CAR (args);
	    args = SCM_CDR (args);
	  }
	a = SCM_SUBRF (proc) (arg1st, arg2nd, arg3rd);
#if 0
	SCM_APPRETBRK_CHECK;
#endif
	return a;
      }
    case scm_tc7_lsubr:
      a = (SCM_SUBRF (proc) (SCM_UNBNDP (arg1st) ? SCM_EOL : scm_cons (arg1st, args)));
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_lsubr_2:
      if (SCM_IS_IMMEDIATE (args) || !SCM_CONSP (args))
	goto wrongnumargs;
      a = (SCM_SUBRF (proc) (arg1st, SCM_CAR (args), SCM_CDR (args)));
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_asubr:
      if (SCM_EOL == args)
	{
	  a = (SCM_SUBRF (proc) (arg1st, SCM_UNDEFINED));
#if 0
	  SCM_APPRETBRK_CHECK;
#endif
	  return a;
	}
      while (!SCM_IS_IMMEDIATE (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, scm_arg2, s_apply);
	  arg1st = SCM_SUBRF (proc) (arg1st, SCM_CAR (args));
	  args = SCM_CDR (args);
	}
      a = (arg1st);
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_rpsubr:
      if (SCM_EOL == args)
	{
	  if (!SCM_UNBNDP (arg1st))
	    {
	      /* Typecheck the argument:
	       */
	      (void) (SCM_SUBRF (proc) (arg1st, arg1st));
	    }
	  a = (SCM_BOOL_T);
#if 0
	  SCM_APPRETBRK_CHECK;
#endif
	  return a;
	}
      while (!SCM_IS_IMMEDIATE (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, scm_arg2, s_apply);
	  if (SCM_BOOL_F == SCM_SUBRF (proc) (arg1st, SCM_CAR (args)))
	    {
	      a = (SCM_BOOL_F);
#if 0
	      SCM_APPRETBRK_CHECK;
#endif
	      return a;
	    }
	  arg1st = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      if (SCM_EOL != args)
	scm_wta (args, scm_argn, s_apply);
      a = (SCM_BOOL_T);
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tcs_closures:
      arg1st = (SCM_UNBNDP (arg1st) ? SCM_EOL : scm_cons (arg1st, args));
#ifndef SCM_RECKLESS
      if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), arg1st))
	goto wrongnumargs;
#endif
      args = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (proc)), arg1st, SCM_ENV (proc));
      proc = SCM_CODE (proc);
      while (SCM_EOL != (proc = SCM_CDR (proc)))
	arg1st = SCM_EVALCAR (proc, args, 0);
      a = arg1st;
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    case scm_tc7_contin:
      if ((SCM_EOL != args) || SCM_UNBNDP (arg1st))
	goto wrongnumargs;
      scm_call_continuation (proc, arg1st);
    case scm_tc7_cclo:
      args = (SCM_UNBNDP(arg1st) ? SCM_EOL : scm_cons (arg1st, args));
      arg1st = proc;
      proc = SCM_CCLO_SUBR (proc);
      goto tail;
    wrongnumargs:
      scm_wta (proc, scm_wna, s_apply);
    default:
    badproc:
      scm_wta (proc, scm_arg1, s_apply);
      a = (arg1st);
#if 0
      SCM_APPRETBRK_CHECK;
#endif
      return a;
    }
}


/* nconc2last list
 * 
 * Return a copy of `list' such that second-to-last CDR is replaced by
 * a list copy of the contents of the last CAR.  If `list' is (),
 * return ().  If `list' has only one element, return a list copy of
 * the CAR of that element.
 */
SCM_PROC (s_nconc2last, "nconc2last", 1, 0, 0, scm_nconc2last);
SCM 
scm_nconc2last (SCM lst)
{
  SCM_INTS_ENABLED;
  SCM *lloc;

  lst = scm_list_copy (lst);
  if (SCM_EOL == lst)
    return SCM_EOL;
  SCM_ASSERT (!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst), lst, scm_arg1, s_nconc2last);
  lloc = &lst;
  while (SCM_EOL != SCM_CDR (*lloc))
    {
      lloc = &SCM_CDR (*lloc);
      SCM_ASSERT (!SCM_IS_IMMEDIATE (*lloc) && SCM_CONSP (*lloc), lst, scm_arg1, s_nconc2last);
    }
  *lloc = scm_list_copy (SCM_CAR (*lloc));
  return lst;
}



/************************************************************************
 *(h1 "Support for Debugging")
 *
 * \WARNING:/ This code hasn't been touched in a long time and is
 * probably somewhat broken.
 * 
 * The evaluator provides low-level support for debugging Scheme
 * programs.  The evalutator can single-step through expressions
 * allowing Scheme programs to examine what is being evaluated, skip
 * chosen subexpressions, catch errors in chosen subexpressions, and
 * override the return value of already evaluated expressions.  At any
 * time the evaluator can produce a backtrace that describes what
 * expressions are currently being evaluated.
 *
 * The procedure `call-stepping' is used to enter the evaluator in
 * single-step mode.  While in single step mode, the evaluator pauses
 * before each subexpression and calls the (user-definable) procedure
 * `step-break' with no arguments.  If `step-break' returns a cons
 * pair, evaluation of that subexpression is skipped and the value in
 * the car of that pair is used as the value of the subexpression.
 *
 */

/*
 * eval frames
 * callout frames
 * global debugging variables: debug-on-error
 * local debugging variables: step, tailcall, return
 * breakpoint functions eval-step, eval-tailcall, eval-return, eval-exception
 * 			callout-step, callout-exception, callout-return
 *
 * raw-backtrace, backtrace-formats, (ice-9 debugging-basics)
 */


/************************************************************************
 *(h1 "Debugging-Related Procedures")
 * 
 * 
 * 
 */

/****************************************************************
 * Entering Single Step Mode
 */

struct call_stepping_closure
{
  int saved_stepbrk;
  SCM thunk;
};


/* scm_set_stepbrk_flag_guard
 * 
 * The scm_c_dynamic_wind guard function of `stepbrk'.  This sets
 * stepbrk_flag while the body is executing.
 */
static void
scm_set_stepbrk_flag_guard (int direction, void * vclosure)
{
  SCM_INTS_ENABLED;		/* but scm_mask_ints is 1 */
  struct call_stepping_closure * closure;

  closure = (struct call_stepping_closure *)vclosure;
  if (direction)
    {
      closure->saved_stepbrk = stepbrk_flag;
      stepbrk_flag = 1;
    }
  else
    stepbrk_flag = closure->saved_stepbrk;
}


/* scm_brkpt_call
 * 
 * The scm_c_dynamic_wind body function of `stepbrk'.  This calls the
 * procedure that will be stepped through.
 */
static SCM
scm_stepbrk_call (void * vclosure)
{
  SCM_INTS_ENABLED;
  struct call_stepping_closure * closure;

  closure = (struct call_stepping_closure *)vclosure;
  return scm_apply (closure->thunk, SCM_EOL);
}


/*(c call-stepping)
 * (call-stepping thunk)
 * 
 * Invoke `thunk' in single-step mode. 
 */
SCM_PROC (s_call_stepping, "call-stepping", 1, 0, 0, scm_call_stepping);
SCM
scm_call_stepping (SCM thunk)
{
  struct call_stepping_closure closure;
  closure.thunk = thunk;
  return scm_c_dynamic_wind (scm_set_stepbrk_flag_guard, scm_stepbrk_call, &closure);
}



/****************************************************************
 * Backtraces
 */

/*(c raw-backtrace)
 * (raw-backtrace)
 * 
 * Return the current bactkrace information. 
 */
SCM_PROC (s_raw_backtrace, "raw-backtrace", 0, 0, 0, scm_raw_backtrace);
SCM
scm_raw_backtrace (void)
{
  SCM_INTS_ENABLED;
  struct scm_debug_frame * info;
  SCM * pos;
  SCM answer;

  info = scm_root->debug_info;
  answer = SCM_EOL;
  pos = &answer;
  while (info)
    {
      SCM frame_desc;
      SCM type;

      frame_desc = SCM_EOL;
      switch (info->type)
	{
	case scm_eval_frame:
	  type = s_eval;
	  frame_desc = scm_listify (scm_int_to_bool (*info->stepbrk_flag_ptr),
				    scm_int_to_bool (*info->retbrk_flag_ptr),
				    info->exp,
				    info->env,
				    SCM_UNDEFINED);
	  break;
	case scm_eval_step_frame:
	  type = s_eval_step;
	  frame_desc = scm_listify (scm_int_to_bool (*info->stepbrk_flag_ptr),
				    scm_int_to_bool (*info->retbrk_flag_ptr),
				    info->exp,
				    info->env,
				    SCM_UNDEFINED);
	  break;
	case scm_eval_tailcall_frame:
	  type = s_eval_tailcall;
	  frame_desc = scm_listify (scm_int_to_bool (*info->stepbrk_flag_ptr),
				    scm_int_to_bool (*info->retbrk_flag_ptr),
				    info->exp,
				    info->env,
				    SCM_UNDEFINED);
	  break;
	case scm_eval_exception_frame:
	  type = s_eval_exception;
	  frame_desc = scm_listify (info->exp,
				    info->env,
				    info->throw_tag,
				    info->throw_args,
				    SCM_UNDEFINED);
	  break;
	case scm_eval_return_frame:
	  type = s_eval_return;
	  frame_desc = scm_listify (info->exp,
				    info->env,
				    info->value,
				    SCM_UNDEFINED);
	  break;
	case scm_callout_frame:
	  type = s_callout;
	  frame_desc = scm_listify (scm_int_to_bool (*info->retbrk_flag_ptr),
				    info->proc,
				    scm_cons (info->arg1st, info->args),
				    SCM_UNDEFINED);
	  break;
	case scm_callout_step_frame:
	  type = s_callout_step;
	  frame_desc = scm_listify (scm_int_to_bool (*info->retbrk_flag_ptr),
				    info->proc,
				    scm_cons (info->arg1st, info->args),
				    SCM_UNDEFINED);
	  break;
	case scm_callout_exception_frame:
	  type = s_callout_exception;
	  frame_desc = scm_listify (info->proc,
				    scm_cons (info->arg1st, info->args),
				    info->throw_tag,
				    info->throw_args,
				    SCM_UNDEFINED);
	  break;
	case scm_callout_return_frame:
	  type = s_callout_return;
	  frame_desc = scm_listify (info->proc,
				    scm_cons (info->arg1st, info->args),
				    info->value,
				    SCM_UNDEFINED);
	  break;
	}
      frame_desc = scm_cons (scm_ulong2num ((unsigned long) info),
			     scm_cons (type,
				       scm_cons (scm_int_to_bool (info->prev_is_direct), frame_desc)));
      *pos = scm_cons (frame_desc, SCM_EOL);
      pos = &SCM_CDR (*pos);
      info = info->prev;
    }
  return answer;
}


static struct scm_debug_frame *
find_debug_frame (SCM frame_id, SCM msg, SCM proc_name)
{
  struct scm_debug_frame * goal;
  struct scm_debug_frame * pos;

  goal = (struct scm_debug_frame *)scm_num2ulong (frame_id, msg, proc_name);
  pos = scm_root->debug_info;
  while (pos)
    if (pos == goal)
      return pos;
    else
      pos = pos->prev;
  return 0;
}


/*(c set-stepbrk-flag)
 * (set-stepbrk-flag frame-id value)
 * 
 * Set the "stepbrk-flag" for a particular backtrace frame.  
 */
SCM_PROC (s_set_stepbrk_flag, "set-stepbrk-flag", 2, 0, 0, scm_set_stepbrk_flag);
SCM 
scm_set_stepbrk_flag (SCM frame_id, SCM value)
{
  struct scm_debug_frame * frame;

  frame = find_debug_frame (frame_id, scm_arg1, s_set_stepbrk_flag);
  *(frame->stepbrk_flag_ptr) = (value != SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}


/*(c set-retbrk-flag)
 * (set-retbrk-flag frame-id value)
 * 
 * Set the "retbrk-flag" for a particular backtrace frame.  (*Note:
 * Support for Debugging.)
 */
SCM_PROC (s_set_retbrk_flag, "set-retbrk-flag", 2, 0, 0, scm_set_retbrk_flag);
SCM 
scm_set_retbrk_flag (SCM frame_id, SCM value)
{
  struct scm_debug_frame * frame;

  frame = find_debug_frame (frame_id, scm_arg1, s_set_retbrk_flag);
  *(frame->retbrk_flag_ptr) = (value != SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}


/****************************************************************
 * Debugger Hooks
 */

struct brkpt_closure
{
  int saved_stepbrk;
  SCM procedure;
};


#if 0
/* scm_clear_stepbrk_flag
 * 
 * The scm_c_dynamic_wind procedure for breakpoint functions. This
 * clears "stepbrk_flag" on entering a dynamic wind context, restores
 * it on exit.
 */
static void
scm_clear_stepbrk_flag (int direction, void * vclosure)
{
  SCM_INTS_ENABLED;		/* but scm_mask_ints is 1 */
  struct brkpt_closure * closure;

  closure = (struct brkpt_closure *)vclosure;
  if (direction)
    {
      closure->saved_stepbrk = stepbrk_flag;
      stepbrk_flag = 0;
    }
  else
    stepbrk_flag = closure->saved_stepbrk;
}


/* scm_brkpt_call
 * 
 * The scm_c_dynamic_wind body for breakpoint functions.
 * It simply invokes the breakpoint procedure with no arguments.
 */
static SCM
scm_brkpt_call (void * vclosure)
{
  SCM_INTS_ENABLED;
  struct brkpt_closure * closure;

  closure = (struct brkpt_closure *)vclosure;
  return scm_apply (closure->procedure, SCM_EOL);
}
#endif

/* scm_stepbrk
 * 
 * ceval is about to start on a new expression.  Because
 * local_stepbrk_flag is set, the procedure `stepbrk' is called.
 */
#if 0
static SCM
scm_stepbrk (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = SCM_BOOL_F;
  if (SCM_BOOL_F != SCM_CDR (g_step_breakpoint))
    {
      struct brkpt_closure closure;

      closure.procedure = SCM_CDR (g_step_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  return answer;
}
#endif

#if 0
/* scm_tailbrk
 * 
 * ceval is about to start on a new expression due to a tail-call from
 * ceval to ceval.  Because local_retbrk_flag was set, the procedure
 * `tailbrk' is called.
 */
static SCM
scm_tailbrk (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = SCM_BOOL_F;
  if (SCM_BOOL_F != SCM_CDR (g_tail_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_tail_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  return answer;
}
#endif

#if 0
/* scm_retbrk
 * 
 * ceval is about to return.  Because the local_retbrk_flag was set
 * for that frame, the procedure `retbrk' is called.
 */
static SCM
scm_retbrk (struct scm_debug_frame * debug_info)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = debug_info->value;
  if (SCM_BOOL_F != SCM_CDR (g_return_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_return_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  return answer;
}
#endif

#if 0
/* scm_errbrk
 * 
 * ceval caught an error (local_errbrk_flag was set).  The
 * procedure `errbrk' is called.
 */
static SCM
scm_errbrk (struct scm_debug_frame * debug_info)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = debug_info->value;
  if (SCM_BOOL_F != SCM_CDR (g_exception_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_exception_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  else
    scm_throw (debug_info->throw_tag, debug_info->throw_args);
  return answer;
}
#endif

#if 0
/* scm_apply_stepbrk
 * 
 * apply3 is about to begin work.  Because local_stepbrk_flag is set,
 * the procedure `applybrk' is called.
 */
static SCM
scm_apply_stepbrk (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = SCM_BOOL_F;
  if (SCM_BOOL_F != SCM_CDR (g_callout_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_callout_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  return answer;
}
#endif

#if 0
/* scm_apply_errbrk
 * 
 * apply3 caught an error (local_errbrk_flag was set).  The
 * procedure `applyerrbrk' is called.
 */
static SCM
scm_apply_errbrk (struct scm_debug_frame * debug_info)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = debug_info->value;
  if (SCM_BOOL_F != SCM_CDR (g_callout_exception_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_callout_exception_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  else
    scm_throw (debug_info->throw_tag, debug_info->throw_args);
  return answer;
}
#endif

#if 0
/* scm_apply_retbrk
 * 
 * apply3 is about to return.  Because the local_retbrk_flag was set
 * for that frame, the procedure `applyretbrk' is called.
 */
static SCM
scm_apply_retbrk (struct scm_debug_frame * debug_info)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = debug_info->value;
  if (SCM_BOOL_F != SCM_CDR (g_callout_return_breakpoint))
    {
      struct brkpt_closure closure;
      
      closure.procedure = SCM_CDR (g_callout_return_breakpoint);
      answer = scm_c_dynamic_wind (scm_clear_stepbrk_flag, scm_brkpt_call, &closure);
    }
  return answer;
}
#endif



void 
scm_init_eval (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/eval.x"
  scm_i_apply = (scm_permanent_object
		 (scm_make_subr ("apply", scm_tc7_lsubr_2, (SCM (*)())scm_apply_eval_helper, 0)));
  scm_top_level_lookup_thunk_var = scm_intern_symhash("*top-level-lookup-thunk*", SCM_BOOL_F);
}


/************************************************************************
 * (h1 "Evaluation Internals")
 *
 * \WARNING:  SPOILERS AHEAD./  When someone posts to a web log
 * a message about a movie, and the message gives away really cool
 * surprises in the plot, it is considered good form to warn potential
 * readers that the message contains *SPOILERS*.  The idea is that
 * if you like movies, you might want to see the movie for yourself
 * before reading the message.
 * 
 * We think the same rule applies here.  The implementation of `eval'
 * in Systas Scheme is lifted right out of SCM.  The implementation
 * of `eval' in SCM is, in our opinion, one of the most beautiful 
 * (if somewhat cryptic) pieces of C code you'll ever see.  If you
 * want to have some fun, grab SCM and puzzle it out for yourself.
 * If you just want the facts, or get fed up with Aubrey's very terse
 * coding style, read on.
 * 
 * Evaluation of immediate values is trivial.
 *
 * Evaluation of non-immediates works by dispatching first on the low
 * order 7 bits of the car of the non-immediate, and then sometimes
 * (if the car holds an isym which is not an ispcsym) dispatching
 * again on an additional 7 bits (the ISYMNUM).
 *
 * The first 7 bits of the car distinguish self-evaluating objects
 * from other objects.  They individually distinguish cons pairs
 * having one of the 13 ispcsyms (virtual machine instructions) in the
 * car.  They individually distinguish cons pairs having an iloc or
 * gloc or in the car.  They distinguish cons pairs having a
 * self-evaluating immediate in the car.  They distinguish cons pairs
 * having a non-immediate object in the car (but do not reveal the
 * type of that non-immediate).  Each of those cases implies a
 * different rule for evaluation and eval is a large switch statement
 * that handles them.  
 * 
 * The cases for the 13 ISPCSYMs implement Scheme-like special forms
 * (#@set!, #@let, #@do, etc.).  Scheme syntax is implemented as macros
 * that translate into code built out of those instructions.  
 *
 * There are 127 immediate values called ISYMS which all share a
 * common 7 bit tag.  That tag effectively functions as a fourteenth
 * ISPCYSM which is handled by a nested switch statement that dispatches
 * on the 7-bit ISYMNUM.  (See "tags.h" for more information on 
 * ISYMs and ISPCSYMs.)
 *
 * A complicated case is when a cons pair has a non-immediate in the
 * car, and that non-immediate evaluates to a subr, cclo, continuation
 * or closure.  That case is a procedure application and is handled by
 * code that first dispatches on the number of arguments provided,
 * then again on the type of the object being applied.  The type of a
 * subr correlates with the number of required and optional
 * parameters and in general there is a different rule for applying
 * each kind of applicable object.  
 * 
 * There are special subrs for the common cases of relational
 * predicates (e.g. "<?"), list accessor functions (e.g. "car" and
 * "cadar"), and associative functions whose arguments (in C) must be
 * double-precision floating point numbers.  List accessors and
 * double-precision associative functions share a common subr type and
 * are differentiated by the function pointer stored in the cdr of the
 * subr: for a list accessor, the function pointer is 0 and the name
 * of the function is used to determine which list accessor is being
 * applied; for a double-precision associative function, the function
 * pointer is not 0 and points to the C function of two arguments that
 * can be called repeatedly in the case that more than two arguments
 * are provided.  Associative predicates return #t for 0 or 1 argument
 * of appropriate type and contain a function pointer to a C function
 * of two arguments that can be called repeatedly to evaluate the
 * predicate for two or more arguments.  When only one argument is
 * provided, the return value is always #t, but the C function is
 * called anyway -- passing that one argument twice.  The single
 * argument is type-checked by the C function, but the C function's
 * return value is ignored.
 *
 * Tail-recursive calls from ceval are handled by a goto.
 * 
 * `apply' of closures is built-in to the interpreter to achieve
 * reasonable performance and to make the interpreter properly
 * tail-recursive.  It makes a tail-recursive call to "eval" and
 * therefore, is built-in to ceval as the virtual instruction
 * `#@apply'.  (If apply were a separate function that ceval had to
 * call, then tail-calls to apply would not be properly tail
 * recursive.)  The procedure named `apply' is written in Scheme and
 * is implemented using the macro `@apply' which expands to use
 * `#@apply'.  There are C functions scm_apply and scm_apply3 that are
 * used to call out from C to Scheme, but these are not the Scheme
 * procedure `apply'.
 *
 * `apply' of subrs (built-in procedures) works by calling
 * `scm_apply3'.  Subrs do not make stack-conserving tail-calls so
 * nothing special needs to be built-in to eval.  `scm_apply3' is used
 * to avoid duplicating the code it contains in eval.
 *
 * (The Scheme procedure `eval' should be implemented similarly, but
 * is not.  Therefore, Scheme programs which make tail-calls to `eval'
 * may be iterations which nevertheless consume an unbounded amount of
 * space in their execution.  [This has been partly fixed
 * already...finish it!])
 *
 * A global list of scm_debug_frame structures is stored in the
 * stack-root field "debug_info" There is one frame per active call to
 * eval or apply3.  The frame contains enough information to construct
 * a backtrace for the Scheme debugger and to be able to modify some
 * of the local variables of ceval or apply3 from Scheme.  (Note that
 * apply3 is used for call-outs from C to Scheme.  The `apply' which
 * Scheme programs use is built-in to ceval.)
 *
 * On entry to ceval and apply3, two local variables are set on the
 * basis of two global variables:
 *
 * 	local_errbrk_flag
 *	 = ((SCM_BOOL_F != SCM_CDR (g_debug_on_exception_flag))
 *	    || stepbrk_flag);
 * 	local_stepbrk_flag = stepbrk_flag;
 *
 * local_stepbrk_flag can be modified by Scheme programs (particularly by
 * programs that implement a debugger).
 *
 * If local_errbrk_flag is not 0, then an exception handler (of type
 * #t) is established.  In the event of an exception, the handler
 * calls out to a debugger hook (`errbrk') before rethrowing the
 * exception.
 *
 * At the beginning of each call or tail-call to ceval,
 * local_stepbrk_flag is checked.  If it is not 0, then a debugger
 * hook (`stepbrk') is called and given a chance to produce a return
 * value.  If it doesn't produce a return value, evaluation proceeds
 * normally.
 *
 * Each time local_stepbrk_flag is checked, it is copied to another
 * local variable: local_retbrk_flag.  local_retbrk_flag can also be
 * modified by Scheme programs.  At each tail-recursive invocation of
 * ceval, if local_retbrk_flag is set, the debugger hook `tailbrk' is
 * called and, like `stepbrk', given a chance to produce a return
 * value.  At each return from ceval, if local_retbrk_flag is set, the
 * hook `retbrk' is called.
 *
 * apply3, the function that initiates most call-outs from C to
 * Scheme, is similarly equipped.  It has local variables
 * local_errbrk_flag and local_retbrk_flag.  local_retbrk_flag can be
 * modified by Scheme programs.
 *
 * On entry to apply3, if local_errbrk_flag is set, a catch handler is
 * established.  If the global stepbrk_flag is true, then
 * `apply-stepbrk' is called.  On return from apply, if
 * local_retbrk_flag is set, then `apply-repbrk' is called.
 */

/************************************************************************
 *(h1 "Rationale -- Evaluation")
 * 
 * 
 * One of the design goals of Systas is:
 * 
 * \Interactive Use/ Systas should include whatever features are
 * desirable for *interactive use*.  In particular, it should be a
 * pleasant experience to extend, modify, and examine a Systas
 * application *as it is running.*
 * 
 * Thus, `eval' is needed.   
 * 
 * `eval2' provides everything needed to implement most module systems
 * I can think of.
 * 
 * `eval-environment' is strictly experimental.
 * 
 * There is no rationale for the debugging procedures -- some design
 * work is needed.
 */
