/* scheme.c - macros for Scheme
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998 Free Software Foundation,Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stddef.h>
#include "systas/libsystas/scheme.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/scheme.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/promise.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/macros.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/macros.h"
#include "systas/libsystas/variable.h"


/************************************************************************
 *(h0 "Special Forms")
 *
 */


SCM_SYMBOL (s_pair_p, "pair?");
SCM_SYMBOL (s_noop, "noop");
SCM_SYMBOL (s_car, "car");
SCM_SYMBOL (s_cdr, "cdr");
SCM_SYMBOL (s_set_car_x, "set-car!");
SCM_SYMBOL (s_set_cdr_x, "set-cdr!");
SCM_SYMBOL (s_expression, "missing or extra expression");
SCM_SYMBOL (s_test, "bad test");
SCM_SYMBOL (s_body, "bad body");
SCM_SYMBOL (s_bindings, "bad bindings");
SCM_SYMBOL (s_variable, "bad variable");
SCM_SYMBOL (s_clauses, "bad or missing clauses");
SCM_SYMBOL (s_bad_else, "bad ELSE clause");
SCM_SYMBOL (s_bad_recipient, "bad recipient");
SCM_SYMBOL (s_formals, "bad formals");
SCM_SYMBOL (s_bad_lambda, "bad lambda");
SCM_SYMBOL (s_quote, "quote");
SCM_SYMBOL (s_strong_quote, "strong-quote");
SCM_SYMBOL (s_begin, "begin");
SCM_SYMBOL (s_set_x, "set!");
SCM_SYMBOL (s_and, "and");
SCM_SYMBOL (s_or, "or");
SCM_SYMBOL (s_case, "case");
SCM_SYMBOL (s_cond, "cond");
SCM_SYMBOL (s_lambda, "lambda");
SCM_SYMBOL (s_let_star, "let*");
SCM_SYMBOL (s_if, "if");
SCM_SYMBOL (s_define, "define");
SCM_SYMBOL (s_do, "do");
SCM_SYMBOL (s_let, "let");
SCM_SYMBOL (s_quasiquote, "quasiquote");
SCM_SYMBOL (s_strong_quasiquote, "strong-quasiquote");
SCM_SYMBOL (s_the_environment, "the-environment");
SCM_SYMBOL (s_delay, "delay");
SCM_SYMBOL (s_at_apply, "@apply");
SCM_SYMBOL (s_at_eval, "@eval");
SCM_SYMBOL (s_at_eval_x, "@eval!");
SCM_SYMBOL (s_at_eval2, "@eval2");
SCM_SYMBOL (s_at_eval2_x, "@eval2!");
SCM_SYMBOL (s_at_call_with_current_continuation, "@call-with-current-continuation");
SCM_SYMBOL (s_procedure_print_name, "procedure-print-name");

SCM_KEYWORD (kw_optionals, "optional");



SCM scm_i_dot;
SCM scm_i_quote;
SCM scm_i_strong_quote;
SCM scm_i_quasiquote;
SCM scm_i_strong_quasiquote;
SCM scm_i_lambda;
SCM scm_i_let;
SCM scm_i_arrow;
SCM scm_i_else;
SCM scm_i_unquote;
SCM scm_i_uq_splicing;
SCM scm_i_name;



#define ASSYNT(_cond, _arg, _pos, _subr) if(!(_cond))scm_wta(_arg, _pos, _subr);
#define ASRTSYNTAX(cond_, msg_) if(!(cond_))scm_wta(xorig, (msg_), what);

static SCM
bodycheck (SCM xorig, SCM *bodyloc, SCM what)
{
  SCM_INTS_ENABLED;

  ASRTSYNTAX (scm_ilength (*bodyloc) >= 1, s_expression);
  return SCM_BOOL_F;
}


/* static SCM scm_unpaint (SCM quoted);
 * 
 * Replace by side-effect all occurences of variables in the
 * list/vector structure `quoted' with the symbolic names of the
 * variables.
 */
static SCM 
scm_unpaint (SCM quoted)
{
  if (SCM_IS_IMMEDIATE (quoted))
    return quoted;
  if (SCM_CONSP (quoted))
    {
      SCM_CAR (quoted) = scm_unpaint (SCM_CAR (quoted));
      SCM_CDR (quoted) = scm_unpaint (SCM_CDR (quoted));
      return quoted;
    }
  else if (scm_is_vector (quoted))
    {
      SCM * elts;
      int x;
      elts = SCM_VECTOR_ELTS (quoted);
      x = SCM_LENGTH (quoted);
      while (x--)
	elts[x] = scm_unpaint (elts[x]);
      return quoted;
    }
  else if (SCM_VARIABLEP (quoted))
    {
      return SCM_VARIABLE_NAME (quoted);
    }
  else
    return quoted;
}


/*(c quote)
 * (quote expresion)
 *
 * Return `expresion', unevaluated, substituting variable names for
 * first-class variables in `expression'.  Quote expressions can be
 * written using the special syntax:
 *
 *	'expression
 *
 * as in:
 *
 *	'(this list is not evaluated)
 *
 * See also `strong-quote'.
 */
SCM 
scm_m_quote (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, s_quote);
  SCM_CADR (xorig) = scm_unpaint (SCM_CADR (xorig));
  return scm_cons (SCM_IM_QUOTE, SCM_CDR (xorig));
}


/*(c strong-quote)
 * (strong-quote expresion)
 *
 * Return `expresion' completely unevaluated.  `strong-quote' expressions
 * can be written using the special syntax:
 *
 *	#'expression
 *
 * as in:
 *
 *	#'(this list is not evaluated)
 */
SCM 
scm_m_strong_quote (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, s_strong_quote);
  return scm_cons (SCM_IM_QUOTE, SCM_CDR (xorig));
}


/*(c begin)
 * (begin expression . expressions)
 *
 * Evaluate the expressions in order, returning the value of
 * the last expression.
 */
SCM 
scm_m_begin (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (SCM_CDR (xorig)) >= 1, xorig, s_expression, s_begin);
  return scm_cons (SCM_IM_BEGIN, SCM_CDR (xorig));
}


/*(c if)
 * (if condition then-expression else-expression)
 *
 * Evaluate `condition'.  If it returns a true value, evaluate
 * `the-expression' and return its value.  Otherwise, evaluate 
 * `else-expression' and return its value.
 */
SCM 
scm_m_if (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  int len;

  len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 2 && len <= 3, xorig, s_expression, s_if);
  return scm_cons (SCM_IM_IF, SCM_CDR (xorig));
}


/*(c set!)
 * (set! variable expression . rest)
 *
 * Evaluate `expression' and bind its value to `variable'.
 * `variable' is not evaluated.
 *
 * More than one variable and expression may be provided:
 *
 *	;; assign 1 to a, 2 to b and 3 to c
 *	;;
 *	(set! a 1 b 2 c 3)
 */
SCM 
scm_m_set_x (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  SCM x;
  int len;

  x = SCM_CDR (xorig);
  len = scm_ilength (x);
  ASSYNT ((len > 0) && !(len & 1), xorig, s_expression, s_set_x);

  {
    SCM y;
    y = x;
    while (len)
      {
	ASSYNT ((   !SCM_IS_IMMEDIATE (SCM_CAR (y))
		 && (   scm_is_symbol (SCM_CAR (y))
		     || SCM_VARIABLEP (SCM_CAR (y)))),
		xorig,
		s_variable,
		s_set_x);
	y = SCM_CDR (SCM_CDR (x));
	len -= 2;
      }
  }

  return scm_cons (SCM_IM_SET, x);
}


/*(c and)
 * (and . expressions)
 *
 * Evaluate the expressions in order, stopping if one of them
 * returns #f.  Return the value of the last expression evaluated.
 */
SCM 
scm_m_and (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  int len;

  len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, s_and);
  if (len >= 1)
    return scm_cons (SCM_IM_AND, SCM_CDR (xorig));
  else
    return SCM_BOOL_T;
}

/*(c and=>)
 * (and=> value thunk)
 * 
 * This is simply:
 * 
 * 	(define (and=> value thunk) (and value (thunk value)))
 * 
 * It saves you from having to invent a variable name for `value'.
 */



/*(c or)
 * (or . expressions)
 *
 * Evaluate the expressions in order, stopping if one of them
 * returns a value other than #f.  Return the value of the last 
 * expression evaluated.
 */
SCM 
scm_m_or (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  int len;

  len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, s_or);
  if (len >= 1)
    return scm_cons (SCM_IM_OR, SCM_CDR (xorig));
  else
    return SCM_BOOL_F;
}


/*(c case)
 * (case expression . cases)
 *
 * Each case must be of the form:
 *
 *	(values . body)
 * 
 * Each `values' is a list of unevaluated values or the symbol `else'.
 *
 * Evaluate `expression'.  Search for its value in the values lists,
 * in order, comparing values with `eqv?'.  If a matching value is
 * found, evaluate the expressions of the corresponding `body' and
 * return the value of the last expression evaluated.
 *
 * If the `values' part of a case is `else', immediate evaluate
 * the corresponding body and return the value of the last expression
 * evaluated.
 */
SCM 
scm_m_case (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM proc;
  SCM x;

  x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) >= 2, xorig, s_clauses, s_case);
  while (!SCM_IS_IMMEDIATE (x = SCM_CDR (x)))
    {
      proc = SCM_CAR (x);
      ASSYNT (scm_ilength (proc) >= 2, xorig, s_clauses, s_case);
      ASSYNT (scm_ilength (SCM_CAR (proc)) >= 0 || scm_i_else == SCM_CAR (proc),
	      xorig, s_clauses, s_case);
    }
  return scm_cons (SCM_IM_CASE, SCM_CDR (xorig));
}


/*(c cond)
 * (cond . cases)
 *
 * Each case must be of the form:
 *
 *	(test . body)
 *
 * or
 *
 *	(test => procedure-expression)
 *
 * Evaluate the tests, in order, until one returns a value other than #f.
 * Evaluate the expressions of the corresponding `body' and return the
 * value of the last expression. 
 *
 * If the case is of the `=>' form, then evaluate `procedure-expression' to 
 * yield a procedure and apply that procedure to the value returned from `test'.
 * Return the value of the procedure.
 */
SCM 
scm_m_cond (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM arg1;
  SCM x;
  int len;

  x = SCM_CDR (xorig);
  len = scm_ilength (x);

  ASSYNT (len >= 1, xorig, s_clauses, s_cond);
  while (!SCM_IS_IMMEDIATE (x))
    {
      arg1 = SCM_CAR (x);
      len = scm_ilength (arg1);
      ASSYNT (len >= 1, xorig, s_clauses, s_cond);
      if (scm_i_else == SCM_CAR (arg1))
	{
	  ASSYNT ((SCM_EOL == SCM_CDR (x)) && (len >= 2), xorig, s_bad_else, s_cond);
	  SCM_CAR (arg1) = SCM_BOOL_T;
	}
      if (len >= 2 && scm_i_arrow == SCM_CAR (SCM_CDR (arg1)))
	ASSYNT (3 == len && !SCM_IS_IMMEDIATE (SCM_CAR (SCM_CDR (SCM_CDR (arg1)))),
		xorig, s_bad_recipient, s_cond);
      x = SCM_CDR (x);
    }
  return scm_cons (SCM_IM_COND, SCM_CDR (xorig));
}


/* static SCM scm_capture_variable (SCM var, SCM form);
 * 
 * Replace by side effects all occurences of `var' in the 
 * list structure form by the symbolic name of variable `var'.
 */
static SCM
scm_capture_variable (SCM var, SCM form)
{
  if (SCM_IS_IMMEDIATE (form))
    return form;
  else if (SCM_BOOL_F != scm_eqv_p (var, form))
    return SCM_VARIABLE_NAME (form);
  else if (SCM_CONSP (form))
    {
      SCM_CAR (form) = scm_capture_variable (var, SCM_CAR (form));
      SCM_CDR (form) = scm_capture_variable (var, SCM_CDR (form));
      return form;
    }
  else
    return form;
}



/*(c extended-formals->scheme)
 * (extended-formals->scheme lambda-exp)
 * 
 * Convert a list of formal parameters from extended syntax to ordinary
 * Scheme syntax. 
 *
 *   I.	(lambda ((a b ...) x y ...) <body>)
 *
 *  	=>
 *
 *	(lambda (a b ...)
 *	  (lambda (x y ...)
 *	    <body>))
 *
 *
 *  II.	(lambda (a b :optional c d . e)
 *	  <body>)
 *
 *      =>
 * 
 *   	(lambda (a b . optionals)
 *	  (let ((c (and (pair? optionals)
 *		        (noop (car optionals)
 *			      (set! optionals (cdr optionals)))))
 *		(d (and (pair? optionals)
 *			(noop (car optionals)
 *			      (set! optionals (cdr optionals)))))
 *		(e optionals))
 *	    <body>))
 */
SCM_PROC (s_extended_formals_to_scheme, "extended-formals->scheme", 1, 0, 0, scm_extended_formals_to_scheme);
SCM
scm_extended_formals_to_scheme (SCM lambda_exp)
{
  SCM_INTS_ENABLED;
  SCM answer;
  SCM error_exp;
  SCM error_desc;
  int was_error;
  SCM formals;
  SCM body;
  int l;
  SCM optionals;
  SCM new_var;
  SCM param_getter;


  was_error = 0;
  answer = SCM_BOOL_F;

  SCM_DEFER_INTS;
  {
  retry_unnested:
    if (scm_ilength (lambda_exp) < 3)
      {
	error_exp = lambda_exp;
	error_desc = s_formals;
	goto handle_error;
      }

    formals = SCM_CADR (lambda_exp);
    body = SCM_CDDR (lambda_exp);

    l = scm_sloppy_ilength (formals);
    if (l <= 0)
      {
	answer = lambda_exp;
	goto return_answer;
      }

    if (SCM_CONSP (SCM_CAR (formals)))
      {
	lambda_exp = scm_listify (s_lambda,
				  SCM_CAR (formals),
				  scm_cons (s_lambda, scm_cons (SCM_CDR (formals), body)),
				  SCM_UNDEFINED);
	goto retry_unnested;
      }

    optionals = scm_memq (kw_optionals, formals);
    if (SCM_BOOL_F == optionals)
      {
	answer = lambda_exp;
	goto return_answer;
      }

    new_var = SCM_CAR (scm_hash_table_intern (SCM_BOOL_F, "optionals", sizeof ("optionals") - 1, 1, 0));

    {
      SCM * p;
      p = & formals;
      while (*p != optionals)
	p = &SCM_CDR (*p);
      *p = new_var;
    }

    optionals = SCM_CDR (optionals);
    if (SCM_EOL == optionals)
      {
	error_exp = lambda_exp;
	error_desc = s_formals;
	goto handle_error;
      }

    param_getter = scm_listify (s_and,
				scm_listify (s_pair_p,  new_var, SCM_UNDEFINED),
				scm_listify (s_noop,
					     scm_listify (s_car, new_var, SCM_UNDEFINED),
					     scm_listify (s_set_x, new_var, scm_listify (s_cdr, new_var, SCM_UNDEFINED), SCM_UNDEFINED),
					     SCM_UNDEFINED),
				SCM_UNDEFINED);



    {
      SCM l;
      SCM let_variables;
      SCM * p;
      l = optionals;
      let_variables = SCM_EOL;
      p = &let_variables;
      while (1)
	{
	  if (SCM_EOL == l)
	    break;
	  if (SCM_NCONSP (l))
	    {
	      *p = scm_cons (scm_listify (l, new_var, SCM_UNDEFINED), SCM_EOL);
	    break;
	    }
	  *p = scm_cons (scm_listify (SCM_CAR (l), param_getter, SCM_UNDEFINED), SCM_EOL);
	  p = &SCM_CDR (*p);
	  l = SCM_CDR (l);
	}

      answer = scm_cons  (s_lambda,
			  scm_listify (formals,
				       scm_cons (s_let, scm_cons (let_variables, body)),
				       SCM_UNDEFINED));

    return_answer:
      goto leave_critical_section;
      
    }

  handle_error:
    was_error = 1;
    
  leave_critical_section:
  }
  SCM_ALLOW_INTS;
  if (was_error)
    scm_wta (error_exp, error_desc, s_extended_formals_to_scheme);
  return answer;
}


/*(c lambda)
 * (lambda formals . body)
 *
 * Return a procedure.  `formals' and `body' are not evaluated.
 * `formals' may be of one the forms:
 *
 *	a		;; Accept any number of parameters, 
 *			;; binding a list of them to `a'
 *	(a b c ...)	;; Accept the indicated number of parameters
 * 			;; binding the first to `a', the second to `b',
 *			;; etc.
 *
 *	(a b ... . z)   ;; Accept any number of parameters, binding the
 * 			;; first to `a', the second to `b' etc. and 
 *			;; binding all remaining parameters as a list
 *			;; to `z'.
 *
 * When invoked with a correct number of parameters, the expressions
 * of `body' are evaluated in order in a context with variable bindings as
 * described above.  The value of the last expression of `body' is returned.
 */
SCM 
scm_m_lambda (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM formals;
  SCM x;

  xorig = scm_extended_formals_to_scheme (xorig);
  x = SCM_CDR (xorig);

  if (scm_ilength (x) < 2)
    scm_wta (xorig, s_bad_lambda, s_lambda);
  formals = SCM_CAR (x);
  if (SCM_EOL == formals)
    goto memlambda;
  if (SCM_IS_IMMEDIATE (formals))
    goto badforms;
  if (SCM_VARIABLEP (formals))
    {
      SCM_CDR (x) = scm_capture_variable (formals, SCM_CDR (x));
      formals = SCM_VARIABLE_NAME (formals);
      SCM_CAR (x) = formals;
    }
  if (scm_is_symbol (formals))
    goto memlambda;
  if (SCM_NCONSP (formals))
    goto badforms;
  while (!SCM_IS_IMMEDIATE (formals))
    {
      if (SCM_NCONSP (formals))
	{
	  if (!scm_is_symbol (formals))
	    goto badforms;
	  else
	    goto memlambda;
	}
      if (SCM_VARIABLEP (SCM_CAR (formals)))
	{
	  SCM_CDR (x) = scm_capture_variable (SCM_CAR (formals), SCM_CDR (x));
	  SCM_CAR (formals) = SCM_VARIABLE_NAME (SCM_CAR (formals));
	}
      if (!scm_is_symbol (SCM_CAR (formals)))
	goto badforms;
      if (!SCM_IS_IMMEDIATE (SCM_CDR (formals)) && SCM_VARIABLEP (SCM_CDR (formals)))
	{
	  SCM_CDR (x) = scm_capture_variable (SCM_CDR (formals), SCM_CDR (x));
	  SCM_CDR (formals) = SCM_VARIABLE_NAME (SCM_CDR (formals));
	}
      formals = SCM_CDR (formals);
    }
  if (SCM_EOL != formals)
    {
    badforms:
      scm_wta (xorig, s_formals, s_lambda);
    }
memlambda:
  bodycheck (xorig, &SCM_CDR (x), s_lambda);
  return scm_cons (SCM_IM_LAMBDA, SCM_CDR (xorig));
}


/*(c let*)
 * (let* bindings . body)
 *
 * Bindings must be of the form:
 *
 *	((variable init-expression) ...)
 *
 * Each `init-expression' is evaluated in order and its 
 * value bound to the corresponding `variable' (which is not evaluated).
 * Each `init-expression' is evaluated in a scope in containing the 
 * preceeding variable bindings.  
 *
 * The expressions of `body' are evaluated in order and the value
 * of the last expression evaluated is returned.  The body is evaluated
 * in the scope of all of the variables of the `let'.
 */
SCM 
scm_m_letstar (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM x;
  SCM arg1st;
  SCM proc;
  SCM vars;
  SCM * varloc;
  int len;

  x = SCM_CDR (xorig);
  vars = SCM_EOL;
  varloc = &vars;
  len = scm_ilength (x);
  ASSYNT (len >= 2, xorig, s_body, s_let_star);
  proc = SCM_CAR (x);
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, s_let_star);
  while (!SCM_IS_IMMEDIATE (proc))
    {
      arg1st = SCM_CAR (proc);
      ASSYNT (2 == scm_ilength (arg1st), xorig, s_bindings, s_let_star);
      if (!SCM_IS_IMMEDIATE (SCM_CAR (arg1st)) && SCM_VARIABLEP (SCM_CAR (arg1st)))
	{
	  SCM_CDR (proc) = scm_capture_variable (SCM_CAR (arg1st), SCM_CDR (proc));
	  SCM_CDR (x) = scm_capture_variable (SCM_CAR (arg1st), SCM_CDR (x));
	  SCM_CAR (arg1st) = SCM_VARIABLE_NAME (SCM_CAR (arg1st));
	}
      ASSYNT (scm_is_symbol (SCM_CAR (arg1st)),
	      xorig,
	      s_variable,
	      s_let_star);
      *varloc = scm_cons2 (SCM_CAR (arg1st), SCM_CAR (SCM_CDR (arg1st)), SCM_EOL);
      varloc = &SCM_CDR (SCM_CDR (*varloc));
      proc = SCM_CDR (proc);
    }
  x = scm_cons (vars, SCM_CDR (x));
  bodycheck (xorig, &SCM_CDR (x), s_let_star);
  return scm_cons (SCM_IM_LETSTAR, x);
}


/*(c letrec)
 * (letrec bindings . body)
 *
 * Bindings must be of the form:
 *
 *	((variable init-expression) ...)
 *
 * Each `init-expression' is evaluated in order and its 
 * value bound to the corresponding `variable' (which is not 
 * evaluated).  The expressions are evaluated in the scope of
 * all of the variables of the `let*'.  If an expression references
 * a value before its value is established, an error is signalled.
 *
 * The expressions of `body' are evaluated in order and the value
 * of the last expression evaluated is returned.  The body is evaluated
 * in the scope of all of the variables of the `letrec'.
 */
SCM 
scm_m_letrec (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM what;
  SCM cdrx;			/* locally mutable version of form */
  SCM x;			/* structure traversers */
  SCM proc;
  SCM arg1st;
  SCM vars;
  SCM inits;
  SCM * initloc;

  what = SCM_CAR (xorig);
  cdrx = SCM_CDR (xorig);	/* locally mutable version of form */
  x = cdrx;
  vars = SCM_EOL;
  inits = SCM_EOL;
  initloc = &inits;

  ASRTSYNTAX (scm_ilength (x) >= 2, s_body);
  proc = SCM_CAR (x);
  if (SCM_EOL == proc)
    return scm_m_letstar (xorig, env);	/* null binding, let* faster */
  ASRTSYNTAX (scm_ilength (proc) >= 1, s_bindings);
  do
    {
      /* vars list reversed here, inits reversed at evaluation
       */
      arg1st = SCM_CAR (proc);
      ASRTSYNTAX (2 == scm_ilength (arg1st), s_bindings);
      if (!SCM_IS_IMMEDIATE (SCM_CAR (arg1st)) && SCM_VARIABLEP (SCM_CAR (arg1st)))
	x = scm_capture_variable (SCM_CAR (arg1st), x);
      ASRTSYNTAX (scm_is_symbol (SCM_CAR (arg1st)),
		  s_variable);
      vars = scm_cons (SCM_CAR (arg1st), vars);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1st)), SCM_EOL);
      initloc = &SCM_CDR (*initloc);
    }
  while (!SCM_IS_IMMEDIATE (proc = SCM_CDR (proc)));
  cdrx = scm_cons2 (vars, inits, SCM_CDR (x));
  bodycheck (xorig, &SCM_CDR (SCM_CDR (cdrx)), what);
  return scm_cons (SCM_IM_LETREC, cdrx);
}


/*(c let)
 * (let bindings . body)
 *
 * Bindings must be of the form:
 *
 *	((variable init-expression) ...)
 *
 * Each `init-expression' is evaluated in order and its 
 * value bound to the corresponding `variable' (which is not 
 * evaluated).  The expressions are evaluated outside of the scope of
 * all of the variables of the `let'. 
 *
 * The expressions of `body' are evaluated in order and the value
 * of the last expression evaluated is returned.  The body is evaluated
 * in the scope of all of the variables of the `letrec'.
 */
SCM 
scm_m_let (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM what;
  SCM cdrx;			/* locally mutable version of form */
  SCM x;			/* structure traversers */
  SCM proc;			/* structure traversers */
  SCM arg1st;			/* structure traversers */
  SCM name;			/* structure traversers */
  SCM vars;
  SCM inits;
  SCM * varloc;
  SCM * initloc;

  what = SCM_CAR (xorig);
  cdrx = SCM_CDR (xorig);
  x = cdrx;
  vars = SCM_EOL;
  inits = SCM_EOL;
  varloc = &vars;
  initloc = &inits;

  ASSYNT (scm_ilength (x) >= 2, xorig, s_body, s_let);
  proc = SCM_CAR (x);
  if ((SCM_EOL == proc)
      || (!SCM_IS_IMMEDIATE (proc) && SCM_CONSP (proc)
	  && !SCM_IS_IMMEDIATE (SCM_CAR (proc)) && SCM_CONSP (SCM_CAR (proc)) && (SCM_EOL == SCM_CDR (proc))))
    return scm_m_letstar (xorig, env);	/* null or single binding, let* is faster */
  ASSYNT (!SCM_IS_IMMEDIATE (proc), xorig, s_bindings, s_let);
  if (SCM_CONSP (proc))			/* plain let, proc is <bindings> */
    {
      ASRTSYNTAX (scm_ilength (proc) >= 1, s_bindings);
      do
	{
	  /* vars list reversed here, inits reversed at evaluation
	   */
	  arg1st = SCM_CAR (proc);
	  ASRTSYNTAX (2 == scm_ilength (arg1st), s_bindings);
	  if (!SCM_IS_IMMEDIATE (SCM_CAR (arg1st)) && SCM_VARIABLEP (SCM_CAR (arg1st)))
	    {
	      SCM_CDR (x) = scm_capture_variable (SCM_CAR (arg1st), SCM_CDR (x));
	      SCM_CAR (arg1st) = SCM_VARIABLE_NAME (SCM_CAR (arg1st));
	    }
	  ASRTSYNTAX (scm_is_symbol (SCM_CAR (arg1st)), s_variable);
	  vars = scm_cons (SCM_CAR (arg1st), vars);
	  *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1st)), SCM_EOL);
	  initloc = &SCM_CDR (*initloc);
	}
      while (!SCM_IS_IMMEDIATE (proc = SCM_CDR (proc)));
      cdrx = scm_cons2 (vars, inits, SCM_CDR (x));
      bodycheck (xorig, &SCM_CDR (SCM_CDR (cdrx)), what);
      return scm_cons (SCM_IM_LET, cdrx);
    }
  if (SCM_VARIABLEP (proc))
    {
      x = scm_capture_variable (proc, x);
      proc = SCM_CAR (x);
    }
  if (!scm_is_symbol (proc))
    scm_wta (xorig, s_bindings, s_let);	/* bad let */
  name = proc;			/* named let, build equiv letrec */
  x = SCM_CDR (x);
  ASSYNT (scm_ilength (x) >= 2, xorig, s_body, s_let);
  proc = SCM_CAR (x);		/* bindings scm_list */
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, s_let);
  while (!SCM_IS_IMMEDIATE (proc))
    {				/* vars and inits both in order */
      arg1st = SCM_CAR (proc);
      ASSYNT (2 == scm_ilength (arg1st), xorig, s_bindings, s_let);
      if (!SCM_IS_IMMEDIATE (SCM_CAR (arg1st)) && SCM_VARIABLEP (SCM_CAR (arg1st)))
	{
	  scm_capture_variable (SCM_CAR (arg1st), SCM_CDR (x));
	  SCM_CAR (arg1st) = SCM_VARIABLE_NAME (SCM_CAR (arg1st));
	}
      ASSYNT (scm_is_symbol (SCM_CAR (arg1st)),
	      xorig, s_variable, s_let);
      *varloc = scm_cons (SCM_CAR (arg1st), SCM_EOL);
      varloc = &SCM_CDR (*varloc);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1st)), SCM_EOL);
      initloc = &SCM_CDR (*initloc);
      proc = SCM_CDR (proc);
    }
  return
    scm_m_letrec (scm_cons2 (scm_i_let,
			     scm_cons (scm_cons2 (name,
						  scm_cons2 (scm_i_lambda, vars, SCM_CDR (x)),
						  SCM_EOL),
				       SCM_EOL),
			     scm_acons (name, inits, SCM_EOL)), 	/* body */
		  env);
}


/*(c do)
 * (do loop-variables return-condition . body)
 *
 * `loop-variables' must be of the form:
 *
 *	((variable-name init-expression step-expression) ...)
 *
 * `return-condition' must be of the form:
 *
 *	(test-expression . body)
 *
 * Each of the init expressions is evaluated in order to produce
 * values that will be bound to the corresponding loop variables.
 *
 * Each iteration begins by evaluating `test-expression' in the
 * context of the loop variable bindings.  If it returns a true
 * value, the expressions of the return-condition body are evaluated
 * in order and the value of the last expression is returned.
 *
 * If the test expression returns #f, then the expressions of the
 * body are evaluated in order and then the step expressions
 * are evaluated to produce bindings for the loop variables for the
 * next iteration.  The loop begins again, with the evaluation of the
 * test expression.
 */
SCM 
scm_m_do (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM x = SCM_CDR (xorig);
  SCM arg1st;
  SCM proc;
  SCM vars;
  SCM inits;
  SCM steps;
  SCM captures;
  SCM * initloc;
  SCM * steploc;
  int len;


  /* DO gets the most radically altered syntax
   * (do ((<var1> <init1> <step1>)
   *      (<var2> <init2>)
   *      ... )
   *     (<test> <return>)
   *     <body>)
   *
   * ;; becomes
   *
   * (do_mem (varn ... var2 var1)
   * 	     (<init1> <init2> ... <initn>)
   * 	     (<test> <return>)
   * 	     (<body>)
   * 	     <step1> <step2> ... <stepn>) ;; missing steps replaced by var
   */
  x = SCM_CDR (xorig);
  vars = SCM_EOL;
  inits = SCM_EOL;
  steps = SCM_EOL;
  captures = SCM_EOL;
  initloc = &inits;
  steploc = &steps;
  len = scm_ilength (x);

  ASSYNT (len >= 2, xorig, s_test, s_do);
  proc = SCM_CAR (x);
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, s_do);
  while (!SCM_IS_IMMEDIATE (proc))
    {
      arg1st = SCM_CAR (proc);
      len = scm_ilength (arg1st);
      ASSYNT (2 == len || 3 == len, xorig, s_bindings, s_do);
      if (!SCM_IS_IMMEDIATE (SCM_CAR (arg1st)) && SCM_VARIABLEP (SCM_CAR (arg1st)))
	{
	  captures = scm_cons (SCM_CAR (arg1st), captures);
	  SCM_CAR (arg1st) = SCM_VARIABLE_NAME (SCM_CAR (arg1st));
	}
      ASSYNT (scm_is_symbol (SCM_CAR (arg1st)),
	      xorig, s_variable, s_do);
      /* vars reversed here, inits and steps reversed at evaluation */
      vars = scm_cons (SCM_CAR (arg1st), vars);	/* variable */
      arg1st = SCM_CDR (arg1st);
      *initloc = scm_cons (SCM_CAR (arg1st), SCM_EOL);	/* init */
      initloc = &SCM_CDR (*initloc);
      arg1st = SCM_CDR (arg1st);
      *steploc = scm_cons (SCM_IS_IMMEDIATE (arg1st) ? SCM_CAR (vars) : SCM_CAR (arg1st),
			   SCM_EOL);	/* step */
      steploc = &SCM_CDR (*steploc);
      proc = SCM_CDR (proc);
    }
  x = SCM_CDR (x);
  {
    SCM v;
    for (v = captures; !SCM_IS_IMMEDIATE (v); v = SCM_CDR (v))
      {
	x = scm_capture_variable (SCM_CAR (v), x);
	steps = scm_capture_variable (SCM_CAR (v), steps);
      }
  }
  ASSYNT (scm_ilength (SCM_CAR (x)) >= 1, xorig, s_test, s_do);
  x = scm_cons2 (SCM_CAR (x), SCM_CDR (x), steps);
  x = scm_cons2 (vars, inits, x);
  bodycheck (xorig, &SCM_CAR (SCM_CDR (SCM_CDR (x))), s_do);
  return scm_cons (SCM_IM_DO, x);
}


/* iqq
 * 
 * quasi-quote internal helper function.
 *
 * `form' is what has been quasi-quoted.
 * `env' is the evaluation context.
 * `depth' is the number of quasiquotes we are embedded in.
 *
 * If `depth' is 1, then unquote causes the unquoted form
 * to be evaluated.
 */
static SCM 
iqq (SCM form, SCM env, int depth, int strong)
{
  SCM_INTS_ENABLED;
  SCM tmp;
  int edepth;

  edepth = depth;

  if (SCM_IS_IMMEDIATE (form))
    return form;
  if (SCM_VARIABLEP (form))
    {
      if (strong)
	return form;
      else
	return SCM_VARIABLE_NAME (form);
    }
  if (scm_is_vector (form))
    {
      long i;
      SCM *data;

      i = SCM_LENGTH (form);
      data = SCM_VECTOR_ELTS (form);
      tmp = SCM_EOL;
      for (; --i >= 0;)
	tmp = scm_cons (data[i], tmp);
      return scm_vector (iqq (tmp, env, depth, strong));
    }
  if (SCM_NCONSP (form))
    return form;
  tmp = SCM_CAR (form);
  if (!SCM_IS_IMMEDIATE (tmp) && SCM_VARIABLEP (tmp))
    tmp = SCM_VARIABLE_NAME (tmp);
  if ((scm_i_quasiquote == tmp) || (scm_i_strong_quasiquote == tmp))
    {
      depth++;
      goto label;
    }
  if (scm_i_unquote == tmp)
    {
      --depth;
    label:
      form = SCM_CDR (form);
      /* !!! might need a check here to be sure that form isn't a struct. */
      SCM_ASSERT (!SCM_IS_IMMEDIATE (form) && SCM_ECONSP (form) && (SCM_EOL == SCM_CDR (form)),
		  form, scm_arg1, s_quasiquote);
      if (0 == depth)
	return scm_evalcar (form, env);
      return scm_cons2 (tmp, iqq (SCM_CAR (form), env, depth, strong), SCM_EOL);
    }
  {
    int splice;

    SCM_DEFER_INTS;
    splice = (   !SCM_IS_IMMEDIATE (tmp)
	      && (   (scm_i_uq_splicing == SCM_CAR (tmp))
		  || (   !SCM_IS_IMMEDIATE (SCM_CAR (tmp))
		      && SCM_VARIABLEP (SCM_CAR (tmp))
		      && (scm_i_uq_splicing == SCM_VARIABLE_NAME (SCM_CAR (tmp))))));
    SCM_ALLOW_INTS;
    if (splice)
      {
	tmp = SCM_CDR (tmp);
	if (0 == --edepth)
	  return scm_list_append (scm_cons2 (scm_evalcar (tmp, env),
					     iqq (SCM_CDR (form), env, depth, strong), SCM_EOL));
      }
  }
  return scm_cons (iqq (SCM_CAR (form), env, edepth, strong),
		   iqq (SCM_CDR (form), env, depth, strong));
}


/* Here are acros which return values rather than code. */

/*(c quasiquote)
 * (quasiquote template)
 *
 * Quasiquote is a convenient short-hand for writing expressions that
 * compose the functions and syntax `list', `vector', `cons', and
 * `quote'.
 * 
 * Put another way, it is a convenient way to write expressions
 * denoting "almost constant" values.
 * 
 * `template' should be a list (possibly improper but non-circular) or
 * vector.  The template is recursively copied with two exceptions.
 * 
 * If an element has the form `(unquote exp)', it is replaced by
 * the result of evaluating `exp'.  A one element list beginning
 * with `unquote' can be written using a comma:
 *
 *	,exp	;; The same as ,(exp)
 * 
 * If the final element of a proper list has the form
 * `(unquote-splicing exp)', then the value of evaluating `exp'
 * becomes the tail of the list.   A one element list beginning
 * with `unquote-splicing' can be written using the special symbol
 * `,@':
 *
 *	,@exp	;; The same as (unquote-splicing exp)
 * 
 * Here are some expamples that illustrate quasiquotation:
 * 
 * 	(let ((x 5))
 * 	  `(1 (+ 2 3) ,(+ 4 x)))
 * 	
 * 	=> (1 (+ 2 3) 9)
 * 	
 * 	(let ((x 5))
 * 	  `(1 (+ 2 3) #(,(+ 4 x) a b)))
 * 	
 * 	=> (1 (+ 2 3) #(9 a b))
 * 	
 * 	
 * 	(let ((naughts '(bowman tichy)))
 * 	  `(kelvin ,@ naughts))
 * 	
 * 	=> (kelvin bowman tichy)
 * 
 * Quasiquotes nest, so:
 * 
 * 	;; Note that the inner quasiquote prevents
 * 	;; the outer quasiquote from evaluating one
 * 	;; level of unquotes:
 * 	;;
 * 	
 * 	`(1 2 `(3 ,(+ 4 ,(+ 1 5))))
 * 	=>
 * 	(1 2 (quasiquote (3 (unquote (+ 4 6)))))
 */
SCM 
scm_m_quasiquote (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  SCM x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) == 1, xorig, s_expression, s_quasiquote);
  return iqq (SCM_CAR (x), env, 1, 0);
}


/*(c strong-quasiquote)
 * (strong-quasiquote exp)
 * 
 * Like `quasiquote' except that quoted objects are quoted with `strong-quote'
 * instead of `quote'.
 */
SCM 
scm_m_strong_quasiquote (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM x;

  x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) == 1, xorig, s_expression, s_strong_quasiquote);
  return iqq (SCM_CAR (x), env, 1, 1);
}


/*(c the-environment)
 * (the-environment)
 * 
 * Return the current environment.  This is a dangerous procedure
 * since corruptions to the environment can hopelessly confuse and
 * even crash the interpreter.
 */
SCM 
scm_m_the_environment (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;
  SCM x;

  x = SCM_CDR (xorig);
  ASSYNT (SCM_EOL == x, xorig, s_expression, s_the_environment);
  return scm_cons (SCM_IM_THE_ENV, SCM_EOL);
}


/*(c delay)
 * (delay expression)
 *
 * Promises are expressions whose evaluation is postponed until the
 * result it will yield is needed.  The result of a promise may be
 * requested more than once, but the expression is evaluated only once.
 * (*Note: `force'.)
 */
SCM 
scm_m_delay (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (xorig) == 2, xorig, s_expression, s_delay);
  xorig = SCM_CDR (xorig);
  return (scm_make_promise
	  (scm_make_closure (scm_cons2 (SCM_EOL, SCM_CAR (xorig), SCM_CDR (xorig)),
			     env)));
}


/* env_top_level
 * 
 * Return the top-level environment associated with the lexical
 * scope represented by `env'.
 */
static SCM
env_top_level (SCM env)
{
  SCM_INTS_UNKNOWN;

  while (!SCM_IS_IMMEDIATE(env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR(env)))
	return SCM_CAR(env);
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}


/*(c define)
 * (define name-and-formals . definition)
 *
 * Create a top-level binding for `name' to the value of `expression'.
 * 
 * There are three forms for `define':
 *
 *	(define name expression)
 *	(define (name . formals) . body)
 *	(define ((name . formals-1) . formals2) . body)
 *
 * The second form of define:
 *
 * 	(define (name arg1 arg2 ...)  exp1 exp2 ...)
 * 
 * is equivalent to:
 * 
 * 	(define name (lambda (arg1 arg2 ...) exp1 exp2...))
 * 
 * The form
 * 
 * 	(define ((name arg1 ...) arg2 ...)  exp1 exp2 ...)
 * 
 * means:
 * 
 * 	(define (name arg1...) (lambda (arg2 ...) exp1 exp2...))
 * 
 * The nesting can go on arbitrarily:
 * 
 * 	(define ((((name arg1...) arg2...) arg3...) arg4...) exp1 exp2 ...)
 * 	
 * 	;; is like:
 * 	
 * 	(define (name arg1...)
 * 	   (lambda (arg2...)
 * 	      (lambda (arg3...)
 * 	         (lambda (arg4 ...)
 * 	            exp1
 * 	            exp2
 * 	            ...))))
 */
SCM 
scm_m_define (SCM x, SCM env)
{
  SCM_INTS_ENABLED;
  SCM proc;
  SCM arg1;

  arg1 = x;
  x = SCM_CDR (x);
  ASSYNT (scm_ilength (x) >= 2, arg1, s_expression, s_define);
  proc = SCM_CAR (x);
  x = SCM_CDR (x);
  while (!SCM_IS_IMMEDIATE (proc) && SCM_CONSP (proc))
    {				/* nested define syntax */
      x = scm_cons (scm_cons2 (scm_i_lambda, SCM_CDR (proc), x), SCM_EOL);
      proc = SCM_CAR (proc);
    }
  ASSYNT (scm_is_symbol (proc), arg1, s_variable, s_define);
  ASSYNT (1 == scm_ilength (x), arg1, s_expression, s_define);
  if (SCM_IS_TOP_LEVEL (env))
    {
      x = scm_evalcar (x, env);
      arg1 = scm_symbol_to_vcell (proc, env_top_level (env), SCM_BOOL_T);

      if (!SCM_IS_IMMEDIATE (x) && SCM_CLOSUREP (x))
	scm_set_procedure_property_x (x, s_procedure_print_name, proc);
      SCM_CDR (arg1) = x;
      return scm_cons2 (scm_i_quote, SCM_CAR (arg1), SCM_EOL);
    }
  return scm_cons2 (SCM_IM_DEFINE, proc, x);
}


/*(c apply)
 * (apply function . arguments)
 * 
 * Apply `function' to the `arguments'.  The last argument provided is special:
 * it should be a list.  Each member of the list is treated as an individual
 * argument.
 * 
 * For example:
 * 
 * 	(apply + 1 2 '(3 4 5)) => 15
 */
SCM 
scm_m_apply (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2, xorig, s_expression, s_at_apply);
  return scm_cons (SCM_IM_APPLY, SCM_CDR (xorig));
}

/*c @eval)
 * (@eval form)
 * SCM scm_m_eval (SCM expression, SCM env);
 *
 * This function is used internally to implement `eval'.
 */
SCM 
scm_m_eval (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (xorig) == 2, xorig, s_expression, s_at_eval);
  return scm_cons (SCM_IM_EVAL, SCM_CDR (xorig));
}

/*c @eval!)
 * (@eval! form)
 * SCM scm_m_eval_x (SCM expression, SCM env);
 *
 * This function is used internally to implement `eval!'.
 */
SCM 
scm_m_eval_x (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (xorig) == 2, xorig, s_expression, s_at_eval_x);
  return scm_cons (SCM_IM_EVAL_X, SCM_CDR (xorig));
}


/*c @eval2)
 * (@eval2 form)
 * SCM scm_m_eval2 (SCM expression, SCM env);
 *
 * This function is used internally to implement `eval2'.
 */
SCM 
scm_m_eval2 (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (xorig) == 3, xorig, s_expression, s_at_eval2);
  return scm_cons (SCM_IM_EVAL2, SCM_CDR (xorig));
}

/*c @eval2!)
 * (@eval2! form)
 * SCM scm_m_eval2_x (SCM expression, SCM env);
 *
 * This function is used internally to implement `eval2!'.
 */
SCM 
scm_m_eval2_x (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (xorig) == 3, xorig, s_expression, s_at_eval2_x);
  return scm_cons (SCM_IM_EVAL2_X, SCM_CDR (xorig));
}


/*c @call-with-current-continuation)
 * (@call-with-current-continuation proc)
 * SCM scm_m_cont (SCM expression, SCM env);
 *
 * This function is used internally to implement `call-with-current-continuation'.
 * Use `call-with-current-continuation', not this function.
 */
SCM 
scm_m_cont (SCM xorig, SCM env)
{
  SCM_INTS_ENABLED;

  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig,
	  s_expression, s_at_call_with_current_continuation);
  return scm_cons (SCM_IM_CONT, SCM_CDR (xorig));
}


#ifndef SCM_RECKLESS
/* scm_badargsp
 * 
 * Compare a parameter specification to a list of provided
 * arguments.  Return 0 if a correct number of parameters 
 * were provided, 1 otherwise.
 */
int 
scm_badargsp (SCM formals, SCM args)
{
  SCM_INTS_UNKNOWN;

  while (!SCM_IS_IMMEDIATE (formals))
    {
      if (SCM_NCONSP (formals))
	return 0;
      if (SCM_IS_IMMEDIATE (args))
	return 1;
      formals = SCM_CDR (formals);
      args = SCM_CDR (args);
    }
  return (SCM_EOL == args) ? 0 : 1;
}
#endif





/*c defined?)
 * (defined? variable)
 * SCM scm_m_defined_p (SCM expression, SCM env);
 *
 * Return #t if `variable' (which is unevaluated) is bound,
 * #f if it is undefined. 
 */
SCM 
scm_m_defined_p (SCM x, SCM env)
{
  SCM_INTS_UNKNOWN;
  SCM proc;

  proc = SCM_CAR (x = SCM_CDR (x));
  if (SCM_ISYMP (proc))
    return SCM_BOOL_T;
  else if (!scm_is_symbol (proc))
    return SCM_BOOL_F;
  else
    {
      SCM vcell = scm_symbol_to_vcell (proc, env_top_level(env), SCM_BOOL_F);
      return (vcell == SCM_BOOL_F || SCM_UNBNDP(SCM_CDR(vcell))) ? SCM_BOOL_F : SCM_BOOL_T;
    }
}



void
scm_init_scheme (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/scheme.x"

  scm_i_dot = SCM_CAR (scm_intern_symhash (".", SCM_UNDEFINED));
  scm_i_arrow = SCM_CAR (scm_intern_symhash ("=>", SCM_UNDEFINED));
  scm_i_else = SCM_CAR (scm_intern_symhash ("else", SCM_UNDEFINED));
  scm_i_unquote = SCM_CAR (scm_intern_symhash ("unquote", SCM_UNDEFINED));
  scm_i_quasiquote = scm_make_synt ("quasiquote", scm_procedure_to_syntax,
				    (SCM (*)())scm_m_quasiquote);
  scm_i_strong_quasiquote = scm_make_synt ("strong-quasiquote",
					   scm_procedure_to_syntax,
					   (SCM (*)())scm_m_strong_quasiquote);
  scm_i_uq_splicing = SCM_CAR (scm_intern_symhash ("unquote-splicing", SCM_UNDEFINED));
  scm_i_name = SCM_CAR (scm_intern_symhash ("name", SCM_UNDEFINED));
  scm_i_lambda = scm_make_synt ("lambda", scm_procedure_to_memoizing_macro,
				(SCM (*)())scm_m_lambda);
  scm_i_let = scm_make_synt ("let", scm_procedure_to_memoizing_macro,
			     (SCM (*)())scm_m_let);
  scm_i_quote = scm_make_synt ("quote", scm_procedure_to_memoizing_macro,
			       (SCM (*)())scm_m_quote);
  scm_i_strong_quote = scm_make_synt ("strong-quote", scm_procedure_to_memoizing_macro,
				     (SCM (*)())scm_m_strong_quote);
  scm_permanent_object (scm_i_name);

  scm_make_synt ("the-environment", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_the_environment);
  scm_make_synt ("define", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_define);
  scm_make_synt ("delay", scm_procedure_to_syntax,
		 (SCM (*)())scm_m_delay);
  scm_make_synt ("and", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_and);
  scm_make_synt ("begin", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_begin);
  scm_make_synt ("case", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_case);
  scm_make_synt ("cond", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_cond);
  scm_make_synt ("do", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_do);
  scm_make_synt ("if", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_if);
  scm_make_synt ("letrec", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_letrec);
  scm_make_synt ("let*", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_letstar);
  scm_make_synt ("or", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_or);
  scm_make_synt ("set!", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_set_x);
  scm_make_synt ("@apply", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_apply);
  scm_make_synt ("@eval", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_eval);
  scm_make_synt ("@eval_x", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_eval_x);
  scm_make_synt ("@eval2", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_eval2);
  scm_make_synt ("@eval2_x", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_eval2_x);
  scm_make_synt ("@call-with-current-continuation", scm_procedure_to_memoizing_macro,
		 (SCM (*)())scm_m_cont);
  scm_make_synt ("defined?", scm_procedure_to_syntax,
		 (SCM (*)())scm_m_defined_p);
}


/************************************************************************
 *h1 "Special Forms Internals")
 *
 * The macros defined here make the following transformations:
 *
 *	(quote value)
 *	=> (#@quote value)
 *
 *	(if c e1 e2) 
 *	=> (#@if c e1 e2)
 *
 *	(begin e1 e2 ...) 
 *	=> (#@begin e1 e2 ...)
 *
 *	(set! v1 e1 v2 e2 ...) 
 *	=> (#@set! v1 e1 v2 e2 ...)
 *
 *	(and e1 e2 ...) 
 *	=> (#@and e1 e2 ...)
 *
 *	(or e1 e2 ...) 
 *	=> (#@or e1 e2 ...)
 *
 *	(case e c1 c2 ...) 
 *	=> (#@case e c1 c2 ...) 
 *
 *	(cond c1 c2 ...) 
 *	=> (#@cond c1 c2 ...) 
 *
 *	(lambda formals . body) 
 *	=> (#@lambda formals . body)
 *
 *	(let* ((v1 e1) (v2 e2) (v3 e3) ...) . body)
 *	=> (#@let* (v1 e1 v2 e2 v3 e3 ...) . body)
 *
 *	(letrec ((v1 e1) (v2 e2) (v3 e3) ...) . body)
 *	=> (#@letrec (... v3 v2 v1) (e1 e2 e3 ...) . body)
 *
 *	(let ((v1 e1) (v2 e2) (v3 e3) ...) . body)
 *	=> (#@let (... v3 v2 v1) (e1 e2 e3 ...) . body)
 *	(let label ((v1 e1) (v2 e2) (v3 e3) ...) . body)
 *	=> (#@letrec (label) ((#@lambda (v1 v2 v3 ...) . body) e1 e2 e3 ...))
 * 
 *	(do ((v1 i1 s1) (v2 i2 s2) (v3 i3 s3)) (c . ret-body) . body)
 *	=> (#@do (...v3 v2 v1) (i1 i2 i3 ...) (c . ret-body) body . (s1 s2 s3 ...))
 *
 *	quasiquote is syntax -- it returns a the value of the `delay' 
 *	expression, not transformed code.
 *
 *	delay is syntax -- it returns a the value of the `delay' 
 *	expression, not transformed code.
 *
 *	define is expanded only for internal defines.  For top-level defines,
 *	the function scm_m_define does all the work.
 *
 *	(define a b)
 *	=> (#@define a b)
 *	(define (a v1 v2 ... . vn) . body)
 *	=> (#@define a (#@lambda (v1 v2 ... . vn) . body))
 *	(define ((a v1 v2 ...) w1 w2 ...) . body)
 *	=> (#@define a (#@lambda (v1 v2 ...) (lambda (w1 w2 ...) . body)))
 *	etc.
 *
 *	(call-with-current-continuation e) 
 *	=> (#@call-with-current-continuation e)
 *
 * 	defined? is syntax -- it returns a the value of the `define?' 
 *	expression, not transformed code.
 */
