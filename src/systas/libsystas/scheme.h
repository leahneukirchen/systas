/* scheme.h - macros for scheme
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__SCHEME_H
#define INCLUDE__LIBSYSTAS__SCHEME_H

#include "systas/libsystas/scm.h"



extern SCM scm_i_dot;
extern SCM scm_i_quote;
extern SCM scm_i_strong_quote;
extern SCM scm_i_quasiquote;
extern SCM scm_i_strong_quasiquote;
extern SCM scm_i_lambda;
extern SCM scm_i_let;
extern SCM scm_i_arrow;
extern SCM scm_i_else;
extern SCM scm_i_unquote;
extern SCM scm_i_uq_splicing;
extern SCM scm_i_name;



/* automatically generated __STDC__ prototypes */
extern SCM scm_m_quote (SCM xorig, SCM env);
extern SCM scm_m_begin (SCM xorig, SCM env);
extern SCM scm_m_if (SCM xorig, SCM env);
extern SCM scm_m_set_x (SCM xorig, SCM env);
extern SCM scm_m_and (SCM xorig, SCM env);
extern SCM scm_m_or (SCM xorig, SCM env);
extern SCM scm_m_case (SCM xorig, SCM env);
extern SCM scm_m_cond (SCM xorig, SCM env);
extern SCM scm_m_lambda (SCM xorig, SCM env);
extern SCM scm_m_letstar (SCM xorig, SCM env);
extern SCM scm_m_letrec (SCM xorig, SCM env);
extern SCM scm_m_let (SCM xorig, SCM env);
extern SCM scm_m_do (SCM xorig, SCM env);
extern SCM scm_m_quasiquote (SCM xorig, SCM env);
extern SCM scm_m_delay (SCM xorig, SCM env);
extern SCM scm_m_define (SCM x, SCM env);
extern SCM scm_m_apply (SCM xorig, SCM env);
extern SCM scm_m_eval (SCM xorig, SCM env);
extern SCM scm_m_eval_x (SCM xorig, SCM env);
extern SCM scm_m_eval2 (SCM xorig, SCM env);
extern SCM scm_m_eval2_x (SCM xorig, SCM env);
extern SCM scm_m_cont (SCM xorig, SCM env);
extern int scm_badargsp (SCM formals, SCM args);
extern SCM scm_m_defined_p (SCM x, SCM env);
extern void scm_init_scheme (void);
#endif  /* INCLUDE__LIBSYSTAS__SCHEME_H */
