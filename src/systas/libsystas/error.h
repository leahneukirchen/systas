/* error.h - decls for signalling scheme errors
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__ERROR_H
#define INCLUDE__LIBSYSTAS__ERROR_H

#include "systas/libsystas/scm.h"



/* The exception type for errors thrown from
 * most built-in functions.
 */
extern SCM scm_parameter_error;

/* An exception type for certain errors thrown from eval.
 */
extern SCM scm_eval_error;

/* An exception type for certain errors thrown from eval.
 */
extern SCM scm_unknown_procedure;

/* An exception type when the stack becomes too deep.
 */
extern SCM scm_stack_depth;

/* Standard error messages for use as the first argument 
 * to scm_parameter_error via SCM_ASSERT or scm_wta.
 */
#define SCM_STANDARD_ERROR_MESSAGES \
	SCM_STANDARD_ERROR_MESSAGE (scm_argn, "wrong type argument"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_arg1, "wrong type argument in position 1"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_arg2, "wrong type argument in position 2"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_arg3, "wrong type argument in position 3"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_arg4, "wrong type argument in position 4"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_arg5, "wrong type argument in position 5"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_wna, "wrong number of arguments"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_bad_kw_arg, "bad argument with keyword"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_missing_kw_arg, "missing keyword argument"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_ovflow, "numerical overflow"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_outofrange, "argument out of range"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_nalloc, "could not allocate"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_stack_depth_exceeded, "maximum stack depth exceeded"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_exit, "exit (internal error?)."); \
	SCM_STANDARD_ERROR_MESSAGE (scm_internal, "internal programming error"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_end_of_file_err, "end of file in "); \
	SCM_STANDARD_ERROR_MESSAGE (scm_unexpected_rparen, "unexpected \")\""); \
	SCM_STANDARD_ERROR_MESSAGE (scm_unknown_pound, "unknown # object"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_bad_vector_syntax, "bad vector syntax"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_empty_keyword, "empty keyword"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_missing_rparen, "missing close paren"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_strange_symbol, "strangely interned symbol? "); \
	SCM_STANDARD_ERROR_MESSAGE (scm_uninterned_symbol, "uninterned symbol? "); \
	SCM_STANDARD_ERROR_MESSAGE (scm_unbound_variable, "unbound variable"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_damaged_environment, "damaged environment"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_wrong_type_to_apply, "wrong type to apply"); \
	SCM_STANDARD_ERROR_MESSAGE (scm_bogus_continuation, "continuation from wrong top level");


#undef SCM_STANDARD_ERROR_MESSAGE
#define SCM_STANDARD_ERROR_MESSAGE(c_name, string) extern SCM c_name
SCM_STANDARD_ERROR_MESSAGES



#ifndef SCM_RECKLESS

/* If _cond is 0, then signal an error of type `system-error'
 * with arguments:
 *
 *		_msg _arg _subr
 *
 * _arg may be SCM_UNDEFINED.
 */
#define SCM_ASSERT(_cond, _arg, _msg, _subr) \
	do \
	  { \
	    if (!(_cond)) \
              scm_wta ((_arg), (_msg), (_subr)); \
	  } while (0)

#else
#define SCM_ASSERT(_cond, _arg, _msg, _subr) /* RECKLESS */
#endif



/* automatically generated __STDC__ prototypes */
extern void scm_err_escape (int delay_it, SCM condition, SCM msg, SCM elt, ...);
extern void scm_wta (SCM arg, SCM msg, SCM subr);
extern void scm_delayed_error (SCM arg, SCM msg, SCM subr);
extern void scm_panic (char * desc);
extern void scm_init_error (void);
#endif

