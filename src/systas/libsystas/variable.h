/* variable.h - decls for first-class scheme variables
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#ifndef INCLUDE__LIBSYSTAS__VARIABLE_H
#define INCLUDE__LIBSYSTAS__VARIABLE_H

#include "systas/libsystas/scm.h"



extern int scm_tc16_variable;

/* Is it a variable?
 */
#define SCM_VARIABLEP(X)   		(scm_tc16_variable == SCM_TYP16(X))

enum scm_variable_type
{
  scm_global_variable = 0,	/* value in SCM_CDR (vcell) */
  scm_indirect_a_variable = 1,	/* value in SCM_CADR (vcell) */
  scm_indirect_d_variable = 2,	/* value in SCM_CDDR (vcell) */
};

#define SCM_VARIABLE_TYPE(V)		(((enum scm_variable_type)SCM_CAR(V))>>16)

#define SCM_SET_VARIABLE_TYPE(x, v, t)	(SCM_CAR(x) = ((v)<<16)+(t))

/* What is the value cell for this variable?
 */
#define SCM_VARIABLE_VCELL(V) 		SCM_CDR(V)

/* What is the value of this variable?
 */
#define SCM_VARIABLE_VALUE(V) 		((SCM_VARIABLE_TYPE (V) == scm_global_variable) \
					 ? SCM_CDR (SCM_VARIABLE_VCELL (V)) \
					 : ((SCM_VARIABLE_TYPE (V) == scm_indirect_a_variable) \
					    ? SCM_CADR (SCM_VARIABLE_VCELL (V)) \
					    : SCM_CDDR (SCM_VARIABLE_VCELL (V))))

#define SCM_VARIABLE_VALUE_LOC(V)	((SCM_VARIABLE_TYPE (V) == scm_global_variable) \
					 ? &SCM_CDR (SCM_VARIABLE_VCELL (V)) \
					 : ((SCM_VARIABLE_TYPE (V) == scm_indirect_a_variable) \
					    ? &SCM_CADR (SCM_VARIABLE_VCELL (V)) \
					    : &SCM_CDDR (SCM_VARIABLE_VCELL (V))))


/* Is this an undefined variable?
 */
#define SCM_UD_VARIABLEP(X) 		(SCM_VARIABLEP(X) \
					 && SCM_UNBNDP (SCM_VARIABLE_VALUE (X)))

/* Is this a defined variable?
 */
#define SCM_DEF_VARIABLEP(X) 		(SCM_VARIABLEP(X) \
					 && !SCM_UNBNDP (SCM_VARIABLE_VALUE (X)))

#define SCM_VARIABLE_NAME(X)		(SCM_CAR (SCM_CDR (X)))


/* automatically generated __STDC__ prototypes */
extern SCM scm_make_variable (SCM init, SCM name_hint);
extern SCM scm_construct_variable (enum scm_variable_type type, SCM name_hint, SCM cell);
extern SCM scm_make_undefined_variable (SCM name_hint);
extern SCM scm_variable_p (SCM obj);
extern SCM scm_variable_ref (SCM var);
extern SCM scm_variable_name (SCM var);
extern SCM scm_variable_set_x (SCM var, SCM val);
extern SCM scm_builtin_variable (SCM name);
extern SCM scm_variable_bound_p (SCM var);
extern void scm_init_variable (void);
#endif  /* INCLUDE__LIBSYSTAS__VARIABLE_H */
