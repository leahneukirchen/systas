/* procs.h - procedures
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__PROCS_H
#define INCLUDE__LIBSYSTAS__PROCS_H

#include "systas/libsystas/scm.h"
#include "systas/libsystas/root.h"



/* Most built-in procedures.
 */
typedef struct scm_subr
{
  long sname;
  SCM (*cproc)();
} scm_subr;


/* Built-ins that take one "double" argument and return a "double".
 */
typedef struct scm_dsubr
{
  long sname;
  double (*dproc)(double);
} scm_dsubr;

/* Return the name (as a Scheme string) of a built-in
 * procedure.
 */
#define SCM_SNAME(x) ((SCM_CAR(x)>>8)?(SCM)(scm_heap_org+(SCM_CAR(x)>>8)):scm_nullstr)

/* Return the C function (as a function pointer) of a built-in
 * procedure.
 */
#define SCM_SUBRF(x) (((scm_subr *)x)->cproc)

/* Return the C function (as a function pointer) of a built-in
 * "double" procedure.
 */
#define SCM_DSUBRF(x) (((scm_dsubr *)x)->dproc)

/* Return the C function (as a function pointer) of a closure whose
 * code is a built-in procedure.
 */
#define SCM_CCLO_SUBR(x) (SCM_VECTOR_ELTS(x)[0])

/* Is the non-immediate value "X" a closure?
 */
#define SCM_CLOSUREP(x)		(SCM_TYP3(x)==scm_tc3_closure)

/* What is the car of closure X (discarding the type tag)?
 */
#define SCM_CLOSCAR(x) 		(SCM_CAR(x)-scm_tc3_closure)

/* What is the code (a list) of closure X?
 */
#define SCM_CODE(x) 		SCM_CAR(SCM_CLOSCAR (x))

/* Set the the code of closure X.
 */
#define SCM_SETCODE(x, e) 	SCM_CAR(x) = (scm_cons ((e), SCM_EOL) + scm_tc3_closure)

/* What is the environment (a list) of closure X?
 */
#define SCM_ENV(x) 		SCM_CDR(x)

/* What is the property list associated with closure X?
 */
#define SCM_PROCPROPS(x) 	SCM_CDR(SCM_CLOSCAR (x))



/* automatically generated __STDC__ prototypes */
extern SCM scm_make_subr (char *name, int type, SCM (*fcn)(), int set);
extern SCM scm_make_closure (SCM code, SCM env);
extern SCM scm_makcclo (SCM proc, long len);
extern SCM scm_gsubr_apply (SCM args);
extern SCM scm_make_gsubr (char *name, int req, int opt, int rst, SCM (*fcn)(), int set);
extern SCM scm_procedure_p (SCM obj);
extern SCM scm_procedure_properties (SCM proc);
extern SCM scm_set_procedure_properties_x (SCM proc, SCM new_val);
extern SCM scm_procedure_property (SCM p, SCM k);
extern SCM scm_set_procedure_property_x (SCM p, SCM k, SCM v);
extern SCM scm_closure_p (SCM obj);
extern SCM scm_closure_code (SCM obj);
extern SCM scm_closure_environment (SCM obj);
extern SCM scm_noop (SCM args);
extern SCM scm_noop2 (SCM args);
extern SCM scm_noop3 (SCM args);
extern SCM scm_noop4 (SCM args);
extern void scm_init_procs (void);
#endif  /* INCLUDE__LIBSYSTAS__PROCS_H */
