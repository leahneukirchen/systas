/* procs.c - the scheme type `procedure?'
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


#include <stddef.h>
#include "systas/libsystas/error.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/strings.h" 
#include "systas/libsystas/hashtab.h" 
#include "systas/libsystas/weaks.h" 
#include "systas/libsystas/alist.h" 
#include "systas/libsystas/list.h"
#include "systas/libsystas/numbers.h"


/************************************************************************
 *(h0 "Procedures")
 *
 * Procedures are those objects which the evaluator knows how to apply to
 * arguments.  Built-in functions are procedures as are any functions
 * defined by programs.
 * 
 * A procedure is distinct from the name it is given in any particular context.  
 * Thus the symbol `'+' and the primitive procedure bound to the variable called
 * `+' are distinct objects.
 *
 * Every procedure has an associated property list (association list).  
 * The property "procedure-print-name", if it is not #f, is used when 
 * displaying or writing a procedure.
 */


SCM_SYMBOL (s_procedure_print_name, "procedure-print-name");
SCM_SYMBOL (s_compiled_closure, "compiled-closure");
SCM_SYMBOL (s_gsubr_apply, "gsubr-apply");

/* Construct an argument descriptor for a generalized built-in.
 */
#define GSUBR_MAKTYPE(req, opt, rst) ((req)|((opt)<<4)|((rst)<<8))

/* How many arguments does the generalized built-in X require?
 */
#define GSUBR_REQ(x) ((int)(x)&0xf)

/* How many optional arguments does the generalized built-in X permit?
 */
#define GSUBR_OPT(x) (((int)(x)&0xf0)>>4)

/* Does the generalized built-in X take a "rest" argument?
 */
#define GSUBR_REST(x) ((int)(x)>>8)

/* What is the maximum number of parameters to a generalized 
 * built-in?
 */
#define GSUBR_MAX 11

/* Return the argument descriptor of a generalized built-in.
 */
#define GSUBR_TYPE(cclo) (SCM_VECTOR_ELTS(cclo)[1])

/* Return the subr-handle of a generalized built-in.
 */
#define GSUBR_PROC(cclo) (SCM_VECTOR_ELTS(cclo)[2])

/* The built-in code of a compiled closure which is a generalized
 * built-in.
 */
static SCM f_gsubr_apply;



/* {Procedures}
 */


/* scm_make_subr
 * 
 * Construct a built-in procedure.
 *
 * `type' must be one of these tags (see tags.h)
 *
 *	scm_tc7_rpsubr
 *	scm_tc7_cxr
 *	scm_tc7_subr_3
 *	scm_tc7_subr_1
 *	scm_tc7_asubr
 *	scm_tc7_subr_1o
 *	scm_tc7_lsubr_1o
 *	scm_tc7_lsubr_2
 *	scm_tc7_lsubr
 *
 * If `set' is not 0, then bind the procedure to a top level variable
 * called `name' in the symhash table.
 */
SCM 
scm_make_subr (char *name, int type, SCM (*fcn)(), int set)
{
  SCM_INTS_DISABLED;
  SCM symcell;
  SCM tmp;
  SCM z;

  if (name)
    {
      symcell = scm_intern_symhash (name, SCM_UNDEFINED);
      tmp = ((((struct scm_cell *) (SCM_CAR (symcell))) - scm_heap_org) << 8);
      if ((tmp >> 8) != ((struct scm_cell *) (SCM_CAR (symcell)) - scm_heap_org))
	tmp = 0;
    }
  else
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_CAR (z) = tmp + type;
  if (name && set)
    SCM_CDR (symcell) = z;
  return z;
}


/* make-closure
 * 
 * Construct a closure from `code' and `environment'.
 */
SCM 
scm_make_closure (SCM code, SCM env)
{
  SCM_INTS_INDIFFERENT;
  SCM z;

  SCM_NEWCELL (z);
  SCM_ENV (z) = env;
  SCM_SETCODE (z, code);	/* can trigger GC */
  return z;
}


/* scm_makcclo
 * 
 * Construct a compiled closure.
 *
 * `proc' is a procedure that will be called with a list of
 * arguments if the compiled closure is called.  The first
 * argument is the compiled closure itself, the rest of the
 * arguments are parameters to the compiled closure.
 *
 * SCM_VECTOR_ELTS(cclo) is an array of SCM values.
 * SCM_VECTOR_ELTS(cclo)[0] is `proc'.
 *
 * Other elements (up to `len - 1') may be filled in by
 * the caller of scm_makcclo.
 */
SCM 
scm_makcclo (SCM proc, long len)
{
  SCM_INTS_ENABLED;
  SCM s;

  SCM_NEWCELL (s);
  SCM_DEFER_INTS;
  SCM_CDR (s) = (SCM)scm_must_malloc (len * sizeof (SCM));
  SCM_SET_LENGTH (s, len, scm_tc7_cclo);
  while (--len)
    SCM_VECTOR_ELTS (s)[len] = SCM_UNSPECIFIED;
  SCM_CCLO_SUBR (s) = proc;
  SCM_ALLOW_INTS;
  return s;
}


/* scm_gsubr_apply
 * 
 * The (anonymous) built-in procedure which is the code for
 * compiled closures constructed by scm_make_gsubr.
 */
SCM
scm_gsubr_apply (SCM args)
{
  SCM_INTS_ENABLED;
  SCM self;
  SCM v[11];
  SCM (*fcn)();
  int typ;
  int req;
  int opt;
  int rest;
  int i;

  self = SCM_CAR(args);
  fcn = SCM_SUBRF(GSUBR_PROC(self));
  typ = SCM_INUM(GSUBR_TYPE(self));
  req = GSUBR_REQ(typ);
  opt = GSUBR_OPT(typ);
  rest = GSUBR_REST(typ);

  args = SCM_CDR(args);

  for (i = 0; i < req; i++)
    {
      if (SCM_IS_IMMEDIATE(args))
	SCM_ASSERT (0, self, scm_wna, s_gsubr_apply); 
      v[i] = SCM_CAR(args);
      args = SCM_CDR(args);
    }
  for (; i < req + opt; i++)
    {
      if (!SCM_IS_IMMEDIATE(args))
	{
	  v[i] = SCM_CAR(args);
	  args = SCM_CDR(args);
	}
      else
	v[i] = SCM_UNDEFINED;
    }
  if (rest)
    v[i] = args;
  else if (SCM_EOL != args)
    SCM_ASSERT (0, self, scm_wna, s_gsubr_apply); 
  
  switch (req + opt + rest)
    {
    default: scm_wta(self, scm_internal, s_gsubr_apply);
    case 0: return (*fcn)();
    case 1: return (*fcn)(v[0]);
    case 2: return (*fcn)(v[0], v[1]);
    case 3: return (*fcn)(v[0], v[1], v[2]);
    case 4: return (*fcn)(v[0], v[1], v[2], v[3]);
    case 5: return (*fcn)(v[0], v[1], v[2], v[3], v[4]);
    case 6: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5]);
    case 7: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
    case 8: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
    case 9: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
    case 10: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
    case 11: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10]);
    }
}

/* scm_make_gsubr
 * 
 * Construct a procedure.
 *
 * `req' is the number of required arguments.
 * `opt' is the number of optional arguments.
 * `rest' is non-0 if the procedure accepts a list of "rest" arguments.
 * `set' is non-0 if a top-level binding should be created in the 
 * symhash table.
 */
SCM
scm_make_gsubr (char *name, int req, int opt, int rst, SCM (*fcn)(), int set)
{
  SCM_INTS_DISABLED;

  switch (GSUBR_MAKTYPE(req, opt, rst))
    {
    case GSUBR_MAKTYPE(0, 1, 0): return scm_make_subr (name, scm_tc7_subr_1o, fcn, set);
    case GSUBR_MAKTYPE(2, 0, 0): return scm_make_subr (name, scm_tc7_subr_2, fcn, set);
    case GSUBR_MAKTYPE(3, 0, 0): return scm_make_subr (name, scm_tc7_subr_3, fcn, set);
    case GSUBR_MAKTYPE(0, 0, 1): return scm_make_subr (name, scm_tc7_lsubr, fcn, set);
    case GSUBR_MAKTYPE(2, 0, 1): return scm_make_subr (name, scm_tc7_lsubr_2, fcn, set);
    default:
      {
	SCM symcell;
	SCM z;
	SCM cclo;
	long tmp;

	cclo = scm_makcclo(f_gsubr_apply, 3L);
	if (name)
	  {
	    symcell = scm_intern_symhash(name, SCM_UNDEFINED);
	    tmp = ((((struct scm_cell *)(SCM_CAR(symcell)))-scm_heap_org)<<8);
	    if ((tmp>>8) != ((struct scm_cell *)(SCM_CAR(symcell))-scm_heap_org))
	      tmp = 0;
	  }
	else
	  tmp = 0;
	if (GSUBR_MAX < req + opt + rst)
	  scm_panic ("too many args in scm_make_gsubr");
	SCM_NEWCELL(z);
	SCM_SUBRF(z) = (SCM (*)())fcn;
	SCM_CAR(z) = tmp + scm_tc7_asubr;
	GSUBR_PROC(cclo) = z;
	GSUBR_TYPE(cclo) = SCM_MAKINUM(GSUBR_MAKTYPE(req, opt, rst));
	if (name)
	  {
	    if (set)
	      SCM_CDR(symcell) = cclo;
	    scm_set_procedure_property_x (cclo,
					  s_procedure_print_name,
					  SCM_CAR (symcell));
	  }

	return cclo;
      }
    }
}




/*(c procedure?)
 * (procedure? obj)
 * SCM scm_procedure_p (SCM obj);
 * 
 * Return #t if `obj' is a procedure, #f otherwise.
 */
SCM_PROC(s_procedure_p, "procedure?", 1, 0, 0, scm_procedure_p);
SCM 
scm_procedure_p (SCM obj)
{
  SCM_INTS_UNKNOWN;

  if (!SCM_IS_IMMEDIATE (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_closures:
      case scm_tc7_contin:
      case scm_tcs_subrs:
      case scm_tc7_cclo:
	return SCM_BOOL_T;
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}


/* scm_stand_in_proc
 * 
 * `proc' is a built-in procedure (a subr or cclo).
 *
 * Return an ordinary anonymous closure with an empty body 
 * and environment, coextensive with `proc'.  The property
 * list of the closure serves as the property list for `proc'
 * because subrs and cclos don't have attached property lists.
 */
static SCM
scm_stand_in_proc (SCM proc)
{
  SCM_INTS_INDIFFERENT;
  SCM answer;

  answer = scm_hashq_ref (scm_stand_in_procs, proc, SCM_BOOL_F);

  if (answer == SCM_BOOL_F)
    {
      answer = scm_make_closure (scm_listify (SCM_EOL, SCM_BOOL_F, SCM_UNDEFINED),
					   SCM_EOL);
      scm_stand_in_procs = scm_hashq_set_x (scm_stand_in_procs, proc, answer);
    }

  return answer;
}


/*(c procedure-properties)
 * (procedure-properties procedure)
 * SCM scm_procedure_properties (SCM proc);
 * 
 * Return the property list associated with `procedure'.
 */
SCM_PROC(s_procedure_properties, "procedure-properties", 1, 0, 0, scm_procedure_properties);
SCM
scm_procedure_properties (SCM proc)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_procedure_p (proc), proc, scm_arg1, s_procedure_properties);
  if (!(!SCM_IS_IMMEDIATE (proc) && SCM_CLOSUREP (proc)))
    proc = scm_stand_in_proc(proc);
  return SCM_PROCPROPS (proc);
}


/*(c set-procedure-properties!)
 * (set-procedure-properties! procedure list)
 * SCM scm_set_procedure_property_x (SCM proc, SCM new_val);
 * 
 * Set the property list associated with `procedure'.
 */
SCM_PROC(s_set_procedure_properties_x,
	 "set-procedure-properties!", 2, 0, 0, scm_set_procedure_properties_x);
SCM
scm_set_procedure_properties_x (SCM proc, SCM new_val)
{
  SCM_INTS_ENABLED;

  if (!(!SCM_IS_IMMEDIATE (proc) && SCM_CLOSUREP (proc)))
    proc = scm_stand_in_proc(proc);
  SCM_ASSERT (!SCM_IS_IMMEDIATE (proc) && SCM_CLOSUREP (proc), proc, scm_arg1, s_set_procedure_properties_x);
  SCM_PROCPROPS (proc) = new_val;
  return SCM_UNSPECIFIED;
}


/*(c procedure-property)
 * (procedure-property procedure property-name)
 * SCM scm_procedure_property (SCM proc, SCM property);
 * 
 * Return the named property (or #f) of `procedure'.
 */
SCM_PROC(s_procedure_property, "procedure-property", 2, 0, 0, scm_procedure_property);
SCM
scm_procedure_property (SCM p, SCM k)
{
  SCM_INTS_ENABLED;
  SCM assoc;

  if (!(!SCM_IS_IMMEDIATE (p) && SCM_CLOSUREP (p)))
    p = scm_stand_in_proc(p);
  SCM_ASSERT (scm_procedure_p (p), p, scm_arg1, s_procedure_property);
  assoc = scm_assq (k, SCM_PROCPROPS (p));
  return (!SCM_IS_IMMEDIATE (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}


/*(c set-procedure-property!)
 * (set-procedure-property! procedure property-name value)
 * SCM scm_set_procedure_properties_x (SCM proc,
 *				       SCM key,
 *				       SCM value);
 * 
 * Set the named property of `procedure'.
 */
SCM_PROC(s_set_procedure_property_x,
	 "set-procedure-property!", 3, 0, 0, scm_set_procedure_property_x);
SCM
scm_set_procedure_property_x (SCM p, SCM k, SCM v)
{
  SCM_INTS_ENABLED;
  SCM assoc;

  if (!(!SCM_IS_IMMEDIATE (p) && SCM_CLOSUREP (p)))
    p = scm_stand_in_proc(p);
  SCM_ASSERT (!SCM_IS_IMMEDIATE (p) && SCM_CLOSUREP (p), p, scm_arg1, s_set_procedure_property_x);

  if (!(!SCM_IS_IMMEDIATE (p) && SCM_CLOSUREP (p)))
    p = scm_stand_in_proc(p);
  assoc = scm_assq (k, SCM_PROCPROPS (p));
  if (!SCM_IS_IMMEDIATE (assoc))
    SCM_CDR (assoc) = v;
  else
    SCM_PROCPROPS (p) = scm_acons (k, v, SCM_PROCPROPS (p));
  return SCM_UNSPECIFIED;
}


/*(c closure?)
 * (closure? obj)
 * 
 * You probably don't want to use this function.  Use `procedure?'
 * instead.  This function is for internal use.
 *
 * Return #t of `obj' is a closure, #f otherwise.
 */
SCM_PROC (s_closure_p, "closure?", 1, 0, 0, scm_closure_p);
SCM
scm_closure_p (SCM obj)
{
  SCM_INTS_UNKNOWN;

  return (!SCM_IS_IMMEDIATE (obj) && SCM_CLOSUREP (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c closure-code)
 * (closure-code obj)
 * 
 * This function is for internal use.  It returns the code
 * of a closure.
 */
SCM_PROC (s_closure_code, "closure-code", 1, 0, 0, scm_closure_code);
SCM
scm_closure_code (SCM obj)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (obj) && SCM_CLOSUREP (obj), obj, scm_arg1, s_closure_code);
  
  return SCM_CODE (obj);
}


/*(c closure-environment)
 * (closure-environment obj)
 * 
 * This function is for internal use.  It returns the environment
 * of a closure.
 */
SCM_PROC (s_closure_environment, "closure-environment", 1, 0, 0, scm_closure_environment);
SCM
scm_closure_environment (SCM obj)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (obj) && SCM_CLOSUREP (obj), obj, scm_arg1, s_closure_environment);
  return SCM_ENV (obj);
}



/*(c scm_noop)
 * (noop . args)
 * 
 * Do nothing.  Return the first argument or #f.
 */
SCM_PROC(s_first_value, "first-value", 0, 0, 1, scm_noop);
SCM_PROC(s_noop, "noop", 0, 0, 1, scm_noop);
SCM
scm_noop (SCM args)
{
  SCM_INTS_INDIFFERENT;

  return ((SCM_EOL == args)
	  ? SCM_BOOL_F
	  : SCM_CAR (args));
}


/*(c noop2)
 * (noop2 . args)
 * 
 * Do nothing.  Return the second argument or #f.
 */
SCM_PROC(s_second_value, "second-value", 0, 0, 1, scm_noop2);
SCM_PROC(s_noop2, "noop2", 0, 0, 1, scm_noop2);
SCM
scm_noop2 (SCM args)
{
  SCM_INTS_INDIFFERENT;

  return (((SCM_EOL == args) || (SCM_EOL == SCM_CDR (args)))
	  ? SCM_BOOL_F
	  : SCM_CADR (args));
}


/*(c noop3)
 * (noop3 . args)
 * 
 * Do nothing.  Return the third argument or #f.
 */
SCM_PROC(s_third_value, "third-value", 0, 0, 1, scm_noop3);
SCM_PROC(s_noop3, "noop3", 0, 0, 1, scm_noop3);
SCM
scm_noop3 (SCM args)
{
  SCM_INTS_INDIFFERENT;

  return ((   (SCM_EOL == args)
	   || (SCM_EOL == SCM_CDR (args))
	   || (SCM_EOL == SCM_CDDR (args)))
	  ? SCM_BOOL_F
	  : SCM_CADDR (args));
}


/*(c noop4)
 * (noop4 . args)
 * 
 * Do nothing.  Return the fourth argument or #f.
 */
SCM_PROC(s_fourth_value, "fourth-value", 0, 0, 1, scm_noop4);
SCM_PROC(s_noop4, "noop4", 0, 0, 1, scm_noop4);
SCM
scm_noop4 (SCM args)
{
  SCM_INTS_INDIFFERENT;

  return ((   (SCM_EOL == args)
	   || (SCM_EOL == SCM_CDR (args))
	   || (SCM_EOL == SCM_CDDR (args))
	   || (SCM_EOL == SCM_CDDDR (args)))
	  ? SCM_BOOL_F
	  : SCM_CADDDR (args));
}




void
scm_init_procs (void)
{
  SCM_INTS_DISABLED;

  f_gsubr_apply = scm_make_subr ("gsubr-apply", scm_tc7_lsubr, (SCM (*)())scm_gsubr_apply, 1);
  scm_permanent_object (f_gsubr_apply);
  scm_stand_in_procs = scm_make_weak_key_hash_table (SCM_MAKINUM (61));
#include "systas/libsystas/procs.x"
}


/************************************************************************
 * h1 "Procedures Internals")
 *
 * There are several representations for procedures, all non-immediate
 * values.
 *
 * Built-in procedures:
 *
 *  ...name hint.....scm_tc7_???????  .......SCM (*function)()........
 *
 * The name-hint is a 24-bit offfset within the heap to a symbol name 
 * that is the suggested name of the built-in.  The name-hint may be 0 
 * if there is no symbol within range or if no name-hint was provided
 * when the procedure was constructed.
 *
 * Possible tags (see "tags.h") are:
 *
 *	scm_tc7_rpsubr
 *	scm_tc7_cxr
 *	scm_tc7_subr_3
 *	scm_tc7_subr_1
 *	scm_tc7_asubr
 *	scm_tc7_subr_1o
 *	scm_tc7_lsubr_1o
 *	scm_tc7_lsubr_2
 *	scm_tc7_lsubr
 *
 *
 * Closures whose code is a built-in function:
 *
 *  ...args.desc,,,,....scm_tc7_cclo  ..........SCM * velts...........
 *
 * `args desc' is a 24 bit integer that describes how many required
 * and optional parameters are expected, and whether additional "rest"
 * arguments are accepted.   See the macro GSUBR_MAKTYPE.
 *
 * `velts' is an array whose 0 element points to a procedure tagged
 * scm_tc7_asubr.  That procedure's function implements the closure
 * (and so scm_tc7_asubr doesn't really indicate how many arguments
 * that function takes -- the `args desc' tells that).  Other elements
 * of velts are filled in when the closure is created and represent
 * the "closed over" values.
 *
 *
 * Closures constructed by (lambda ...):
 *
 *  ...........SCM code............1  ..........SCM environment........
 *
 * The code is a list:
 *
 *		( formals-and-body . procedure-properties )
 *
 * `body' is a list:
 *
 *		( formals . body )
 *
 * The meaning of the closure is as if constructed in the appropriate
 * environment by:
 *
 *		(lambda formals . body)
 * 
 * `procedure-properties' is an association list of properties linked
 * to the procedure.  For example, the property "procedure-print-name"
 * is used when writing or displaying the procedure.
 *
 * The `environment' of a closure is a list of environment frames and
 * represents the closed over environment. (see "Eval Internals" in 
 * "eval.c").
 *
 * Every procedure has an associated property list.  Property lists
 * are attached to closures but not subrs or cclos.  See scm_stand_in_proc.
 */



/************************************************************************
 *(h1 "Rationale -- Procedures")
 * 
 * `procedure?' comes from standard Scheme.
 * 
 * `procedure-properties' and related procedures have no particular
 * rationale -- they might be removed in a future release.
 * 
 * `closure?', `closure-code', and `closure-environment' are principally
 * debugging aids.  They may or may not have other uses in the future.
 * 
 * The `noop' family of procedures are surprisingly handy when programming
 * in a functional style.
 * 
 */
