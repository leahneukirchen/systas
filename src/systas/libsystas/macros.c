/* macros.c - functions for constructing scheme macros
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
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/macros.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/read-print.h"


/************************************************************************
 *(h0 "Low Level Macros")
 *
 * Normally when you define a function, arguments passed to that function
 * are evaluated and the function itself has no access to the lexical
 * environment of the caller.
 * 
 * Three low-level hooks provide a way to define functions that violate
 * these rules, taking arguments unevaluated, and gaining access to the
 * lexical environment of the caller.  This is quite a powerful facility
 * that allows programs to extend the evaluator in many ways.  It's also an
 * extremely non-standard feature.
 *
 * Caution should be taken when using these functions.  They can easily 
 * lead to programs that are hard to read and understand.  There may eventually
 * be a compiler for this implementation and some macros can complicate or prevent
 * effective compilation.
 */
/*(menu)
 */


/****************************************************************
 * Small Object Functions for Macros
 */

static int 
prinmacro (SCM exp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

  if (SCM_CAR (exp) & (3l << 16))
    scm_port_puts (&errn, port, "#<macro");
  else
    scm_port_puts (&errn, port, "#<syntax");
  if (SCM_CAR (exp) & (2l << 16))
    scm_port_putc (&errn, port, '!');
  scm_port_putc (&errn, port, ' ');
  scm_iprin1 (SCM_CDR (exp), port, writing);
  scm_port_putc (&errn, port, '>');
  return !0;
}

long scm_tc16_macro;
static scm_small_object_functions macrosmob = {scm_markcdr, scm_free0, prinmacro};


/****************************************************************
 *(h1 "Macro Procedures")
 */

/*(c procedure->syntax)
 *(procedure->syntax procedure)
 * 
 * Convert `procedure' into "syntax".
 * 
 * `procedure' should take an expression and an environment:
 *
 *	(procedure expression environment)
 * 
 * When the syntax object is applied, the expression containing it
 * and the active environment are passed to `procedure'.  The value returned
 * from `procedure' becomes the value of the expression.
 */
SCM_PROC (s_procedure_to_syntax, "procedure->syntax", 1, 0, 0, scm_procedure_to_syntax);
SCM 
scm_procedure_to_syntax (SCM code)
{
  SCM_INTS_UNKNOWN;
  SCM z;

  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro;
  return z;
}


/*(c procedure->macro)
 * (procedure->macro procedure)
 * 
 * Convert `procedure' into a "macro".
 * 
 * `procedure' should take an expression and an environment:
 *
 *		(procedure expression environment)
 * 
 * When the macro object is applied, the expression containing it and
 * the active environment are passed to `procedure'.  The value returned from
 * `procedure' is then evaluated in the caller's lexical environment
 * (the same environement passed to `procedure') and the value returned
 * from that expression is returned from the macro.
 */
SCM_PROC (s_procedure_to_macro, "procedure->macro", 1, 0, 0, scm_procedure_to_macro);
SCM 
scm_procedure_to_macro (SCM code)
{
  SCM_INTS_UNKNOWN;
  SCM z;

  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro | (1l << 16);
  return z;
}


/*(c procedure->memoizing-macro)
 * (procedure->memoizing-macro procedure)
 * 
 * Convert `procedure' into a "memoizing macro".
 * 
 * `procedure' should take an expression and an environment:
 *
 *		(procedure expression environment)
 * 
 * When the memoizing-macro is applied, the expression containing it and
 * the active environment are passed to `procedure'.
 * 
 * The first time that occurs, the value returned from `procedure' replaces
 * the source-expression that called the macro.
 * 
 * Then and thereafter the new expression is evaluated normally.  The 
 * memoizing-macro procedure is never again called at that point in 
 * the program.  (A memoizing macro is a kind of "self-modifying code").
 */
SCM_PROC (s_procedure_to_memoizing_macro,
	  "procedure->memoizing-macro", 1, 0, 0, scm_procedure_to_memoizing_macro);
SCM 
scm_procedure_to_memoizing_macro (SCM code)
{
  SCM_INTS_UNKNOWN;
  SCM z;

  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro | (2l << 16);
  return z;
}


/****************************************************************
 * Defining Built-in Macros.
 */

/* scm_make_synt
 * 
 * Construct a built-in macro.
 *
 * `name' is the name of the macro and the name of the variable
 * to which it will be bound.
 *
 * `macroizer' should be one of:
 *
 *	scm_procedure_to_syntax
 *	scm_procedure_to_macro
 *	scm_procedure_to_memoizing_macro
 *
 * `fcn' should be a function of two arguments that performs the
 * expansion.
 */
SCM 
scm_make_synt (char *name, SCM (*macroizer) (SCM), SCM (*fcn)())
{
  SCM_INTS_DISABLED;
  SCM z;
  SCM symcell;
  long tmp;

  symcell = scm_intern_symhash (name, SCM_UNDEFINED);
  tmp = ((((struct scm_cell *) (SCM_CAR (symcell))) - scm_heap_org) << 8);
  if ((tmp >> 8) != ((struct scm_cell *) (SCM_CAR (symcell)) - scm_heap_org))
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_CAR (z) = tmp + scm_tc7_subr_2;
  SCM_CDR (symcell) = macroizer (z);
  return SCM_CAR (symcell);
}




void
scm_init_macros (void)
{
  SCM_INTS_DISABLED;

  scm_tc16_macro = scm_newsmob (&macrosmob);
#include "systas/libsystas/macros.x"
}


/************************************************************************
 *h1 "Macros Internals")
 *
 * Macros are represented by a non-immediate object:
 *
 *  ..........type....scm_tc16_macro  ..........SCM procedure.........
 *
 * `type', beginning at bit 16, is one of these values:
 *
 *	0	- syntax
 *	1	- macro
 *	2	- memoizing macro
 *
 * `eval' does the work of interpreting macros, including splicing
 * values returned from memoizing macros into the code that called 
 * them.
 */

/************************************************************************
 *(h1 "Macros Rationale")
 *
 * These three styles of macro originated in SCM by Aubrey Jaffer.
 *
 * Memoizing macros are useful for incremental code generation from high-level
 * code.
 * 
 * Hygenic macros systems can be built on these primitives.
 */
