/* variable.c - first-class scheme variables
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
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/variable.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/eq.h"

/****************************************************************
 *(h0 "First-Class Variables")
 *
 * All bindings are stored in locations called "variables".  Variables
 * are first-class objects.
 *
 * A variable has two parts: a "name hint" and a "value".
 *
 * The name hint is usually (but not necessarily) the symbol whose
 * name denotes this variable in the lexical scope of the variables
 * definition.  It is possible to create a variable whose name hint is
 * different from the name that denotes the variable.
 *
 * The value may be uninitialized in which case the variable is
 * "undefined".  It is an error to try to access the value of an
 * undefined variable.
 *
 * Two variables may represent the same binding, yet not be equal in 
 * the sense of `eq?'.  Variables should ordinarily be compared using
 * `eqv?'
 */
/*(menu)
 */



SCM_SYMBOL (s_anonymous_variable, "anonymous-variable");


/******************************************************
 * scm_small_object_functions for variables
 * 
 */

static int
prin_var (SCM exp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<variable ");
  scm_intprint(exp, 16, port);
  {
    SCM val_cell;
    val_cell = SCM_CDR(exp);
    if (SCM_CAR (val_cell) != SCM_UNDEFINED)
      {
	scm_port_puts (&errn, port, " name: ");
	scm_iprin1 (SCM_CAR (val_cell), port, writing);
      }
    scm_port_puts (&errn, port, " binding: ");
    scm_iprin1 (SCM_VARIABLE_VALUE (exp), port, writing);
  }
  scm_port_putc (&errn, port, '>');
  return 1;
}

int scm_tc16_variable;
static scm_small_object_functions variable_smob = {scm_markcdr, scm_free0, prin_var, scm_eqv_p};


/************************************************************************
 *(h1 "First Class Variable Procedures")
 * 
 * 
 * 
 */

/* make_vcell_variable
 *
 * Create a variable for a given vcell.
 */ 
static SCM
make_vcell_variable (SCM vcell)
{
  SCM_INTS_INDIFFERENT;
  SCM answer;

  SCM_NEWCELL(answer);
  SCM_CDR(answer) = vcell;
  SCM_CAR(answer) = scm_tc16_variable;
  return answer;
}


/*(c make-variable)
 * (make-variable initial-value :optional name-hint)
 * 
 * Create a new, initialized variable.
 */
SCM_PROC(s_make_variable, "make-variable", 1, 1, 0, scm_make_variable);
SCM
scm_make_variable (SCM init, SCM name_hint)
{
  SCM_INTS_ENABLED;
  SCM val_cell;
  SCM answer;

  if (SCM_UNBNDP (name_hint))
    name_hint = s_make_variable;
  SCM_NEWCELL(val_cell);
  SCM_DEFER_INTS;
  SCM_CAR(val_cell) = name_hint;
  SCM_CDR(val_cell) = init;
  answer = make_vcell_variable (val_cell);
  SCM_ALLOW_INTS;
  return answer;
}


/* SCM scm_construct_variable (enum scm_variable_type type, SCM name_hint, SCM cell);
 * 
 * Construct a variable from the pair, `cell', which holds its value.
 * 
 * `type' determines how `cell' is interpreted:
 *
 *	scm_indirect_a_variable
 *	   The variable value is in SCM_CAR (cell).
 *
 *	scm_indirect_d_variable
 *	   The variable value is in SCM_CDR (cell).
 *
 * 	scm_global_variable	
 *	   The variable name is in SCM_CAR (cell)
 *	   and the variable value is in SCM_CDR (cell).
 *
 */
SCM
scm_construct_variable (enum scm_variable_type type, SCM name_hint, SCM cell)
{
  SCM_INTS_ENABLED;
  SCM val_cell;
  SCM answer;

  if (SCM_UNBNDP (name_hint))
    name_hint = s_make_variable;

  if (type == scm_global_variable)
    val_cell = cell;
  else
    {
      SCM_NEWCELL(val_cell);
      SCM_DEFER_INTS;
      SCM_CAR (val_cell) = name_hint;
      SCM_CDR (val_cell) = cell;
      SCM_ALLOW_INTS;
    }
  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  SCM_SET_VARIABLE_TYPE (answer, type, scm_tc16_variable);
  SCM_CDR (answer) = val_cell;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c make-undefined-variable)
 * (make-undefined-variable :optional name-hint)
 * 
 * Create a new uninitialized variable.
 */
SCM_PROC(s_make_undefined_variable, "make-undefined-variable", 0, 1, 0, scm_make_undefined_variable);
SCM
scm_make_undefined_variable (SCM name_hint)
{
  SCM_INTS_ENABLED;

  SCM vcell;
  SCM answer;

  if ((name_hint == SCM_UNDEFINED) || (name_hint == SCM_BOOL_F))
    name_hint = s_anonymous_variable;

  SCM_NEWCELL (vcell);
  SCM_DEFER_INTS;
  SCM_CAR (vcell) = name_hint;
  SCM_CDR (vcell) = SCM_UNDEFINED;
  answer = make_vcell_variable (vcell);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c variable?)
 * (variable? obj)
 * 
 * Return #t if `obj' is a variable, #f otherwise.
 */
SCM_PROC(s_variable_p, "variable?", 1, 0, 0, scm_variable_p);
SCM
scm_variable_p (SCM obj)
{
  SCM_INTS_INDIFFERENT;

  return ( (!SCM_IS_IMMEDIATE(obj) && SCM_VARIABLEP (obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}



/*(c variable-ref)
 * (variable-ref var)
 *  
 * Return the value of the variable `var'.
 */
SCM_PROC(s_variable_ref, "variable-ref", 1, 0, 0, scm_variable_ref);
SCM
scm_variable_ref (SCM var)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(var) && SCM_VARIABLEP(var), var, scm_arg1, s_variable_ref);
  return SCM_VARIABLE_VALUE (var);
}


/*(c variable-name)
 * (variable-name var)
 * 
 * Return the name of the variable `var'.
 */
SCM_PROC (s_variable_name, "variable-name", 1, 0, 0, scm_variable_name);
SCM
scm_variable_name (SCM var)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(var) && SCM_VARIABLEP(var), var, scm_arg1, s_variable_name);
  return SCM_VARIABLE_NAME (var);
}


/*(c variable-set!)
 * (variable-set! var val)
 * 
 * Modify the value of a variable.
 */
SCM_PROC(s_variable_set_x, "variable-set!", 2, 0, 0, scm_variable_set_x);
SCM
scm_variable_set_x (SCM var, SCM val)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(var) && SCM_VARIABLEP (var), var, scm_arg1, s_variable_set_x);
  SCM_VARIABLE_VALUE (var) = val;
  return SCM_UNSPECIFIED;
}


/*(c builtin-variable)
 * (builtin-variable name)
 * 
 * Return the variable denoted by `name' in the built-in 
 * top-level.
 */
SCM_PROC(s_builtin_variable, "builtin-variable", 1, 0, 0, scm_builtin_variable);
SCM
scm_builtin_variable (SCM name)
{
  SCM_INTS_ENABLED;
  SCM vcell;
  SCM var_slot;

  SCM_ASSERT (scm_is_symbol (name), name, scm_arg1, s_builtin_variable);
  vcell = scm_symbol_to_vcell (name, SCM_BOOL_F, SCM_BOOL_T);
  if (vcell == SCM_BOOL_F)
    return SCM_BOOL_F;

  scm_intern_symbol (scm_symhash_vars, name);
  var_slot = scm_symbol_to_hash_table_vcell (scm_symhash_vars, name);

  SCM_DEFER_INTS;
  if (   SCM_IS_IMMEDIATE (SCM_CDR (var_slot))
      || !SCM_VARIABLEP (SCM_CDR (var_slot))
      || (SCM_VARIABLE_VCELL (SCM_CDR (var_slot)) != vcell))
    SCM_CDR (var_slot) = make_vcell_variable (vcell);
  SCM_ALLOW_INTS;

  return SCM_CDR (var_slot);
}


/*(c variable-bound?)
 * (variable-bound? var)
 * 
 * Return #t if the variable `var' has a value, #f if
 * it is undefined.
 */
SCM_PROC(s_variable_bound_p, "variable-bound?", 1, 0, 0, scm_variable_bound_p);
SCM 
scm_variable_bound_p (SCM var)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (!SCM_IS_IMMEDIATE(var) && SCM_VARIABLEP (var), var, scm_arg1, s_variable_bound_p);
  return (SCM_UNBNDP (SCM_VARIABLE_VALUE (var))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}




void
scm_init_variable (void)
{
  SCM_INTS_DISABLED;

  scm_tc16_variable = scm_newsmob (&variable_smob);
#include "systas/libsystas/variable.x"
}



/************************************************************************
 *h1 "First-Class Variable Internals")
 *
 *
 * All variables are represented by non-immediate object:
 *
 *  .variable_type..scm_tc16_variable  .......SCM * value_cell........
 * 
 * variable_type is a value of type `enum scm_variable_type'
 *
 * `value_cell' points to a cons pair.  The car of a value cell points
 * to a symbol: the "name hint" of the variable.  The cdr of the value
 * cell contains one of the following, depending on the variable type:
 *
 *   type:                           contents of vcell cdr:
 *   =======================         ======================================
 *   scm_global_variable             SCM_CDR (vcell) is the variable value
 *   scm_indirect_a_variable         SCM_CADR (vcell) is the variable value
 *   scm_indirect_d_variable         SCM_CDDR (vcell) is the variable value
 *
 * "global" variables contain top-level (free variable) bindings.
 * "indirect" variables contain local (bound variable) bindings.
 *
 * First-class variables are created on demand -- there is usually not
 * a first class variable for every variable of a running program.
 */



/****************************************************************
 *(h1 "First-Class Variables Rationale")
 *
 * We exposed top-level variables to Scheme to make it possible
 * to implement the module system in Scheme in a conceptually
 * straightforward way (a hook in eval permits Scheme programs
 * to define the mapping between variable names (represented
 * by symbols) and variables (represented by first-class variables).
 *
 * It is a historical accident that our first-class variables
 * have a "name hint" -- that is simply a consequence of how
 * variables were represented in SCM, the predecessor implementation.
 * Programs are free to ignore this feature, although it does make
 * the printed representation of a variable easier to understand.
 */
