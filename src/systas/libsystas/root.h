/* root.h - decls for the roots of garbage collection and the stack
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */

#ifndef INCLUDE__LIBSYSTAS__ROOT_H
#define INCLUDE__LIBSYSTAS__ROOT_H

#include "systas/libsystas/scm.h"



/* There is one array per process of GC roots.
 */
extern SCM scm_sys_protects[];

#define scm_flo0 scm_sys_protects[0]
#define scm_listofnull scm_sys_protects[1]
#define scm_undefineds scm_sys_protects[2]
#define scm_nullstr scm_sys_protects[3]
#define scm_symhash scm_sys_protects[4]
#define scm_weak_symhash scm_sys_protects[5]
#define scm_symhash_vars scm_sys_protects[6]
#define scm_kw_obarray scm_sys_protects[7]
#define scm_type_obj_list scm_sys_protects[8]
#define scm_first_type scm_sys_protects[9]
#define scm_stand_in_procs scm_sys_protects[10]
#define scm_object_whash scm_sys_protects[11]
#define scm_permobjs scm_sys_protects[12]
#define scm_asyncs scm_sys_protects[13]
#define scm_deferred_throw_tag scm_sys_protects[14]
#define scm_deferred_throw_args scm_sys_protects[15]
#define scm_errno_table scm_sys_protects[16]
#define scm_buffer_strings scm_sys_protects[17]
#define scm_coextensive_objects scm_sys_protects[18]
#define scm_print_struct_functions scm_sys_protects[19]
#define scm_cpp_constants scm_sys_protects[20]
#define scm_n_gc_roots 21




/* There is one of these structures per stack.
 */
struct scm_root_state
{
  SCM_STACKITEM * stack_base;		/* The bottom of the stack for GC purposes. */

  SCM rootcont;				/* The root continuation -- calling it exits Scheme. */
  SCM dynwinds;				/* The wind chain. */
  struct scm_debug_frame * debug_info; 	/* The debug info chain. */
  SCM progargs;				/* The argv parameters to the process as a list. */
  SCM exitval;				/* A value to return from Scheme. */
  SCM cur_inp;				/* The current input port. */
  SCM cur_outp;				/* The current output port. */
  SCM cur_errp;				/* The current error port. */
  SCM cur_loadp;			/* The current try-load port. */
  SCM def_inp;				/* The default input port. */
  SCM def_outp;				/* The default output port. */
  SCM def_errp;				/* The default error port. */
  SCM top_level_lookup_thunk_var;	/* See below. */

  int argc;				/* The argv parameters to the process as an array */
  t_uchar ** argv;
};

/* top_level_lookup_thunk_var
 *
 * The vcell of a variable that holds #f or a procedure that
 * takes a symbol and returns a variable.  If its a procedure,
 * that procedure defines the current top-level environment.
 */

extern struct scm_root_state * scm_root;

#define scm_stack_base			(scm_root->stack_base)
#define scm_rootcont			(scm_root->rootcont)
#define scm_dynwinds			(scm_root->dynwinds)
#define scm_debug_info			(scm_root->debug_info)
#define scm_progargs			(scm_root->progargs)
#define scm_exitval 			(scm_root->exitval)
#define scm_cur_inp			(scm_root->cur_inp)
#define scm_cur_outp			(scm_root->cur_outp)
#define scm_cur_errp			(scm_root->cur_errp)
#define scm_cur_loadp			(scm_root->cur_loadp)
#define scm_def_inp			(scm_root->def_inp)
#define scm_def_outp			(scm_root->def_outp)
#define scm_def_errp			(scm_root->def_errp)
#define scm_top_level_lookup_thunk_var	(scm_root->top_level_lookup_thunk_var)

#define scm_argc			(scm_root->argc)
#define scm_argv			(scm_root->argv)



/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__LIBSYSTAS__ROOT_H */
