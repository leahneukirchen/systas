/* root.c - the roots of garbage collection and the stack
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
#include "systas/libsystas/root.h"


/************************************************************************
 * scm_root_state and scm_sys_protects internals
 *
 * scm_sys_protects is the roots of garbage collection (other than the
 * C stack) and is initialized in "gc.c" and in various "_init" functions.
 *
 * scm_root_state holds the per-stack state of Scheme evaluation.
 * It is initialized primarily in "init.c".  See "root.h".
 */



SCM scm_sys_protects[scm_n_gc_roots];
struct scm_root_state the_scm_root;
struct scm_root_state * scm_root = &the_scm_root;

