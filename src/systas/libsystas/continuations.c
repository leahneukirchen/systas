/* continuations.c - first-class continuations
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
#include <setjmp.h>
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/dynwind.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/symbols.h"


/************************************************************************
 *(h0 "Continuations")
 * 
 * Systas Scheme provides the standard function
 * `call-with-current-continuation'.
 * 
 * The current implementation of that function is slow.  Avoid using it.
 * 
 */


/************************************************************************
 *h1 "Continuation Internals")
 *
 * First class continuations are stored as non-immediate object:
 *
 *  ...long length......scm_tc7_contin  ......t_uchar * data......
 *
 * The continuation contains a heap-allocated copy of the C stack
 * along with a structure of type scm_regs:
 *
 *  data = ...scm_regs regs...|...t_uchar stack_data[]...
 *
 * Continuations are invoked by copying the stack data back onto the
 * C stack and using longjmp to restore control to the point at
 * which the continuation was created.  There is only one such 
 * place in the interpreter (search for "setjmp" in scm_ceval).
 *
 * There is some special treatment for machines with register windows:
 * the windows must be flushed:
 *
 *	1. Before creating a continuation, so that all of the data
 *	   in registers is correctly copied into the heap-allocated
 *	   continuation.
 *
 *	2. Before restoring a continuation stack, so that none of the
 *	   data in registers is incorrectly flushed onto the restored
 *	   stack.
 *
 * The procedure `call-with-current-continuation' is built-in to the
 * interpreter.  See "eval.c".
 */

/****************************************************************
 *h1 "Continuations Rationale")
 *
 * The implementation of continuations uses stack-copying which is
 * a slow operation.  Some other implementation heap-allocate all
 * procedure activation records and in those implementations, 
 * creating a continuation is comparatively inexpensive (no stack-copying
 * required).
 *
 * Systas uses stack copying because it is the only technique compatible
 * with using the C stack in the usual way and hence, the only way to 
 * achieve an implementation in which C and Scheme mix fluidly.
 *
 * Benefits of mixing C and Scheme fluidly include a very simple calling
 * convention for built-in Scheme procedures and the ability to mix the
 * interpreter into almost any C program.
 *
 * A drawback of stack-copying continuations is that they aren't suitable
 * for multi-threading or exception handling.  For exception handling,
 * we have provided built-in "catch" and "throw", "handles" and "handle"
 * which do not use continuations (see "throw.c").
 *
 * For multi-threading of Scheme programs, we suggest either programming
 * in explicit continuation-passing style, or writing macros that 
 * automatically convert Scheme programs to continuation-passing style.
 */


SCM_SYMBOL (s_cont, "continuation");
static void grow_throw (SCM *);
static void dynthrow (SCM *);



/* scm_make_cont
 * 
 * Create a new continuation object.
 */
SCM 
scm_make_cont (SCM * answer)
{
  SCM_INTS_ENABLED;
  long j;
  SCM cont;
  SCM_STACKITEM *src;
  SCM_STACKITEM *dst;

  SCM_NEWCELL (cont);
  *answer = cont;

  SCM_DEFER_INTS;
  SCM_FLUSH_REGISTER_WINDOWS;
  j = scm_stack_size (SCM_REGS (scm_rootcont)->base);
  SCM_REGS (cont) = ((scm_regs *)
		     scm_must_malloc ((long) (sizeof (scm_regs) + j * sizeof (SCM_STACKITEM))));
  SCM_SET_LENGTH (cont, j, scm_tc7_contin);
  SCM_REGS (cont)->dynwind = scm_dynwinds;
  SCM_REGS (cont)->debug_info = scm_debug_info;
  SCM_REGS (cont)->throw_value = SCM_EOL;
  src = SCM_REGS (cont)->base = SCM_REGS (scm_rootcont)->base;
  SCM_REGS (cont)->seq = SCM_REGS (scm_rootcont)->seq;
  SCM_ALLOW_INTS;

#ifndef SCM_STACK_GROWS_UP
  src -= SCM_LENGTH (cont);
#endif /* ndef SCM_STACK_GROWS_UP */
  dst = (SCM_STACKITEM *) (SCM_STACK (cont) + sizeof (scm_regs));
  for (j = SCM_LENGTH (cont); 0 <= --j;)
    *dst++ = *src++;

  return cont;
}


/* scm_call_continuation
 * 
 * Return `val' from continuation `cont'.  This procedure
 * does not return to the caller.
 *
 * Before returning from the continuation, dynamic-wind exit 
 * procedures on the wind chain are invoked.
 *
 * This procedure checks to be sure that the continuation
 * was created on the current stack and signals an error
 * if it was not.
 */
void
scm_call_continuation (SCM cont, SCM val)
{
  SCM_INTS_ENABLED;
  SCM a[2];

  a[0] = cont;
  a[1] = val;

  if (   (SCM_REGS (cont)->seq != SCM_REGS (scm_rootcont)->seq)
      || (SCM_REGS (cont)->base != SCM_REGS (scm_rootcont)->base))
    scm_wta (cont, scm_bogus_continuation, s_cont);
  
  scm_dowinds (SCM_REGS (cont)->dynwind,
	       scm_ilength (scm_dynwinds) - scm_ilength (SCM_REGS (cont)->dynwind));
  scm_debug_info = SCM_REGS (cont)->debug_info; 
  dynthrow (a);
}


/* scm_dynthrow
 * 
 * Return from a continuation.
 *
 * a[0] holds the continuation to return from.
 * a[1] holds the value to return.
 *
 * 
 */
static void 
dynthrow (SCM *a)
{
  SCM_INTS_ENABLED;
  SCM cont;
  SCM val;
  long j;
  SCM_STACKITEM *src;
  SCM_STACKITEM *dst;

  cont = a[0];
  val = a[1];
  dst = SCM_REGS (scm_rootcont)->base;

  /* If the stack isn't large enough, make
   * it larger.
   */
#ifdef SCM_STACK_GROWS_UP
  if (dst + SCM_LENGTH (cont) >= (SCM_STACKITEM *) & a)
    grow_throw (a);		/* doesn't return */
#else
  dst -= SCM_LENGTH (cont);
  if (dst <= (SCM_STACKITEM *) & a)
    grow_throw (a);		/* doesn't return */
#endif


  /* If this machine has register windows, flush
   * them now -- they belong to the stack we are about
   * to overwrite.
   *
   * Otherwise, a signal between here and the longjmp, or the
   * longjmp itself, could incorrectly flush some of those 
   * windows onto the new stack.
   */
  SCM_FLUSH_REGISTER_WINDOWS;

  /* Copy the new stack from the continuation object onto the
   * C stack.
   */
  src = (SCM_STACKITEM *) (SCM_STACK (cont) + sizeof (scm_regs));
  for (j = SCM_LENGTH (cont); 0 <= --j;)
    *dst++ = *src++;

  SCM_REGS(cont)->throw_value = val;
  longjmp (SCM_REGS (cont)->jmpbuf, 1);
}


/* grow_throw
 * 
 * Make the stack larger and call `dynthrow'.
 */
static void 
grow_throw (SCM *a)
{
  SCM_INTS_ENABLED;
  SCM growth[100];

  growth[0] = a[0];
  growth[1] = a[1];
  scm_return_first (SCM_BOOL_F, growth); /* foil -O */
  dynthrow (growth);
}




void
scm_init_continuations (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/continuations.x"
}

