/* async.h - decls handling asynchronous events
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__ASYNC_H
#define INCLUDE__LIBSYSTAS__ASYNC_H

#include "systas/libsystas/scm.h"



enum scm_signals
{
  scm_hup_signal,
  scm_chld_signal,
  scm_int_signal,
  scm_fpe_signal,
  scm_bus_signal,
  scm_segv_signal,
  scm_io_signal,
  scm_alrm_signal,
  scm_gc_signal,
  scm_tick_signal,
  scm_exception_signal,
  scm_num_sigs
};



/* automatically generated __STDC__ prototypes */
extern void scm_async_click (void);
extern SCM scm_async (SCM thunk);
extern SCM scm_system_async (SCM thunk);
extern SCM scm_async_mark (SCM a);
extern SCM scm_system_async_mark (SCM a);
extern SCM scm_set_tick_timer (SCM n);
extern SCM scm_run_asyncs (SCM list_of_a);
extern SCM scm_mask_interrupts (void);
extern SCM scm_unmask_interrupts (void);
extern SCM scm_interrupts_masked_p (void);
extern SCM scm_take_signal (int n);
extern void scm_defer_exception (SCM tag, SCM args);
extern void scm_init_signals (void);
extern void scm_restore_signals (void);
extern SCM scm_ignore_signals (void);
extern SCM scm_unignore_signals (void);
extern int scm_is_async (SCM obj);
extern void scm_init_async (void);
#endif  /* INCLUDE__LIBSYSTAS__ASYNC_H */
