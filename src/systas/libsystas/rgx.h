/* rgx.h - decls for the Scheme regexp interface
 *
 ****************************************************************
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__RGX_H
#define INCLUDE__LIBSYSTAS__RGX_H



/* automatically generated __STDC__ prototypes */
extern SCM scm_compiled_regexp_p (SCM obj);
extern SCM scm_regcomp (SCM pattern, SCM cflags);
extern SCM scm_low_level_regexec (SCM rgx, SCM str, SCM match_pick, SCM efl);
extern SCM scm_set_rx_superset_cache_size (SCM sn);
extern SCM scm_regexp_to_dfa (SCM regexp, SCM cfl);
extern SCM scm_dfa_fork (SCM dfa);
extern SCM scm_reset_dfa_x (SCM dfa);
extern SCM scm_dfa_final_tag (SCM dfa);
extern SCM scm_dfa_continuable_p (SCM dfa);
extern SCM scm_sys_advance_dfa_to_final_state_x (SCM dfa, SCM s);
extern int scm_is_compiled_regexp (SCM obj);
extern void scm_init_rgx (void);
#endif  /* INCLUDE__LIBSYSTAS__RGX_H */
