/* md5s.h:
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__LIBSYSTAS__MD5S_H
#define INCLUDE__LIBSYSTAS__MD5S_H



/* automatically generated __STDC__ prototypes */
extern SCM scm_md5_state_p (SCM obj);
extern SCM scm_make_md5_engine (void);
extern SCM scm_md5_append_x (SCM engine, SCM str);
extern SCM scm_md5_finish_x (SCM engine);
extern int scm_is_md5_state (SCM obj);
extern void scm_init_md5s (void);
#endif  /* INCLUDE__LIBSYSTAS__MD5S_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (md5s.h)
 */
