/* unexec.h:
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__LIBSYSTAS__UNEXEC_H
#define INCLUDE__LIBSYSTAS__UNEXEC_H



extern char * scm_unexec_request_filename;



/* automatically generated __STDC__ prototypes */
extern int main_after_unexec (int argc, char * argv[]);
extern SCM scm_unsafe_unexec (SCM old_filename, SCM new_filename);
extern void scm_init_unexec (void);
#endif  /* INCLUDE__LIBSYSTAS__UNEXEC_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (unexec.h)
 */
