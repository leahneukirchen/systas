/* unexec.c: 
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/sys/stat.h"
#include "hackerlab/char/str.h"
#include "systas/libsystas/systas.h"
#include "systas/libsystas/unexec.h"




SCM_SYMBOL (s_quit, "quit");

char * scm_unexec_request_filename = 0;



SCM_PROC (s_request_unexec, "request-unexec", 1, 0, 0, scm_request_unexec);
SCM
scm_request_unexec (SCM new_filename)
{
  SCM_ASSERT (scm_is_ro_string (new_filename), new_filename, scm_arg1, s_request_unexec);

  SCM_DEFER_INTS;
  scm_unexec_request_filename = str_save (0, SCM_CHARS (new_filename));
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}



void
scm_init_unexec (void)
{
#include "systas/libsystas/unexec.x"
}




/* tag: Tom Lord Fri May  3 19:38:12 2002 (unexec.c)
 */
