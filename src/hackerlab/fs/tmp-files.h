/* tmp-files.h:
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__FS__TMP_FILES_H
#define INCLUDE__FS__TMP_FILES_H

#include "hackerlab/vu/vu.h"
#include "hackerlab/mem/alloc-limits.h"


/* automatically generated __STDC__ prototypes */
extern char * tmp_dir (struct alloc_limits * limits);
extern int tmp_open (int * errn,
		     char ** name,
		     struct alloc_limits * limits,
		     char * basename,
		     int flags, int mode);
extern int tmp_open_anonymous (int * errn, int flags, int mode);
#endif  /* INCLUDE__FS__TMP_FILES_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (tmp-files.h)
 */
