/* vu-dash.c - the dash (stdin/stdout) file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/vu/vu-dash.h"


/* __STDC__ prototypes for static functions */
static int vu_dash_open (int * errn, char * path, int flags, int mode, void * closure);


static struct vu_fs_discipline vu_dash_vtable;


/************************************************************************
 *(h0 "A VU Name Handler for Standard Input and Output")
 * 
 * These functions provide a VU namespace handler for file names matching
 * the regexp:
 * 
 * 		^-$
 * 
 * When opened for `O_RDONLY' access, `"-"' refers to descriptor 0.
 * 
 * When opened for `O_WRONLY' access, `"-"' refers to descriptor 1.
 * 
 * The file `"-"' may not be opened for `O_RDWR' access.
 */



/*(c vu_push_dash_handler)
 * void vu_push_dash_handler (int is_optional);
 * 
 * Establish a VU namespace handler for file-names matching the
 * regexp:
 * 
 * 		^-$
 * 
 * 
 * When opened for `O_RDONLY' access, `"-"' refers to descriptor 0.
 * 
 * When opened for `O_WRONLY' access, `"-"' refers to descriptor 1.
 * 
 * The file `"-"' may not be opened for `O_RDWR' access.
 * 
 * If the flag `is_optional' is 0, the namespace handler is simply
 * installed.  If it is not 0, the handler is registered under the
 * name `"fd"', but not installed.  
 * 
 * (See xref:"vu_enable_optional_name_handler".)
 */
void
vu_push_dash_handler (int is_optional)
{
  static int initialized = 0;
  static regex_t rgx;
  static t_uchar * doc[] =
    {
      "-",
      "Standard input or output.",
      0
    };

  if (!initialized)
    {
      if (0 > regcomp (&rgx, "^-$", 0))
	panic ("unable to compile regexp");
      initialized = 1;
    }
  
  vu_push_name_handler ("standard files", doc, &rgx, 0, &vu_dash_vtable, 0, is_optional);
}



static int
vu_dash_open (int * errn, char * path, int flags, int mode, void * closure)
{
  switch (flags & O_ACCMODE)
    {
    case O_RDONLY:
      return 0;

    case O_WRONLY:
      return 1;

    default:
      *errn = EINVAL;
      return -1;
    }
}


#define vu_dash_make_closure	vu_sys_make_closure
#define vu_dash_free_closure	vu_sys_free_closure
#define vu_dash_access		vu_sys_access
#define vu_dash_chdir		vu_sys_chdir
#define vu_dash_chmod		vu_sys_chmod
#define vu_dash_chown		vu_sys_chown
#define vu_dash_chroot		vu_sys_chroot
#define vu_dash_close		vu_sys_close
#define vu_dash_closedir	vu_sys_closedir
#define vu_dash_fchdir		vu_sys_fchdir
#define vu_dash_fchmod		vu_sys_fchmod
#define vu_dash_fchown		vu_sys_fchown
#define vu_dash_fstat		vu_sys_fstat
#define vu_dash_fsync		vu_sys_fsync
#define vu_dash_ftruncate	vu_sys_ftruncate
#define vu_dash_link		vu_sys_link
#define vu_dash_lseek		vu_sys_lseek
#define vu_dash_lstat		vu_sys_lstat
#define vu_dash_mkdir		vu_sys_mkdir
#define vu_dash_opendir		vu_sys_opendir
#define vu_dash_read		vu_sys_read
#define vu_dash_readdir		vu_sys_readdir
#define vu_dash_readlink	vu_sys_readlink
#define vu_dash_rename		vu_sys_rename
#define vu_dash_rmdir		vu_sys_rmdir
#define vu_dash_stat		vu_sys_stat
#define vu_dash_symlink		vu_sys_symlink
#define vu_dash_telldir		vu_sys_telldir
#define vu_dash_truncate	vu_sys_truncate
#define vu_dash_unlink		vu_sys_unlink
#define vu_dash_utime		vu_sys_utime
#define vu_dash_write		vu_sys_write
#define vu_dash_fcntl		vu_sys_fcntl
#define vu_dash_dup		vu_sys_dup
#define vu_dash_dup2		vu_sys_dup2
#define vu_dash_move_state	vu_sys_move_state

static struct vu_fs_discipline vu_dash_vtable = { VU_FS_DISCIPLINE_INITIALIZERS (vu_dash_) };

