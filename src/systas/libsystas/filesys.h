/* filesys.h - unix file system decls
 *
 ****************************************************************
 * Copyright (C) 1995,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__FILESYS_H
#define INCLUDE__LIBSYSTAS__FILESYS_H

#include "systas/libsystas/scm.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/weaks.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"



extern long scm_tc16_fd;
extern long scm_tc16_dir;

/* A first-class file descriptor includes
 * a set of flags which are the bitwise-or
 * of any of these values:
 */
enum scm_fd_flags
{
  scm_fd_is_open = 1,
  scm_close_fd_on_gc = 2
};

/* Is the non-immediate object X a file descriptor? 
 */
#define SCM_FDP(x) 			(SCM_TYP16(x)==(scm_tc16_fd))

/* What is the descriptor number of the file descriptor object X?
 */
#define SCM_FD(x)			((int)SCM_CDR (x))

/* What are the flags associated with X?
 */
#define SCM_FD_FLAGS(x)			(SCM_CAR(x) >> 16)


/* Is the non-immediate object X a directory handle?
 */
#define SCM_DIRP(x) (SCM_TYP16(x)==(scm_tc16_dir))


/* Is the directory handle X open (ready to be read)?
 */
#define SCM_OPDIRP(x) (SCM_CAR(x)==(scm_tc16_dir | SCM_OPN))

/* Flag bit 16 in the car of a scm_tc16_dir.
 */
#define SCM_OPN		(1L<<16) /* Is the directory open? */



/* automatically generated __STDC__ prototypes */
extern SCM scm_sys_chown (SCM path, SCM owner, SCM group);
extern SCM scm_file_mode_to_integer (SCM mode);
extern SCM scm_mode2scm (int mode);
extern SCM scm_integer_to_file_mode (SCM mode);
extern SCM scm_sys_chmod (SCM path, SCM smode);
extern SCM scm_numeric_permissions (SCM perms);
extern SCM scm_sys_fchmod (SCM fd, SCM smode);
extern SCM scm_umask (SCM smode);
extern SCM scm_makefd (int fd, int flags);
extern SCM scm_file_descriptor_p (SCM x);
extern SCM scm_integer_to_file_descriptor (SCM n);
extern SCM scm_integer_to_existing_file_descriptor (SCM n);
extern int scm_fileno (SCM obj);
extern SCM scm_file_descriptor_to_integer (SCM x);
extern SCM scm_all_file_descriptors (void);
extern SCM scm_autoclose_file_descriptor_p (SCM x);
extern SCM scm_set_autoclose_file_descriptor_x (SCM x, SCM f);
extern SCM scm_fd_is_open_p (SCM obj);
extern SCM scm_sys_pipe (void);
extern SCM scm_open_flags_to_integer (SCM flags);
extern SCM scm_sys_open (SCM path, SCM sflags, SCM smode);
extern SCM scm_sys_create (SCM path, SCM smode);
extern SCM scm_sys_close (SCM sfd);
extern SCM scm_sys_write (SCM sfd, SCM buf);
extern SCM scm_sys_write_retry (SCM sfd, SCM buf);
extern SCM scm_sys_read (SCM sfd, SCM buf);
extern SCM scm_sys_fsync(SCM sfd);
extern SCM scm_sys_ftruncate (SCM sfd, SCM where);
extern SCM scm_lseek_flag_to_integer (SCM flag);
extern SCM scm_sys_lseek (SCM sfd, SCM offset, SCM whence);
extern SCM scm_sys_dup (SCM oldfd);
extern SCM scm_sys_dup2 (SCM oldfd, SCM newfd);
extern SCM scm_sys_move_fd (SCM oldfd, SCM newfd);
extern SCM scm_sys_fstat (SCM fd);
extern SCM scm_sys_fcntl (SCM sfd, SCM scmd, SCM sarg);
extern SCM scm_sys_isatty_p (SCM fd);
extern SCM scm_sys_ttyname (SCM port);
extern SCM scm_sys_ctermid (void);
extern SCM scm_sys_tcgetpgrp (SCM port);
extern SCM scm_sys_tcsetpgrp (SCM port, SCM pgid);
extern SCM scm_stat2scm (struct stat *stat_temp);
extern SCM scm_sys_stat (SCM path);
extern SCM scm_sys_utime (SCM pathname, SCM actime, SCM modtime);
extern SCM scm_sys_link (SCM oldpath, SCM newpath);
extern SCM scm_sys_unlink (SCM path);
extern SCM scm_sys_rename (SCM oldname, SCM newname);
extern SCM scm_sys_mkdir (SCM path, SCM smode);
extern SCM scm_sys_rmdir (SCM path);
extern SCM scm_sys_opendir (SCM dirname);
extern SCM scm_sys_readdirname (SCM port);
extern SCM scm_sys_rewinddir (SCM port);
extern SCM scm_sys_closedir (SCM port);
extern SCM scm_sys_chdir (SCM str);
extern SCM scm_sys_fchdir (SCM sfd);
extern SCM scm_sys_getcwd (void);
extern SCM scm_sys_select (SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs);
extern SCM scm_sys_symlink(SCM oldpath, SCM newpath);
extern SCM scm_sys_readlink(SCM path);
extern SCM scm_sys_lstat(SCM str);
extern void scm_init_filesys (void);
#endif
