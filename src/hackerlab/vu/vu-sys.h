/* vu-sys.h - decls for direct VU functions
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__VU_SYS_H
#define INCLUDE__VU__VU_SYS_H


#include "hackerlab/vu/vu.h"


/****************************************************************
 * vu_system_fs_vtable
 *
 * All functions perform ordinary system calls.
 */
extern struct vu_fs_discipline _vu_system_fs_vtable;


/* automatically generated __STDC__ prototypes */
extern void * vu_sys_make_closure (void * closure);
extern void vu_sys_free_closure (void * closure);
extern int vu_sys_access (int * errn, char * path, int mode, void * closure);
extern int vu_sys_chdir (int * errn, char * path, void * closure);
extern int vu_sys_chmod (int * errn, char * path, int mode, void * closure);
extern int vu_sys_chown (int * errn,
			 char * path,
			 int owner,
			 int group,
			 void * closure);
extern int vu_sys_chroot (int * errn, char * path, void * closure);
extern int vu_sys_closedir (int * errn, DIR * dir, void * closure);
extern int vu_sys_close (int * errn, int fd, void * closure);
extern int vu_sys_fchdir (int * errn, int fd, void * closure);
extern int vu_sys_fchmod (int * errn, int fd, int mode, void * closure);
extern int vu_sys_fchown (int * errn,
			  int fd,
			  int owner,
			  int group,
			  void * closure);
extern int vu_sys_fstat (int * errn,
			 int fd,
			 struct stat * buf,
			 void * closure);
extern int vu_sys_fsync (int * errn, int fd, void * closure);
extern int vu_sys_ftruncate (int * errn, int fd, off_t where, void * closure);
extern int vu_sys_link (int * errn, char * from, char * to, void * closure);
extern off_t vu_sys_lseek (int * errn,
			   int fd,
			   off_t offset,
			   int whence,
			   void * closure);
extern int vu_sys_lstat (int * errn,
			 char * path,
			 struct stat * buf,
			 void * closure);
extern int vu_sys_mkdir (int * errn, char * path, int mode, void * closure);
extern int vu_sys_open (int * errn,
			char * path,
			int flags,
			int mode,
			void * closure);
extern int vu_sys_opendir (int * errn,
			   DIR ** retv,
			   char * path,
			   void * closure);
extern ssize_t vu_sys_read (int * errn,
			    int fd,
			    char * buf,
			    size_t count,
			    void * closure);
extern int vu_sys_readdir (int * errn,
			   struct alloc_limits * limits,
			   char ** file_ret,
			   DIR * dir,
			   void * closure);
extern int vu_sys_readlink (int * errn,
			    char * path,
			    char * buf,
			    int bufsize,
			    void * closure);
extern int vu_sys_rename (int * errn, char * from, char * to, void * closure);
extern int vu_sys_rmdir (int * errn, char * path, void * closure);
extern int vu_sys_stat (int * errn,
			char * path,
			struct stat * buf,
			void * closure);
extern int vu_sys_symlink (int * errn, char * from, char * to, void * closure);
extern int vu_sys_telldir (int * errn, DIR * dir, void * closure);
extern int vu_sys_truncate (int * errn,
			    char * path,
			    off_t where,
			    void * closure);
extern int vu_sys_unlink (int * errn, char * path, void * closure);
extern int vu_sys_utime (int * errn,
			 char * path,
			 struct utimbuf * times,
			 void * closure);
extern ssize_t vu_sys_write (int * errn,
			     int fd,
			     char * buf,
			     size_t count,
			     void * closure);
extern int vu_sys_fcntl (int * errn,
			 int fd,
			 int cmd,
			 long arg,
			 void * closure);
extern int vu_sys_dup (int * errn, int fd, void * closure);
extern int vu_sys_dup2 (int * errn, int fd, int newfd, void * closure);
extern int vu_sys_move_state (int * errn, int fd, int newfd, void * closure);
#endif  /* INCLUDE__VU__VU_SYS_H */
