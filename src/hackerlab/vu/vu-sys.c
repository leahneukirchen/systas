/* vu-sys.c - direct VU functions
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/os/unistd.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/char/str.h"


/************************************************************************
 *(h0 "VU Native File System Access")
 * 
 * 
 * These functions define a trivial file-system implementation in
 * which all functions call their system-call equivalents.
 */

struct vu_fs_discipline _vu_system_fs_vtable
  = { VU_FS_DISCIPLINE_INITIALIZERS (vu_sys_) };

/*(c vu_sys_make_closure)
 * void * vu_sys_make_closure (void * closure);
 * 
 * Return `closure'.
 */
void *
vu_sys_make_closure (void * closure)
{
  return closure;
}


/*(c vu_sys_free_closure)
 * void vu_sys_free_closure (void * closure);
 * 
 * A noop.
 */
void
vu_sys_free_closure (void * closure)
{}


/*(c vu_sys_access)
 * int vu_sys_access (int * errn, char * path, int mode, void * closure);
 * 
 * Call `access'.
 */
int
vu_sys_access (int * errn, char * path, int mode, void * closure)
{
  int rv;
  rv = access (path, mode);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_chdir)
 * int vu_sys_chdir (int * errn, char * path, void * closure);
 * 
 * Call `chdir'.
 */
int
vu_sys_chdir (int * errn, char * path, void * closure)
{
  int rv;
  rv = chdir (path);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_chmod)
 * int vu_sys_chmod (int * errn, char * path, int mode, void * closure);
 * 
 * Call `chmod'.
 */
int
vu_sys_chmod (int * errn, char * path, int mode, void * closure)
{
  int rv;
  rv = chmod (path, mode);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_chown)
 * int vu_sys_chown (int * errn,
 *                   char * path,
 *                   int owner,
 *                   int group,
 *                   void * closure);
 * 
 * Call `chown'.
 */
int
vu_sys_chown (int * errn,
	      char * path,
	      int owner,
	      int group,
	      void * closure)
{
  int rv;
  rv = chown (path, owner, group);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_chroot)
 * int vu_sys_chroot (int * errn, char * path, void * closure);
 * 
 * Call `chroot'.
 */
int
vu_sys_chroot (int * errn, char * path, void * closure)
{
  int rv;
  rv = chroot (path);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_closedir)
 * int vu_sys_closedir (int * errn, DIR * dir, void * closure);
 * 
 * Call `closedir'.
 */
int
vu_sys_closedir (int * errn, DIR * dir, void * closure)
{
  int rv;
  rv = closedir (dir);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_close)
 * int vu_sys_close (int * errn, int fd, void * closure);
 * 
 * Call `close'.
 */
int
vu_sys_close (int * errn, int fd, void * closure)
{
  int rv;
  rv = close (fd);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_fchdir)
 * int vu_sys_fchdir (int * errn, int fd, void * closure);
 * 
 * Call `fchdir'.
 */
int
vu_sys_fchdir (int * errn, int fd, void * closure)
{
  int rv;
  rv = fchdir (fd);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_fchmod)
 * int vu_sys_fchmod (int * errn, int fd, int mode, void * closure);
 * 
 * Call `fchmod'.
 */
int
vu_sys_fchmod (int * errn, int fd, int mode, void * closure)
{
  int rv;
  rv = fchmod (fd, mode);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_fchown)
 * int vu_sys_fchown (int * errn,
 *                    int fd,
 *                    int owner,
 *                    int group,
 *                    void * closure);
 * 
 * Call `fchown'.
 */
int
vu_sys_fchown (int * errn,
	       int fd,
	       int owner,
	       int group,
	       void * closure)
{
  int rv;
  rv = fchown (fd, owner, group);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_fstat)
 * int vu_sys_fstat (int * errn,
 *                   int fd,
 *                   struct stat * buf,
 *                   void * closure);
 * 
 * Call `fstat'.
 */
int
vu_sys_fstat (int * errn,
	      int fd,
	      struct stat * buf,
	      void * closure)
{
  int rv;
  rv = fstat (fd, buf);
  if (rv)
    *errn = errno;
  return rv;
}

/*(c vu_sys_fsync)
 * int vu_sys_fsync (int * errn, int fd, void * closure);
 * 
 * Call `fsync'.
 */
int
vu_sys_fsync (int * errn, int fd, void * closure)
{
  int rv;
  rv = fsync (fd);
  if (rv)
    *errn = errno;
  return rv;
}

/*(c vu_sys_ftruncate)
 * int vu_sys_ftruncate (int * errn, int fd, off_t where, void * closure);
 * 
 * Call `ftruncate'.
 */
int
vu_sys_ftruncate (int * errn, int fd, off_t where, void * closure)
{
  int rv;
  rv = ftruncate (fd, where);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_link)
 * int vu_sys_link (int * errn, char * from, char * to, void * closure);
 * 
 * Call `link'.
 */
int
vu_sys_link (int * errn, char * from, char * to, void * closure)
{
  int rv;
  rv = link (from, to);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_lseek)
 * off_t vu_sys_lseek (int * errn,
 *                     int fd,
 *                     off_t offset,
 *                     int whence,
 *                     void * closure);
 * 
 * Call `lseek'.
 */
off_t
vu_sys_lseek (int * errn,
	      int fd,
	      off_t offset,
	      int whence,
	      void * closure)
{
  off_t rv;
  rv = lseek (fd, offset, whence);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_lstat)
 * int vu_sys_lstat (int * errn,
 *                   char * path,
 *                   struct stat * buf,
 *                   void * closure);
 * 
 * Call `lstat'.
 */
int
vu_sys_lstat (int * errn,
	      char * path,
	      struct stat * buf,
	      void * closure)
{
  int rv;
  rv = lstat (path, buf);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_mkdir)
 * int vu_sys_mkdir (int * errn, char * path, int mode, void * closure);
 * 
 * Call `mkdir'.
 */
int
vu_sys_mkdir (int * errn, char * path, int mode, void * closure)
{
  int rv;
  rv = mkdir (path, mode);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_open)
 * int vu_sys_open (int * errn,
 *                  char * path,
 *                  int flags,
 *                  int mode,
 *                  void * closure);
 * 
 * Call `open'.
 */
int
vu_sys_open (int * errn,
	     char * path,
	     int flags,
	     int mode,
	     void * closure)
{
  int rv;
  rv = open (path, flags, mode);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_opendir)
 * int vu_sys_opendir (int * errn,
 *                     DIR ** retv,
 *                     char * path,
 *                     void * closure);
 * 
 * Call `opendir'.
 */
int
vu_sys_opendir (int * errn,
		DIR ** retv,
		char * path,
		void * closure)
{
  *retv = opendir (path);
  if (*retv)
    *errn = errno;
  return (*retv == 0 ? -1 : 0);
}

/*(c vu_sys_read)
 * ssize_t vu_sys_read (int * errn,
 *                      int fd,
 *                      char * buf,
 *                      size_t count,
 *                      void * closure);
 * 
 * 
 * Call `read'.
 */
ssize_t
vu_sys_read (int * errn,
	     int fd,
	     char * buf,
	     size_t count,
	     void * closure)
{
  ssize_t rv;
  rv = read (fd, buf, count);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_readdir)
 * int vu_sys_readdir (int * errn,
 *		       struct alloc_limits * limits,
 *                     char ** file_ret,
 *                     DIR * dir,
 *                     void * closure);
 * 
 * Call `readdir'.
 */
int
vu_sys_readdir (int * errn,
		struct alloc_limits * limits,
		char ** file_ret,
		DIR * dir,
		void * closure)
{
  struct dirent * de;
  errno = 0;
  de = readdir (dir);
  if (!de)
    {
      *errn = errno;
      return -1;
    }

  if (file_ret)
    {
      *file_ret = str_save (limits, de->d_name);
      if (!*file_ret)
	{
	  *errn = ENOMEM;
	  return -1;
	}
    }

  return 0;
}

/*(c vu_sys_readlink)
 * int vu_sys_readlink (int * errn,
 *                      char * path,
 *                      char * buf,
 *                      int bufsize,
 *                      void * closure);
 * 
 * Call `readlink'.
 */
int
vu_sys_readlink (int * errn,
		 char * path,
		 char * buf,
		 int bufsize,
		 void * closure)
{
  int rv;
  rv = readlink (path, buf, bufsize);
  if (rv)
    *errn = errno;
  return rv;
}

/*(c vu_sys_rename)
 * int vu_sys_rename (int * errn, char * from, char * to, void * closure);
 * 
 * Call `rename'.
 */
int
vu_sys_rename (int * errn, char * from, char * to, void * closure)
{
  int rv;
  rv = rename (from, to);
  if (rv)
    *errn = errno;
  return rv;
}

/*(c vu_sys_rmdir)
 * int vu_sys_rmdir (int * errn, char * path, void * closure);
 * 
 * Call `rmdir'.
 */
int
vu_sys_rmdir (int * errn, char * path, void * closure)
{
  int rv;
  rv = rmdir (path);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_stat)
 * int vu_sys_stat (int * errn,
 *                  char * path,
 *                  struct stat * buf,
 *                  void * closure);
 * 
 * Call `stat'.
 */
int
vu_sys_stat (int * errn,
	     char * path,
	     struct stat * buf,
	     void * closure)
{
  int rv;
  rv = stat (path, buf);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_symlink)
 * int vu_sys_symlink (int * errn, char * from, char * to, void * closure);
 * 
 * Call `symlink'.
 */
int
vu_sys_symlink (int * errn, char * from, char * to, void * closure)
{
  int rv;
  rv = symlink (from, to);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_telldir)
 * int vu_sys_telldir (int * errn, DIR * dir, void * closure);
 * 
 * Call `telldir'.
 */
int
vu_sys_telldir (int * errn, DIR * dir, void * closure)
{
  int rv;
  rv = telldir (dir);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_truncate)
 * int vu_sys_truncate (int * errn,
 *                      char * path,
 *                      off_t where,
 *                      void * closure);
 * 
 * Call `truncate'.
 */
int
vu_sys_truncate (int * errn,
		 char * path,
		 off_t where,
		 void * closure)
{
  int rv;
  rv = truncate (path, where);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_unlink)
 * int vu_sys_unlink (int * errn, char * path, void * closure);
 * 
 * Call `unlink'.
 */
int
vu_sys_unlink (int * errn, char * path, void * closure)
{
  int rv;
  rv = unlink (path);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_utime)
 * int vu_sys_utime (int * errn,
 *                   char * path,
 *                   struct utimbuf * times,
 *                   void * closure);
 * 
 * Call `utime'.
 */
int
vu_sys_utime (int * errn,
	      char * path,
	      struct utimbuf * times,
	      void * closure)
{
  int rv;
  rv = utime (path, times);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_write)
 * ssize_t vu_sys_write (int * errn,
 *                       int fd,
 *                       char * buf,
 *                       size_t count,
 *                       void * closure);
 * 
 * Call `write'.
 */
ssize_t
vu_sys_write (int * errn,
	      int fd,
	      char * buf,
	      size_t count,
	      void * closure)
{
  ssize_t rv;
  rv = write (fd, buf, count);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_fcntl)
 * int vu_sys_fcntl (int * errn,
 *                   int fd,
 *                   int cmd,
 *                   long arg,
 *                   void * closure);
 * 
 * Call `fcntl'.
 */
int
vu_sys_fcntl (int * errn,
	      int fd,
	      int cmd,
	      long arg,
	      void * closure)
{
  int rv;
  rv = fcntl (fd, cmd, arg);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_dup)
 * int vu_sys_dup (int * errn, int fd, void * closure);
 * 
 * Call `dup'.
 */
int
vu_sys_dup (int * errn, int fd, void * closure)
{
  int rv;
  rv = dup (fd);
  if (rv)
    *errn = errno;
  return rv;
}


/*(c vu_sys_dup2)
 * int vu_sys_dup2 (int * errn, int fd, int newfd, void * closure);
 * 
 * Call `dup2'.
 */
int
vu_sys_dup2 (int * errn, int fd, int newfd, void * closure)
{
  int rv;
  rv = dup2 (fd, newfd);
  if (rv)
    *errn = errno;
  return rv;
}



/*(c vu_sys_move_state)
 * int vu_sys_move_state (int * errn, int fd, int newfd, void * closure);
 * 
 * A noop (return 0).
 */
int
vu_sys_move_state (int * errn, int fd, int newfd, void * closure)
{
  return 0;
}

