/* vu-virtual-null.c - the virtual-null file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/reserv.h"
#include "hackerlab/vu/vu-virtual-null.h"


/* __STDC__ prototypes for static functions */
static void * vu_virtual_null_make_closure (void * closure);
static void vu_virtual_null_free_closure (void * closure);
static int vu_virtual_null_access (int * errn, char * path, int mode, void * closure);
static int vu_virtual_null_chdir (int * errn, char * path, void * closure);
static int vu_virtual_null_chmod (int * errn, char * path, int mode, void * closure);
static int vu_virtual_null_chown (int * errn, char * path, int owner, int group, void * closure);
static int vu_virtual_null_chroot (int * errn, char * path, void * closure);
static int vu_virtual_null_closedir (int * errn, DIR * dir, void * closure);
static int vu_virtual_null_close (int * errn, int fd, void * closure);
static int vu_virtual_null_fchdir (int * errn, int fd, void * closure);
static int vu_virtual_null_fchmod (int * errn, int fd, int mode, void * closure);
static int vu_virtual_null_fchown (int * errn, int fd, int owner, int group, void * closure);
static int vu_virtual_null_fstat (int * errn, int fd, struct stat * buf, void * closure);
static int vu_virtual_null_fsync (int * errn, int fd, void * closure);
static int vu_virtual_null_ftruncate (int * errn, int fd, off_t where, void * closure);
static int vu_virtual_null_link (int * errn, char * from, char * to, void * closure);
static off_t vu_virtual_null_lseek (int * errn, int fd, off_t offset, int whence, void * closure);
static int vu_virtual_null_lstat (int * errn, char * path, struct stat * buf, void * closure);
static int vu_virtual_null_mkdir (int * errn, char * path, int mode, void * closure);
static int vu_virtual_null_open (int * errn, char * path, int flags, int mode, void * closure);
static int vu_virtual_null_opendir (int * errn, DIR ** retv,  char * path, void * closure);
static ssize_t vu_virtual_null_read (int * errn, int fd, char * buf, size_t count, void * closure);
static int vu_virtual_null_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure);
static int vu_virtual_null_readlink (int * errn,
				     char * path,
				     char * buf,
				     int bufsize,
				     void * closure);
static int vu_virtual_null_rename (int * errn, char * from, char * to, void * closure);
static int vu_virtual_null_rmdir (int * errn, char * path, void * closure);
static int vu_virtual_null_stat (int * errn, char * path, struct stat * buf, void * closure);
static int vu_virtual_null_symlink (int * errn, char * from, char * to, void * closure);
static int vu_virtual_null_truncate (int * errn, char * path, off_t where, void * closure);
static int vu_virtual_null_unlink (int * errn, char * path, void * closure);
static int vu_virtual_null_utime (int * errn, char * path, struct utimbuf * times, void * closure);
static ssize_t vu_virtual_null_write (int * errn, int fd, char * buf, size_t count, void * closure);
static int vu_virtual_null_fcntl (int * errn, int fd, int cmd, long arg, void * closure);
static int vu_virtual_null_dup (int * errn, int fd, void * closure);
static int vu_virtual_null_dup2 (int * errn, int fd, int newfd, void * closure);
static int vu_virtual_null_move_state (int * errn, int fd, int newfd, void * closure);


static struct vu_fs_discipline vu_virtual_null_vtable
  = { VU_FS_DISCIPLINE_INITIALIZERS (vu_virtual_null_) };

/************************************************************************
 *(h0 "Virtual Null File Descriptors")
 * 
 * These VU functions define a trivial file system implementation in
 * which all functions act like the file is /dev/null but no actual
 * system calls are performed.
 *
 * Virtual /dev/null is considered a universally readable/writable,
 * permanently 0-length file whose permissions bits and ownerships can
 * not be changed by anyone (including root).  It's access,
 * modification and change times are permanently 0.
 *
 * The closure for opened files is the access flags cast to
 * (void *).
 */

static off_t * fd_offset = 0;

/*(c vu_make_virtual_null_fd)
 * int vu_make_virtual_null_fd (int * errn, int flags);
 * 
 * Return a new (pseudo) descriptor opened for
 * "virtual /dev/null".
 */
int
vu_make_virtual_null_fd (int * errn, int flags)
{
  int fd;

  fd = reserv_pseudo (errn, flags);
  if (fd >= 0)
    {
      off_t ** offset;
      offset = (off_t **)ar_ref ((void **)&fd_offset, lim_use_must_malloc, fd, sizeof (off_t));
      *offset = 0;
      vu_set_fd_handler (fd, &vu_virtual_null_vtable, (void *)flags);
    }
  return fd;
}


/*(c vu_make_virtual_null_fd_ge_n)
 * int vu_make_virtual_null_fd_ge_n (int * errn, int n, int flags);
 * 
 * Return a new (pseudo) descriptor opened for
 * "virtual /dev/null".
 * 
 * The new descriptor is greater than or equal to `n'.
 */
int
vu_make_virtual_null_fd_ge_n (int * errn, int n, int flags)
{
  int fd;

  fd = reserv_pseudo_ge_n (errn, n, flags);
  if (fd >= 0)
    {
      off_t ** offset;
      offset = (off_t **)ar_ref ((void **)&fd_offset, lim_use_must_malloc, fd, sizeof (off_t));
      *offset = 0;
      vu_set_fd_handler (fd, &vu_virtual_null_vtable, (void *)flags);
    }
  return fd;
}


static void *
vu_virtual_null_make_closure (void * closure)
{
  return closure;
}


static void
vu_virtual_null_free_closure (void * closure)
{}


static int
vu_virtual_null_access (int * errn, char * path, int mode, void * closure)
{
  if (mode & X_OK)
    {
      *errn = EACCES;
      return -1;
    }
  return 0;
}


static int
vu_virtual_null_chdir (int * errn, char * path, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_chmod (int * errn, char * path, int mode, void * closure)
{
  *errn = EPERM;
  return -1;
}


static int
vu_virtual_null_chown (int * errn, char * path, int owner, int group, void * closure)
{
  *errn = EPERM;
  return -1;
}


static int
vu_virtual_null_chroot (int * errn, char * path, void * closure)
{
  *errn = EPERM;
  return -1;
}


static int
vu_virtual_null_closedir (int * errn, DIR * dir, void * closure)
{
  *errn = ENOTDIR;
  return -1;
}


static int
vu_virtual_null_close (int * errn, int fd, void * closure)
{
  fd_offset[fd] = 0;
  unreserv_pseudo (fd);
  return 0;
}


static int
vu_virtual_null_fchdir (int * errn, int fd, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_fchmod (int * errn, int fd, int mode, void * closure)
{
  *errn = EPERM;
  return -1;
}


static int
vu_virtual_null_fchown (int * errn, int fd, int owner, int group, void * closure)
{
  *errn = EPERM;
  return -1;
}


static int
vu_virtual_null_fstat (int * errn, int fd, struct stat * buf, void * closure)
{
  buf->st_dev = 0;
  buf->st_ino = 0;
  buf->st_mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  buf->st_nlink = 1;
  buf->st_uid = 0;
  buf->st_gid = 0;
  buf->st_rdev = 0;
  buf->st_size = 0;
  buf->st_blksize = 4096;
  buf->st_blocks = 0;
  buf->st_atime = 0;
  buf->st_mtime = 0;
  buf->st_ctime = 0;
  return 0;
}


static int
vu_virtual_null_fsync (int * errn, int fd, void * closure)
{
  return 0;
}


static int
vu_virtual_null_ftruncate (int * errn, int fd, off_t where, void * closure)
{
  if (((int)closure == O_RDONLY) || (where != 0))
    {
      *errn = EIO;
      return -1;
    }
  return 0;
}


static int
vu_virtual_null_link (int * errn, char * from, char * to, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static off_t
vu_virtual_null_lseek (int * errn, int fd, off_t offset, int whence, void * closure)
{
  switch (whence)
    {
    case SEEK_END:
    case SEEK_SET:
      if (offset < 0)
	{
	offset_error:
	  *errn = EINVAL;
	  return -1;
	}
      fd_offset[fd] = offset;
      return offset;

    case SEEK_CUR:
      if ((fd_offset[fd] + offset) < 0)
	goto offset_error;

      fd_offset[fd] = fd_offset[fd] + offset;
      return fd_offset[fd];

    default:
      *errn = EINVAL;
      return -1;
    }
}


static int
vu_virtual_null_lstat (int * errn, char * path, struct stat * buf, void * closure)
{
  return vu_virtual_null_fstat (errn, 0, buf, closure);
}


static int
vu_virtual_null_mkdir (int * errn, char * path, int mode, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_open (int * errn, char * path, int flags, int mode, void * closure)
{
  return vu_make_virtual_null_fd (errn, flags);
}


static int
vu_virtual_null_opendir (int * errn, DIR ** retv,  char * path, void * closure)
{
  *errn = ENOTDIR;
  return -1;
}


static ssize_t
vu_virtual_null_read (int * errn, int fd, char * buf, size_t count, void * closure)
{
  if ((int)closure == O_WRONLY)
    {
      *errn = EBADF;
      return -1;
    }
  return 0;
}


static int
vu_virtual_null_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure)
{
  *errn = ENOTDIR;
  return -1;
}


static int
vu_virtual_null_readlink (int * errn,
			  char * path,
			  char * buf,
			  int bufsize,
			  void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
vu_virtual_null_rename (int * errn, char * from, char * to, void * closure)
{
  *errn = ENOSYS;
  return -1;
}

static int
vu_virtual_null_rmdir (int * errn, char * path, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_stat (int * errn, char * path, struct stat * buf, void * closure)
{
  return vu_virtual_null_fstat (errn, 0, buf, closure);
}


static int
vu_virtual_null_symlink (int * errn, char * from, char * to, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_truncate (int * errn, char * path, off_t where, void * closure)
{
  if (where != 0)
    {
      *errn = EIO;
      return -1;
    }
  return 0;
}


static int
vu_virtual_null_unlink (int * errn, char * path, void * closure)
{
  *errn = ENOSYS;
  return -1;
}


static int
vu_virtual_null_utime (int * errn, char * path, struct utimbuf * times, void * closure)
{
  *errn = EPERM;
  return -1;
}


static ssize_t
vu_virtual_null_write (int * errn, int fd, char * buf, size_t count, void * closure)
{
  if ((int)closure == O_RDONLY)
    {
      *errn = EBADF;
      return -1;
    }
  fd_offset[fd] += count;
  return count;
}


static int
vu_virtual_null_fcntl (int * errn, int fd, int cmd, long arg, void * closure)
{
  switch (cmd)
    {
    default:
      *errn = ENOSYS;
      return -1;

    case F_DUPFD:
      return vu_make_virtual_null_fd_ge_n (errn, arg, (int)closure);
      
    case F_GETFD:
      return FD_CLOEXEC;

    case F_SETFD:
      if (arg != FD_CLOEXEC)
	{
	  *errn = EINVAL;
	  return -1;
	}
      return 0;

    case F_GETFL:
      return O_NONBLOCK | (int)closure;
      
    case F_SETFL:
      if (arg != O_NONBLOCK)
	{
	  *errn = EINVAL;
	  return -1;
	}
      return 0;

    case F_GETOWN:
    case F_SETOWN:
      *errn = EINVAL;
      return -1;
    }
}


static int
vu_virtual_null_dup (int * errn, int fd, void * closure)
{
  return vu_virtual_null_fcntl (errn, fd, F_DUPFD, 0, closure);
}


static int
vu_virtual_null_dup2 (int * errn, int fd, int newfd, void * closure)
{
  return vu_virtual_null_fcntl (errn, fd, F_DUPFD, newfd, closure);
}


static int
vu_virtual_null_move_state (int * errn, int fd, int newfd, void * closure)
{
  off_t ** offset_fd;
  off_t ** offset_newfd;

  if (fd == newfd)
    return fd;

  offset_fd = (off_t **)ar_ref ((void **)&fd_offset, lim_use_must_malloc, fd, sizeof (off_t));
  offset_newfd = (off_t **)ar_ref ((void **)&fd_offset, lim_use_must_malloc, newfd, sizeof (off_t));
  *offset_newfd = *offset_fd;

  vu_set_fd_handler (newfd, &vu_virtual_null_vtable, closure);
  return 0;
}

