/* url-fd.c - the fd: URL file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/vu/url-fd.h"


/************************************************************************
 *(h0 "A VU Handler for the URL File-Descriptor Scheme")
 * 
 * These functions provide a VU namespace handler for file-names of
 * the form:
 * 
 * 		fd:N
 * 
 * where `N' is a non-negative integer.
 * 
 * Opening such a file, with this namespace handler installed, returns
 * the indicated file descriptor.
 */



static struct vu_fs_discipline url_fd_vtable;


/* __STDC__ prototypes for static functions */
static void * url_fd_make_closure (void * closure);
static void url_fd_free_closure (void * closure);
static int url_fd_access (int * errn, char * path, int mode, void * closure);
static int url_fd_chdir (int * errn, char * path, void * closure);
static int url_fd_chmod (int * errn, char * path, int mode, void * closure);
static int url_fd_chown (int * errn, char * path, int owner, int group, void * closure);
static int url_fd_chroot (int * errn, char * path, void * closure);
static int url_fd_close (int * errn, int fd, void * closure);
static int url_fd_closedir (int * errn, DIR * dir, void * closure);
static int url_fd_fchdir (int * errn, int fd, void * closure);
static int url_fd_fchmod (int * errn, int fd, int mode, void * closure);
static int url_fd_fchown (int * errn, int fd, int owner, int group, void * closure);
static int url_fd_fstat (int * errn, int fd, struct stat * buf, void * closure);
static int url_fd_fsync (int * errn, int fd, void * closure);
static int url_fd_ftruncate (int * errn, int fd, off_t where, void * closure);
static int url_fd_link (int * errn, char * from, char * to, void * closure);
static off_t url_fd_lseek (int * errn, int fd, off_t offset, int whence, void * closure);
static int url_fd_lstat (int * errn, char * path, struct stat * buf, void * closure);
static int url_fd_mkdir (int * errn, char * path, int mode, void * closure);
static int url_fd_open (int * errn, char * path, int flags, int mode, void * closure);
static int url_fd_opendir (int * errn, DIR ** retv,  char * path, void * closure);
static ssize_t url_fd_read (int * errn, int fd, char * buf, size_t count, void * closure);
static int url_fd_readdir (int * errn, struct alloc_limits * limits, char ** file, DIR * dir, void * closure);
static int url_fd_readlink (int * errn, char * path, char * buf, int bufsize, void * closure);
static int url_fd_rename (int * errn, char * from, char * to, void * closure);
static int url_fd_rmdir (int * errn, char * path, void * closure);
static int url_fd_stat (int * errn, char * path, struct stat * buf, void * closure);
static int url_fd_symlink (int * errn, char * from, char * to, void * closure);
static int url_fd_truncate (int * errn, char * path, off_t where, void * closure);
static int url_fd_unlink (int * errn, char * path, void * closure);
static int url_fd_utime (int * errn, char * path, struct utimbuf * times, void * closure);
static ssize_t url_fd_write (int * errn, int fd, char * buf, size_t count, void * closure);
static int url_fd_fcntl (int * errn, int fd, int cmd, long arg, void * closure);
static int url_fd_dup (int * errn, int fd, void * closure);
static int url_fd_dup2 (int * errn, int fd, int newfd, void * closure);
static int url_fd_move_state (int * errn, int fd, int newfd, void * closure);



/*(c url_fd_push_handler)
 * void url_fd_push_handler (int is_optional);
 * 
 * Push a VU namespace handler named "fd" which recognizes file-names
 * using the regexp:
 * 
 *		^fd:[0-9]\+
 *
 * It handles file names like:
 * 
 * 		fd:N
 * 
 * where `N' is a non-negative integer.
 * 
 * Opening such a file, with this namespace handler installed, returns
 * the indicated file descriptor.
 * 
 * If the flag `is_optional' is 0, the namespace handler is simply
 * installed.  If it is not 0, the handler is registered under the
 * name `"fd"', but not installed.  
 * 
 * (See xref:"vu_enable_optional_name_handler".)
 */
void
url_fd_push_handler (int is_optional)
{
  static int initialized = 0;
  static regex_t url_fd_regex;
  static t_uchar * doc[] =
    {
      "fd:N",
      "File descriptor N.",
      0
    };

  if (!initialized)
    {
      if (0 > regcomp (&url_fd_regex, "^fd:[0-9]\\+", 0))
	panic ("unable to compile regexp");
      initialized = 1;
    }
  
  vu_push_name_handler ("fd", doc, &url_fd_regex, 0, &url_fd_vtable, 0, is_optional);
}


/*(c url_fd_to_fd)
 * int url_fd_to_fd (int * errn, char * path);
 * 
 * Return the file descriptor number named by a filename of the form:
 * 
 * 	fd:N
 * 
 * where `N' is a non-negative integer.
 * 
 * If the filename can not be parsed, -1 is returned and `*errn' is
 * set.
 */
int
url_fd_to_fd (int * errn, char * path)
{
  char * fd_spec;
  int fd;

  fd_spec = str_chr_index (path, ':');
  if (!fd_spec)
    {
      *errn = ENOENT;
      return -1;
    }

  ++fd_spec;
  if (0 > cvt_decimal_to_int (errn, &fd, fd_spec, str_length (fd_spec)))
    return -1;

  return fd;
}



static void *
url_fd_make_closure (void * closure)
{
  return 0;
}


static void
url_fd_free_closure (void * closure)
{
}


static int
url_fd_access (int * errn, char * path, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_chdir (int * errn, char * path, void * closure)
{
  int fd;

  fd = url_fd_to_fd (errn, path);
  if (0 > fd)
    return -1;
  return vu_sys_fchdir (errn, fd, 0);
}


static int
url_fd_chmod (int * errn, char * path, int mode, void * closure)
{
  int fd;

  fd = url_fd_to_fd (errn, path);
  if (0 > fd)
    return -1;
  return vu_sys_fchmod (errn, fd, mode, 0);
}


static int
url_fd_chown (int * errn, char * path, int owner, int group, void * closure)
{
  int fd;

  fd = url_fd_to_fd (errn, path);
  if (0 > fd)
    return -1;
  return vu_sys_fchown (errn, fd, owner, group, 0);
}


static int
url_fd_chroot (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_close (int * errn, int fd, void * closure)
{
  return vu_sys_close (errn, fd, 0);
}


static int
url_fd_closedir (int * errn, DIR * dir, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_fchdir (int * errn, int fd, void * closure)
{
  return vu_sys_fchdir (errn, fd, 0);
}


static int
url_fd_fchmod (int * errn, int fd, int mode, void * closure)
{
  return vu_sys_fchmod (errn, fd, mode, 0);
}


static int
url_fd_fchown (int * errn, int fd, int owner, int group, void * closure)
{
  return vu_sys_fchown (errn, fd, owner, group, 0);
}


static int
url_fd_fstat (int * errn, int fd, struct stat * buf, void * closure)
{
  return vu_sys_fstat (errn, fd, buf, 0);
}


static int
url_fd_fsync (int * errn, int fd, void * closure)
{
  return vu_sys_fsync (errn, fd, 0);
}


static int
url_fd_ftruncate (int * errn, int fd, off_t where, void * closure)
{
  return vu_sys_ftruncate (errn, fd, where, 0);
}


static int
url_fd_link (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static off_t
url_fd_lseek (int * errn, int fd, off_t offset, int whence, void * closure)
{
  return vu_sys_lseek (errn, fd, offset, whence, 0);
}


static int
url_fd_lstat (int * errn, char * path, struct stat * buf, void * closure)
{
  return url_fd_stat (errn, path, buf, closure);
}


static int
url_fd_mkdir (int * errn, char * path, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_open (int * errn, char * path, int flags, int mode, void * closure)
{
  return url_fd_to_fd (errn, path);
}


static int
url_fd_opendir (int * errn, DIR ** retv,  char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static ssize_t
url_fd_read (int * errn, int fd, char * buf, size_t count, void * closure)
{
  return vu_sys_read (errn, fd, buf, count, 0);
}


static int
url_fd_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_readlink (int * errn, char * path, char * buf, int bufsize, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_rename (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_rmdir (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_stat (int * errn, char * path, struct stat * buf, void * closure)
{
  int fd;

  fd = url_fd_to_fd (errn, path);
  if (0 > fd)
    return -1;
  return vu_sys_fstat (errn, fd, buf, 0);
}


static int
url_fd_symlink (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_truncate (int * errn, char * path, off_t where, void * closure)
{
  int fd;

  fd = url_fd_to_fd (errn, path);
  if (0 > fd)
    return -1;
  return vu_sys_ftruncate (errn, fd, where, 0);
}


static int
url_fd_unlink (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_fd_utime (int * errn, char * path, struct utimbuf * times, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static ssize_t
url_fd_write (int * errn, int fd, char * buf, size_t count, void * closure)
{
  return vu_sys_write (errn, fd, buf, count, 0);
}

static int
url_fd_fcntl (int * errn, int fd, int cmd, long arg, void * closure)
{
  return vu_sys_fcntl (errn, fd, cmd, arg, 0);
}


static int
url_fd_dup (int * errn, int fd, void * closure)
{
  return vu_sys_dup (errn, fd, 0);
}


static int
url_fd_dup2 (int * errn, int fd, int newfd, void * closure)
{
  return vu_sys_dup2 (errn, fd, newfd, 0);
}

static int
url_fd_move_state (int * errn, int fd, int newfd, void * closure)
{
  return vu_sys_move_state (errn, fd, newfd, 0);
}


static struct vu_fs_discipline url_fd_vtable = { VU_FS_DISCIPLINE_INITIALIZERS (url_fd_) };

