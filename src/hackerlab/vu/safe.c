/* safe.c - error-free I/O routines (panic on error)
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/stdarg.h"
#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/vu-utils.h"
#include "hackerlab/vu/safe.h"


/************************************************************************
 *(h0 "An Errorless Front-end to the VU File-system Interface")
 * 
 * These functions perform I/O but never return errors.  If an error
 * occurs, they print a message and abort the program.
 * 
 */




/*(c safe_access)
 * int safe_access (char * path, int mode);
 * 
 * See xref:"vu_access".
 */
int
safe_access (char * path, int mode)
{
  int errn;
  int answer;
  answer = vu_access (&errn, path, mode);
  if (answer < 0)
    {
      printfmt (&errn, 2, "Error during call to `vu_access' for %s (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_chdir)
 * int safe_chdir (char * path);
 * 
 * See xref:"vu_chdir".
 */
int
safe_chdir (char * path)
{
  int errn;
  int answer;
  answer = vu_chdir (&errn, path);
  if (answer < 0)
    {
      printfmt (&errn, 2, "Error during call to `vu_chdir' for %s (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_chmod)
 * int safe_chmod (char * path, int mode);
 * 
 * See xref:"vu_chmod".
 */
int
safe_chmod (char * path, int mode)
{
  int errn;
  int answer;
  answer = vu_chmod (&errn, path, mode);
  if (answer < 0)
    {
      printfmt (&errn, 2, "Error during call to `vu_chmod' for %s (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_chown)
 * int safe_chown (char * path, int owner, int group);
 * 
 * See xref:"vu_chown".
 */
int
safe_chown (char * path, int owner, int group)
{
  int errn;
  int answer;
  answer = vu_chown (&errn, path, owner, group);
  if (answer < 0)
    {
      printfmt (&errn, 2, "Error during call to `vu_chown' for %s (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_chroot)
 * int safe_chroot (char * path);
 * 
 * See xref:"vu_chroot".
 */
int
safe_chroot (char * path)
{
  int errn;
  int answer;
  answer = vu_chroot (&errn, path);
  if (answer < 0)
    {
      printfmt (&errn, 2, "Error during call to `vu_chroot' for %s (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_close)
 * int safe_close (int fd);
 * 
 * See xref:"vu_close".
 */
int
safe_close (int fd)
{
  int errn;
  int answer;

  answer = vu_close (&errn, fd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error while closing descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_closedir)
 * int safe_closedir (DIR * dir);
 * 
 * See xref:"vu_closedir".
 */
int
safe_closedir (DIR * dir)
{
  int errn;
  int answer;

  answer = vu_closedir (&errn, dir);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error while closing directory (%s)\n", errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fchdir)
 * int safe_fchdir (int fd);
 * 
 * See xref:"vu_fchdir".
 */
int
safe_fchdir (int fd)
{
  int errn;
  int answer;

  answer = vu_fchdir (&errn, fd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fchdir' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fchmod)
 * int safe_fchmod (int fd, int mode);
 * 
 * See xref:"vu_fchmod".
 */
int
safe_fchmod (int fd, int mode)
{
  int errn;
  int answer;

  answer = vu_fchmod (&errn, fd, mode);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fchmod' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fchown)
 * int safe_fchown (int fd, int owner, int group);
 * 
 * See xref:"vu_fchown".
 */
int
safe_fchown (int fd, int owner, int group)
{
  int errn;
  int answer;

  answer = vu_fchown (&errn, fd, owner, group);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fchgrp' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fstat)
 * int safe_fstat (int fd, struct stat * buf);
 * 
 * See xref:"vu_fstat".
 */
int
safe_fstat (int fd, struct stat * buf)
{
  int errn;
  int answer;

  answer = vu_fstat (&errn, fd, buf);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fstat' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fsync)
 * int safe_fsync (int fd);
 * 
 * See xref:"vu_fsync".
 */
int
safe_fsync (int fd)
{
  int errn;
  int answer;

  answer = vu_fsync (&errn, fd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fsync' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_ftruncate)
 * int safe_ftruncate (int fd, long where);
 * 
 * See xref:"vu_ftruncate".
 */
int
safe_ftruncate (int fd, long where)
{
  int errn;
  int answer;

  answer = vu_ftruncate (&errn, fd, where);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_ftruncate' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_link)
 * int safe_link (char * from, char * to);
 * 
 * See xref:"vu_link".
 */
int
safe_link (char * from, char * to)
{
  int errn;
  int answer;

  answer = vu_link (&errn, from, to);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_link' to link \"%s\" to \"%s\" (%s)\n", to, from, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_lseek)
 * long safe_lseek (int fd, long offset, int whence);
 * 
 * See xref:"vu_lseek".
 */
long
safe_lseek (int fd, long offset, int whence)
{
  int errn;
  long answer;

  answer = vu_lseek (&errn, fd, offset, whence);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_lseek' for descriptor %d, offset %ld, whence=%d (%s)\n", fd, offset, whence, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_lstat)
 * int safe_lstat (char * path, struct stat * buf);
 * 
 * See xref:"vu_lstat".
 */
int
safe_lstat (char * path, struct stat * buf)
{
  int errn;
  int answer;

  answer = vu_lstat (&errn, path, buf);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_lstat' for \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_mkdir)
 * int safe_mkdir (char * path, int mode);
 * 
 * See xref:"vu_mkdir".
 */
int
safe_mkdir (char * path, int mode)
{
  int errn;
  int answer;

  answer = vu_mkdir (&errn, path, mode);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to create directory \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}

/*(c safe_open)
 * int safe_open (char * path, int flags, int mode);
 * 
 * See xref:"vu_open".
 */
int
safe_open (char * path, int flags, int mode)
{
  int errn;
  int fd;

  fd = vu_open (&errn, path, flags, mode);
  if (0 > fd)
    {
      printfmt (&errn, 2, "unable to open file \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return fd;
}

/*(c safe_opendir)
 * int safe_opendir (DIR ** retv, char * path);
 * 
 * See xref:"vu_opendir".
 */
int
safe_opendir (DIR ** retv, char * path)
{
  int errn;
  int answer;

  answer = vu_opendir (&errn, retv, path);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to open directory \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_read)
 * long safe_read (int fd, char * buf, long count);
 * 
 * See xref:"vu_read".
 */
long
safe_read (int fd, char * buf, long count)
{
  int errn;
  long answer;

  answer = vu_read (&errn, fd, buf, count);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_read' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_read_retry)
 * long safe_read_retry (int fd, char * buf, long count);
 * 
 * See xref:"vu_read_retry".
 */
long
safe_read_retry (int fd, char * buf, long count)
{
  int errn;
  long answer;

  answer = vu_read_retry (&errn, fd, buf, count);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_read_retry' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_readdir)
 * int safe_readdir (char ** file_ret, DIR * dir);
 * 
 * See xref:"vu_readdir".
 */
int
safe_readdir (char ** file_ret, DIR * dir)
{
  int errn;
  int answer;

  answer = vu_readdir (&errn, 0, file_ret, dir);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to read directory (%s)\n", errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_readlink)
 * int safe_readlink (char * path, char * buf, int bufsize);
 * 
 * See xref:"vu_readlink".
 */
int
safe_readlink (char * path, char * buf, int bufsize)
{
  int errn;
  int answer;

  answer = vu_readlink (&errn, path, buf, bufsize);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to read link \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_rename)
 * int safe_rename (char * from, char * to);
 * 
 * See xref:"vu_rename".
 */
int
safe_rename (char * from, char * to)
{
  int errn;
  int answer;

  answer = vu_rename (&errn, from, to);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to rename \"%s\" to \"%s\" (%s)\n", from, to, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_rmdir)
 * int safe_rmdir (char * path);
 * 
 * See xref:"vu_rmdir".
 */
int
safe_rmdir (char * path)
{
  int errn;
  int answer;

  answer = vu_rmdir (&errn, path);
  if (0 > answer)
    {
      printfmt (&errn, 2, "unable to rmdir \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}



/*(c safe_stat)
 * int safe_stat (char * path, struct stat * buf);
 * 
 * See xref:"vu_stat".
 */
int
safe_stat (char * path, struct stat * buf)
{
  int errn;
  int answer;

  answer = vu_stat (&errn, path, buf);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_stat' for \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_symlink)
 * int safe_symlink (char * from, char * to);
 * 
 * See xref:"vu_symlink".
 */
int
safe_symlink (char * from, char * to)
{
  int errn;
  int answer;

  answer = vu_symlink (&errn, from, to);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_symlink' to link \"%s\" to \"%s\" (%s)\n", to, from, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}



/*(c safe_truncate)
 * int safe_truncate (char * path, long where);
 * 
 * See xref:"vu_truncate".
 */
int
safe_truncate (char * path, long where)
{
  int errn;
  int answer;

  answer = vu_truncate (&errn, path, where);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_truncate' to truncate \"%s\" at %ld (%s)\n", path, where, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_unlink)
 * int safe_unlink (char * path);
 * 
 * See xref:"vu_unlink".
 */
int
safe_unlink (char * path)
{
  int errn;
  int answer;

  answer = vu_unlink (&errn, path);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_unlink' to remove \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_utime)
 * int safe_utime (char * path, struct utimbuf * times);
 * 
 * See xref:"vu_utime".
 */
int
safe_utime (char * path, struct utimbuf * times)
{
  int errn;
  int answer;

  answer = vu_utime (&errn, path, times);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_utime' to change timestamps of \"%s\" (%s)\n", path, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_write)
 * long safe_write (int fd, char * buf, long count);
 * 
 * See xref:"vu_write".
 */
long
safe_write (int fd, char * buf, long count)
{
  int errn;
  long answer;

  answer = vu_write (&errn, fd, buf, count);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_write' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_write_retry)
 * long safe_write_retry (int fd, t_uchar * buf, int amt);
 * 
 * See xref:"vu_write_retry".
 */
long
safe_write_retry (int fd, t_uchar * buf, int amt)
{
  int errn;
  long answer;

  answer = vu_write_retry (&errn, fd, buf, amt);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_write_retry' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_fcntl)
 * int safe_fcntl (int fd, int cmd, long arg);
 * 
 * See xref:"vu_fcntl".
 */
int
safe_fcntl (int fd, int cmd, long arg)
{
  int errn;
  int answer;

  answer = vu_fcntl (&errn, fd, cmd, arg);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_fcntl' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_dup)
 * int safe_dup (int fd);
 * 
 * See xref:"vu_dup".
 */
int
safe_dup (int fd)
{
  int errn;
  int answer;

  answer = vu_dup (&errn, fd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_dup' for descriptor %d (%s)\n", fd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


/*(c safe_dup2)
 * int safe_dup2 (int fd, int newfd);
 * 
 * See xref:"vu_dup2".
 */
int
safe_dup2 (int fd, int newfd)
{
  int errn;
  int answer;

  answer = vu_dup2 (&errn, fd, newfd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_dup2' to duplicate %d to %d (%s)\n", fd, newfd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}



/*(c safe_move_state)
 * int safe_move_state (int fd, int newfd);
 * 
 * See xref:"vu_move_state".
 */
int
safe_move_state (int fd, int newfd)
{
  int errn;
  int answer;

  answer = vu_move_state (&errn, fd, newfd);
  if (0 > answer)
    {
      printfmt (&errn, 2, "Error calling `vu_move_state' to copy VU state for descriptor %d to descriptor %d (%s)\n", fd, newfd, errno_to_string (errn));
      panic ("I/O error");
    }
  return answer;
}


