/* vuprocs.c - procedures for vu functions
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include "hackerlab/vu/reserv.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vu-virtual-null.h"
#include "hackerlab/vu/vfdbuf.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/vuprocs.h"


/************************************************************************
 *(h1 "Pseudo-Descriptors")
 * 
 * Pseudo-descriptors are descriptor objects (ports) that do not 
 * correspond to a descriptor maintained by the operating system.
 * They are used, for example, to implement string ports.
 */




SCM_SYMBOL (s_open_flag, "open-flag");
SCM_SYMBOL (s_vfdbuf_buffer_flag, "vfdbuf-buffer-flag");



/*(c %reserv)
 * (%reserv flags)
 * 
 * Allocate a file descriptor by using `%open' to open `"/dev/null"'.
 *
 * `flags' is as the parameter of the same name to `%open'.
 */
SCM_PROC (s_sys_reserv, "%reserv", 0, 0, 0, scm_sys_reserv);
SCM
scm_sys_reserv (SCM sflags)
{
  SCM_INTS_ENABLED;
  int flags;
  int fd;
  int errn;
  SCM answer;

  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg1, s_sys_reserv);

  SCM_DEFER_INTS;
  fd = reserv (&errn, flags);
  if (fd < 0)
    answer = scm_makerrno (errn);
  else
    scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return answer;
}


/*(c %reserv-pseudo)
 * (%reserv-pseudo flags)
 * 
 * Reserve a pseudo file descriptor suitable for use with
 * `set-fd-handler'.
 *
 * `flags' which may be one of `O_RDONLY', `O_RDWR' etc.
 *
 * A pseudo file descriptor can be used with file system functions and
 * is guaranteed not to be the same as any real file descriptor.
 */
SCM_PROC (s_sys_reserv_pseudo, "%reserv-pseudo", 1, 0, 0, scm_sys_reserv_pseudo);
SCM
scm_sys_reserv_pseudo (SCM sflags)
{
  SCM_INTS_ENABLED;
  int flags;
  int fd;
  int errn;
  SCM answer;

  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg1, s_sys_reserv_pseudo);

  SCM_DEFER_INTS;
  fd = reserv_pseudo (&errn, flags);
  if (fd < 0)
    answer = scm_makerrno (errn);
  else
    scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return answer;
}



/************************************************************************
 *(h1 "Virtual Null Descriptors")
 * 
 * A virtual null descriptor is a pseudo-descriptor that is perpetually
 * at end-of-file, and that discards all output.
 */


/*(c %virtual-null-fd)
 * (%virtual-null-fd flags)
 * 
 * Return a pseudo-file descriptor that acts as if opened
 * for `/dev/null'.
 */
SCM_PROC (s_sys_virtual_null_fd, "%virtual-null-fd", 1, 0, 0, scm_sys_virtual_null_fd);
SCM
scm_sys_virtual_null_fd (SCM sflags)
{
  SCM_INTS_ENABLED;
  int flags;
  int fd;
  int errn;
  SCM answer;

  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg1, s_sys_virtual_null_fd);

  SCM_DEFER_INTS;
  fd = vu_make_virtual_null_fd (&errn, flags);
  if (fd < 0)
    answer = scm_makerrno (fd);
  else
    answer = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return answer;
}


/************************************************************************
 *(h1 "Descriptor Buffering")
 * 
 * An individual file descriptor (port) may have an associated buffer.
 * 
 */


/*(c %vfdbuf-buffer-fd)
 * (%vfdbuf-buffer-fd fd bufsize access-flags :rest buffer-flags)
 * 
 * Ensure that `fd' is buffered.
 * 
 * `bufsize' is the size of the buffer, 0 or #f for a 
 * default buffer size.
 * 
 * `access-flags' which may be one of `O_RDONLY', `O_RDWR' etc.
 * 
 * `buffer-flags' is a list of symbols that can effect
 * buffering.  Supported flags are:
 * 
 * 	vfdbuf_auto_shift
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_buffer_fd, "%vfdbuf-buffer-fd", 3, 1, 0, scm_sys_vfdbuf_buffer_fd);
SCM
scm_sys_vfdbuf_buffer_fd (SCM sfd, SCM sbufsize, SCM sacc_flags, SCM sbuffer_flags)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  unsigned long bufsize;
  int acc_flags;
  int buffer_flags;
  int got;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_buffer_fd);

  if (sbufsize == SCM_BOOL_F)
    bufsize = 0;
  else
    bufsize = scm_num2ulong (sbufsize, scm_arg2, s_sys_vfdbuf_buffer_fd);

  acc_flags = scm_integer_logior_cpp_constants (s_open_flag, sacc_flags, scm_arg3, s_sys_vfdbuf_buffer_fd);
  buffer_flags = scm_integer_logior_cpp_constants (s_vfdbuf_buffer_flag, sbuffer_flags, scm_arg4, s_sys_vfdbuf_buffer_fd);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  got = vfdbuf_buffer_fd (&errn, fd, bufsize, acc_flags, vfdbuf_add_zero_byte | buffer_flags);
  if (got < 0)
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return answer;
}


/*(c vfdbuf-is-buffered?)
 * (vfdbuf-is-buffered? fd)
 * 
 * Return #t if `fd' is buffered, #f otherwise.
 */
SCM_PROC (s_vfdbuf_is_buffered_p, "vfdbuf-is-buffered?", 1, 0, 0, scm_vfdbuf_is_buffered_p);
SCM
scm_vfdbuf_is_buffered_p (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int is;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_vfdbuf_is_buffered_p);
  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  is = vfdbuf_is_buffered (fd);
  SCM_ALLOW_INTS;
  return is ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c %vfdbuf-dont-flush)
 * (%vfdbuf-dont-flush :optional fd setting)
 * 
 * Set the `dont-flush' flag for buffered descriptor `fd'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */

SCM_PROC (s_sys_vfdbuf_set_dont_flush, "%vfdbuf-dont-flush", 0, 2, 0, scm_sys_vfdbuf_set_dont_flush);
SCM
scm_sys_vfdbuf_set_dont_flush (SCM sfd, SCM setting)
{
  SCM_INTS_ENABLED;
  int fd;
  int set;
  int errn;
  SCM answer;

  if ((SCM_UNDEFINED == sfd) || (SCM_BOOL_F == sfd))
    sfd = scm_cur_inp;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_set_dont_flush);

  if (SCM_UNDEFINED == setting)
    set = 1;
  else
    set = (SCM_BOOL_F != setting);
  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  if (0 > vfdbuf_set_dont_flush (&errn, fd, set))
    answer = scm_makerrno (errn);
  else
    answer = (set ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %vfdbuf-is-dont-flush?)
 * (%vfdbuf-is-dont-flush? :optional fd)
 * 
 * Return #t if flushing is disabled for buffered descriptor `fd',
 * #f otherwise.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 * 
 */
SCM_PROC (s_sys_vfdbuf_flushing_disabled_p, "%vfdbuf-is-dont-flush?", 1, 0, 0, scm_sys_vfdbuf_flushing_disabled_p);
SCM
scm_sys_vfdbuf_flushing_disabled_p (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int rv;
  SCM answer;

  if ((SCM_UNDEFINED == sfd) || (SCM_BOOL_F == sfd))
    sfd = scm_cur_inp;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_set_dont_flush);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  rv = vfdbuf_is_dont_flush (&errn, fd);
  if (0 > rv)
    answer = scm_makerrno (errn);
  else
    answer = (rv ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_ALLOW_INTS;
  return answer;
}

/*(c %vfdbuf-shift)
 * (%vfdbuf-shift fd amt)
 * 
 * Discard `amt' buffered characters from the buffer for descriptor `fd'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_shift, "%vfdbuf-shift", 2, 0, 0, scm_sys_vfdbuf_shift);
SCM
scm_sys_vfdbuf_shift (SCM sfd, SCM samt)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  off_t amt;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_shift);
  amt = scm_num2long (samt, scm_arg2, s_sys_vfdbuf_shift);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  if (0 > vfdbuf_shift (&errn, fd, amt))
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %vfdbuf-advance)
 * (%vfdbuf-advance fd amt)
 * 
 * Advance the `read_write_pos' of buffered descriptor `fd' by
 * `amt' characters.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_advance, "%vfdbuf-advance", 2, 0, 0, scm_sys_vfdbuf_advance);
SCM
scm_sys_vfdbuf_advance (SCM sfd, SCM samt)
{
  SCM_INTS_ENABLED;
  int fd;
  unsigned long amt;
  int errn;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_advance);
  amt = scm_num2ulong (samt, scm_arg2, s_sys_vfdbuf_advance);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  if (0 > vfdbuf_advance (&errn, fd, amt))
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}



/*(c %vfdbuf-flush)
 * (%vfdbuf-flush fd)
 * 
 * Flush buffered output for descriptor `fd'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_flush, "%vfdbuf-flush", 1, 0, 0, scm_sys_vfdbuf_flush);
SCM
scm_sys_vfdbuf_flush (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int got;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_flush);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  got = vfdbuf_flush (&errn, fd);
  if (got >= 0)
    answer = SCM_BOOL_T;
  else
    answer = scm_makerrno (errn);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %vfdbuf-is-eof)
 * (%vfdbuf-is-eof fd)
 * 
 * Return #t if buffered descriptor `fd' is at the the end-of-file,
 * #f otherwise.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_is_eof, "%vfdbuf-is-eof", 1, 0, 0, scm_sys_vfdbuf_is_eof);
SCM
scm_sys_vfdbuf_is_eof (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int got;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_is_eof);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  got = vfdbuf_is_eof (&errn, fd);
  if (got >= 0)
    answer = scm_int_to_bool (got);
  else
    answer = scm_makerrno (errn);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %vfdbuf-return)
 * (%vfdbuf-return fd str)
 * 
 * ``Unread'' the characters in `str' for buffered descriptor `fd'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_return, "%vfdbuf-return", 2, 0, 0, scm_sys_vfdbuf_return);
SCM
scm_sys_vfdbuf_return (SCM sfd, SCM str)
{
  SCM_INTS_ENABLED;
  int fd;
  t_uchar * chrs;
  int amt;
  int errn;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_return);

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg2, s_sys_vfdbuf_return);
  amt = SCM_RO_LENGTH (str);
  chrs = SCM_RO_UCHARS (str);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  if (0 > vfdbuf_return (&errn, fd, chrs, (unsigned long)amt))
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


static void
free_buffer (t_uchar * buffer, void * vfd)
{
  SCM_INTS_DISABLED;
  int fd;

  fd = (int)vfd;
  scm_hashq_remove_x (scm_buffer_strings, SCM_MAKINUM (fd));
}


/*(c %vfdbuf-get-buffered)
 * (%vfdbuf-get-buffered)
 * 
 * Return a shared substring that refers to the unread characters
 * currently buffered for descriptor `fd'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_get_buffered, "%vfdbuf-get-buffered", 1, 0, 0, scm_sys_vfdbuf_get_buffered);
SCM
scm_sys_vfdbuf_get_buffered (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  SCM buffer_str;
  t_uchar * bufbase;
  unsigned long bufsize;
  t_uchar * read_write_pos;
  unsigned long buffered;
  int errn;
  SCM answer;

  
  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_get_buffered);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  buffer_str = scm_hashq_ref (scm_buffer_strings, SCM_MAKINUM (fd), SCM_BOOL_F);
  if (buffer_str != SCM_BOOL_F)
    bufbase = SCM_STRING_UCHARS (buffer_str);
  else
    bufbase = 0;
  if (0 > vfdbuf_takebuf (&errn,
			  &bufbase,
			  &bufsize,
			  &read_write_pos,
			  &buffered,
			  0,
			  fd,
			  free_buffer, (void *)fd))
    answer = scm_makerrno (errn);
  else
    {
      if ((buffer_str == SCM_BOOL_F) || (SCM_STRING_UCHARS (buffer_str) != bufbase))
	{
	  buffer_str = scm_take_str (bufbase, bufsize);
	  scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (fd), buffer_str);
	}
      {
	long rwpos;
	rwpos = read_write_pos - bufbase;
	answer = scm_listify (buffer_str,
			      scm_make_shared_substring (buffer_str,
							 SCM_INUM0,
							 SCM_MAKINUM (rwpos)),
			      scm_make_shared_substring (buffer_str,
							 SCM_MAKINUM (rwpos),
							 SCM_MAKINUM (rwpos
								      + (buffered > 0
									 ? buffered
									 : 0))),
			      SCM_UNDEFINED);
      }
    }
  SCM_ALLOW_INTS;
  return answer;
}



/*(c %vfdbuf-set-buffer)
 * (%vfdbuf-set-buffer fd buffer :optional read-write-pos buffered)
 * 
 * Make the string `buffer' the buffer for descriptor `fd'.
 * 
 * Optional parameters `read-write-pos' and `buffered' are the
 * offset and length within that string of unread characters.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_set_buffer, "%vfdbuf-set-buffer", 2, 2, 0, scm_sys_vfdbuf_set_buffer);
SCM
scm_sys_vfdbuf_set_buffer (SCM sfd, SCM sbuffer, SCM sread_write_pos, SCM sbuffered)
{
  SCM_INTS_ENABLED;
  int fd;
  t_uchar * buf;
  long bufsize;
  long read_write_pos;
  long buffered;
  int errn;
  int stat;
  
  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_set_buffer);
  SCM_ASSERT (scm_is_ro_string (sbuffer), sbuffer, scm_arg2, s_sys_vfdbuf_set_buffer);

  buf = SCM_RO_UCHARS (sbuffer);
  bufsize = SCM_LENGTH (sbuffer);

  if ((sread_write_pos == SCM_UNDEFINED) || (sread_write_pos == SCM_BOOL_F))
    read_write_pos = 0;
  else
    read_write_pos = scm_num2long (sread_write_pos, scm_arg3, s_sys_vfdbuf_set_buffer);

  if ((sbuffered == SCM_UNDEFINED) || (sbuffered == SCM_BOOL_F))
    buffered = bufsize;
  else
    buffered = scm_num2long (sbuffered, scm_arg4, s_sys_vfdbuf_set_buffer);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  stat = vfdbuf_set_buffer (&errn, fd, buf, bufsize, read_write_pos, buffered, 0, free_buffer, (void *)fd);
  if (stat)
    scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (fd), sbuffer);
  SCM_ALLOW_INTS;

  if (stat)
    return scm_makerrno (errn);
  else
    return sbuffer;
}


/*(c %vfdbuf-more)
 * (%vfdbuf-more fd amt)
 * 
 * Increase the number of characters buffered for input descriptor `fd'
 * by `amt'.
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_vfdbuf_more, "%vfdbuf-more", 1, 1, 0, scm_sys_vfdbuf_more);
SCM
scm_sys_vfdbuf_more (SCM sfd, SCM samt)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  unsigned long amt;
  SCM buffer_str;
  t_uchar * bufbase;
  unsigned long bufsize;
  t_uchar * read_write_pos;
  unsigned long buffered;
  int got;
  SCM answer;
  
  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_vfdbuf_more);

  if ((samt == SCM_UNDEFINED) || (samt == SCM_BOOL_F))
    amt = 0;
  else
    amt = scm_num2ulong (samt, scm_arg2, s_sys_vfdbuf_more);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  got = vfdbuf_more (&errn, &bufbase, &bufsize, &read_write_pos, &buffered, 0, fd, amt);
  answer = (got >= 0) ? SCM_BOOL_F : scm_makerrno (errn);
  if (got > 0)
    {
      buffer_str = scm_hashq_ref (scm_buffer_strings, SCM_MAKINUM (fd), SCM_BOOL_F);
      if (buffer_str != SCM_BOOL_F)
	bufbase = SCM_STRING_UCHARS (buffer_str);
      else
	bufbase = 0;
      if (0 > vfdbuf_takebuf (&errn,
			      &bufbase,
			      &bufsize,
			      &read_write_pos,
			      &buffered,
			      0,
			      fd,
			      free_buffer, (void *)fd))
	answer = scm_makerrno (errn);
      else
	{
	  if ((buffer_str == SCM_BOOL_F) || (SCM_STRING_UCHARS (buffer_str) != bufbase))
	    {
	      buffer_str = scm_take_str (bufbase, bufsize);
	      scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (fd), buffer_str);
	    }
	  {
	    long rwpos;
	    rwpos = read_write_pos - bufbase;
	    answer = scm_make_shared_substring (buffer_str,
						scm_long2num (rwpos),
						SCM_MAKINUM (rwpos + (buffered > 0 ? buffered : 0)));
	  }
	}
    }
  SCM_ALLOW_INTS;
  return answer;
}



/************************************************************************
 *(h1 "String Ports")
 * 
 * String ports are pseudo-descriptors which read from or write to
 * a string instead of a file.
 * 
 */


/*(c %make-string-port)
 * (%make-string-port flags :optional init)
 * 
 * Return a pseudo-descriptor suitable for output.
 * Output to this descriptor is available as a string.
 * See xref:"string-port->string".
 * 
 * For more information about the operation of the buffer system,
 * see \libhackerlab/, the Hackerlab C library reference manual.
 */
SCM_PROC (s_sys_make_string_port, "%make-string-port", 1, 1, 0, scm_sys_make_string_port);
SCM
scm_sys_make_string_port (SCM sflags, SCM init)
{
  t_uchar * str;
  long len;
  int flags;
  int fd;
  int errn;
  int has_init;
  SCM answer;

  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg1, s_sys_make_string_port);

  if ((init == SCM_UNDEFINED) || (init == SCM_BOOL_F))
    {
      has_init = 0;
      str = "";
      len = 0;
    }
  else
    {
      SCM_ASSERT (scm_is_ro_string (init),
		  init, scm_arg2, s_sys_make_string_port);

      has_init = 1;
      str = SCM_RO_UCHARS (init);
      len = SCM_RO_LENGTH (init);
    }
  
  SCM_DEFER_INTS;
  fd = vu_make_virtual_null_fd (&errn, flags);
  if (   (fd < 0)
      || (0 > vfdbuf_buffer_fd (&errn, fd, len, flags, vfdbuf_add_zero_byte | vfdbuf_auto_shift))
      || (0 > vfdbuf_set_dont_flush (&errn, fd, 1))
      || (   has_init
	  && (0 > vfdbuf_set_buffer (&errn, fd, str, len, 0, len, 0, free_buffer, (void *)fd))))
    {
      answer = scm_makerrno (errn);
    }
  else
    {
      if (has_init)
	scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (fd), init);
      answer = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
    }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c string-port->string)
 * (string-port->string port)
 * 
 * Return the string associated with a string port.
 * 
 * See xref:"%make-string-port".
 */
SCM_PROC (s_string_port_to_string, "string-port->string", 1, 0, 0, scm_string_port_to_string);
SCM
scm_string_port_to_string (SCM port)
{
  SCM strings;
  strings = scm_sys_vfdbuf_get_buffered (port);
  if (scm_ilength (strings) >= 2)
    return SCM_CADR (strings);
  else
    return strings;
}


/*(c %write->string)
 * (%write->string obj :rest kws)
 * 
 * Return a string which contains the output of `write' for
 * `obj'.  `kws' is as to the procedure `write'.
 * 
 * See xref:"write".
 */
SCM_PROC (s_sys_write_to_string, "%write->string", 1, 0, 1, scm_sys_write_to_string);
SCM
scm_sys_write_to_string (SCM obj, SCM kws)
{
  SCM_INTS_ENABLED;
  SCM port;
  SCM answer;

  port = scm_sys_make_string_port (SCM_MAKINUM (O_WRONLY), SCM_UNDEFINED);
  if (!SCM_IS_IMMEDIATE (port) && SCM_ERRNOP (port))
    return port;
  scm_write (obj, port, kws);
  answer = scm_string_port_to_string (port);
  scm_close_port (port);
  return answer;
}




void
scm_init_vuprocs (void)
{
#include "systas/libsystas/vuprocs.x"

  scm_declare_integer_cpp_constant (s_vfdbuf_buffer_flag, "vfdbuf_auto_shift", SCM_MAKINUM (vfdbuf_auto_shift));
}

