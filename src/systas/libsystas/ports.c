/* tag: Tom Lord Tue Dec  4 14:41:54 2001 (ports.c)
 */
/* ports.c -
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stddef.h>
#include <errno.h>
#include <setjmp.h>
#include "hackerlab/mem/mem.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vfdbuf.h"
#include "hackerlab/char/str.h"
#include "hackerlab/arrays/ar.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/vuprocs.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/ports.h"




SCM_SYMBOL (s_open_flag, "open-flag");

SCM_PROCEDURE (proc_open_file, "open-file");
SCM_PROCEDURE (proc_read_char, "read-char");
SCM_PROCEDURE (proc_peek_char, "peek-char");
SCM_PROCEDURE (proc_force_output, "force-output");
SCM_PROCEDURE (proc_close_port, "close-port");


/************************************************************************
 *(h0 "Ports")
 * 
 * Ports are objects, such as open files, from which characters can be
 * read or written in sequence.  Ports are distinguished by whether
 * they are for reading, writing, or both.
 * 
 * In Systas Scheme, ports are indistinguishable from file descriptor
 * objects.  (See *xref*).
 * 
 * 
 * These procedures (defined in `scheme-library/ice-9/basic.scm')
 * still need documentation
 * 
 *     call-with-input-string call-with-output-string open-input-file
 *     open-output-file open-io-file close-input-port
 *     close-output-port close-io-port call-with-input-file
 *     call-with-output-file with-input-from-port with-output-to-port
 *     with-error-to-port with-input-from-file with-output-to-file
 *     with-error-to-file with-input-from-string with-output-to-string
 *     with-error-to-string ->string display->string the-eof-object
 * 
 */

/*(menu)
 */


/*(c port?)
 * (port? obj)
 * 
 * Return `#t' if `obj' is a port, `#f' otherwise.
 */
SCM_PROC (s_port_p, "port?", 1, 0, 0, scm_port_p);
SCM
scm_port_p (SCM x)
{
  return scm_int_to_bool (scm_is_port (x));
}

/*(c input-port?)
 * (input-port? x)
 * 
 * Return `#t' if `obj' is an input port, `#f' otherwise.
 */
SCM_PROC (s_input_port_p, "input-port?", 1, 0, 0, scm_input_port_p);
SCM
scm_input_port_p (SCM x)
{
  int fd;
  char c;
  int errn;
  SCM answer;

  if (!SCM_INUMP (x) && !scm_is_port (x))
    return SCM_BOOL_F;

  SCM_DEFER_INTS;
  if (SCM_INUMP (x))
    fd = SCM_INUM (x);
  else
    fd = SCM_FD (x);
 retry:
  if (0 == vu_read (&errn, fd, &c, 0))
    answer = SCM_BOOL_T;
  else if (errn == EAGAIN) 
    answer = SCM_BOOL_T;
  else if (errn == EINTR)
    goto retry;
  else
    answer = SCM_BOOL_F;
  SCM_ALLOW_INTS;

  return answer;
}


/*(c output-port?)
 * (output-port? x)
 * 
 * Return `#t' if `obj' is an output port, `#f' otherwise.
 */
SCM_PROC (s_output_port_p, "output-port?", 1, 0, 0, scm_output_port_p);
SCM
scm_output_port_p (SCM x)
{
  int fd;
  char c;
  int errn;
  SCM answer;

  if (!SCM_INUMP (x) && !scm_is_port (x))
    return SCM_BOOL_F;

  SCM_DEFER_INTS;
  if (SCM_INUMP (x))
    fd = SCM_INUM (x);
  else
    fd = SCM_FD (x);

 retry:
  if (0 == vu_write (&errn, fd, &c, 0))
    answer = SCM_BOOL_T;
  else if ((errn == EAGAIN) || (errn == EPIPE) || (errn == ENOSPC))
    answer = SCM_BOOL_T;
  else if (errn == EINTR)
    goto retry;
  else
    answer = SCM_BOOL_F;
  SCM_ALLOW_INTS;
  return answer;
}


/************************************************************************
 *(h1 "The Current Ports")
 * 
 * Three dynamic values, the current input, output, and error port,
 * provide the default ports for input operations, output operations,
 * and error messages.
 * 
 * The "current load port" is the port currently opened for loading a
 * file (or `#f' if no file is being loaded).
 */

/*(c current-input-port)
 * (current-input-portvoid)
 * 
 * Return the current input port.
 */
SCM_PROC (s_current_input_port, "current-input-port", 0, 0, 0, scm_current_input_port);
SCM
scm_current_input_port (void)
{
  SCM_INTS_INDIFFERENT;

  return scm_cur_inp;
}


/*(c current-output-port)
 * (current-output-portvoid)
 * 
 * Return the current output port.
 */
SCM_PROC (s_current_output_port, "current-output-port", 0, 0, 0, scm_current_output_port);
SCM
scm_current_output_port (void)
{
  SCM_INTS_INDIFFERENT;

  return scm_cur_outp;
}


/*(c current-error-port)
 * (current-error-portvoid)
 * 
 * Return the current error port.
 */
SCM_PROC (s_current_error_port, "current-error-port", 0, 0, 0, scm_current_error_port);
SCM
scm_current_error_port (void)
{
  SCM_INTS_INDIFFERENT;

  return scm_cur_errp;
}


/*(c current-load-port)
 * (current-load-portvoid)
 * 
 * Return the current load port.
 */
SCM_PROC (s_current_load_port, "current-load-port", 0, 0, 0, scm_current_load_port);
SCM
scm_current_load_port (void)
{
  SCM_INTS_INDIFFERENT;

  return scm_cur_loadp;
}


/*(c set-current-input-port)
 * (set-current-input-port port)
 * 
 * Make `port' the current input port.
 */
SCM_PROC (s_set_current_input_port, "set-current-input-port", 1, 0, 0, scm_set_current_input_port);
SCM
scm_set_current_input_port (SCM port)
{
  SCM_INTS_ENABLED;
  SCM oinp;

  oinp = scm_cur_inp;

  SCM_ASSERT (scm_is_port (port), port, scm_arg1, s_set_current_input_port);
  scm_cur_inp = port;
  return oinp;
}


/*(c set-current-output-port)
 * (set-current-output-port port)
 * 
 * Make `port' the current output port.
 */
SCM_PROC (s_set_current_output_port, "set-current-output-port", 1, 0, 0, scm_set_current_output_port);
SCM
scm_set_current_output_port (SCM port)
{
  SCM_INTS_ENABLED;
  SCM ooutp;

  ooutp = scm_cur_outp;

  SCM_ASSERT (scm_is_port (port), port, scm_arg1, s_set_current_output_port);
  scm_cur_outp = port;
  return ooutp;
}


/*(c set-current-error-port)
 * (set-current-error-port port)
 * 
 * Make `port' the current error port.
 */
SCM_PROC (s_set_current_error_port, "set-current-error-port", 1, 0, 0, scm_set_current_error_port);
SCM
scm_set_current_error_port (SCM port)
{
  SCM_INTS_ENABLED;
  SCM oerrp;

  oerrp = scm_cur_errp;

  SCM_ASSERT (scm_is_port (port), port, scm_arg1, s_set_current_error_port);
  scm_cur_errp = port;
  return oerrp;
}


/************************************************************************
 *(h1 "Opening Files and Closing Ports")
 * 
 */

/*(c open-file)
 * (open-file name flags mode)
 * 
 * Open the file `name' with `flags' and `mode' as to `%open' (*xref).
 * Return a port for the newly opened file.
 * 
 * The port returned is buffered.  To open an unbuffered port, see
 * `%open'.
 * 
 * If the file can not be opened, an exception is signaled.
 */
SCM_PROC (s_open_file, "open-file", 3, 0, 0, scm_open_file);
SCM
scm_open_file (SCM name, SCM flags, SCM mode)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = scm_sys_open_buffered (name, flags, mode);
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_open_file, name, flags, mode, SCM_UNDEFINED));
  return answer;
}


/*(c %open-buffered)
 * (%open-buffered name sflags mode)
 * 
 * Open the file `name' with `flags' and `mode' as to `%open' (*xref).
 * Return a port for the newly opened file.
 * 
 * The port returned is buffered.  To open an unbuffered port, see
 * `%open'.
 * 
 * If the file can not be opened, an errno object is returned. (*xref*).
 */
SCM_PROC (s_sys_open_buffered, "%open-buffered", 3, 0, 0, scm_sys_open_buffered);
SCM
scm_sys_open_buffered (SCM name, SCM sflags, SCM mode)
{
  SCM_INTS_ENABLED;
  int flags;
  int errn;
  SCM answer;

  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg2, s_sys_open_buffered);
  answer = scm_sys_open (name, sflags, mode);
  if (!SCM_IS_IMMEDIATE (answer) && SCM_FDP (answer))
    {
      int fd;
      SCM_DEFER_INTS;
      fd = SCM_FD (answer);
      if (0 > vfdbuf_buffer_fd (&errn, fd, 0, flags, vfdbuf_add_zero_byte | vfdbuf_auto_shift))
	answer = scm_makerrno (errn);
      SCM_ALLOW_INTS;
    }
  return answer;
}



/*(c close-port)
 * (close-port port)
 * 
 * Close the indicated port.  If an error occurs, an exception is signaled.
 */
SCM_PROC (s_close_port, "close-port", 0, 1, 0, scm_close_port);
SCM
scm_close_port (SCM port)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = scm_sys_close (port);
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_close_port, port, SCM_UNDEFINED));
  return answer;
}




/*(c eof-object?)
 * (eof-object? obj)
 * 
 * Return `#t' if `obj' is the end-of-file indicator.
 */
SCM_PROC (s_eof_object_p, "eof-object?", 1, 0, 0, scm_eof_object_p);
SCM
scm_eof_object_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return (SCM_EOF_VAL == x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c read-char)
 * (read-char :optional port)
 * 
 * Read one character from `port' or the current input port.
 */
SCM_PROC (s_read_char, "read-char", 0, 1, 0, scm_read_char);
SCM
scm_read_char (SCM port)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int got;
  char c;
  SCM answer;

  if ((port == SCM_UNDEFINED) || (port == SCM_BOOL_F))
    port = scm_cur_inp;

  SCM_ASSERT (SCM_INUMP (port) || scm_is_port (port), port, scm_arg1, s_read_char);
  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else
    fd = SCM_FD (port);
  got = vu_read (&errn, fd, &c, 1);
  if (got == 1)
    answer = scm_int_to_char (c);
  else if (got == 0)
    answer = SCM_EOF_VAL;
  else
    answer = scm_makerrno (errn);
  SCM_ALLOW_INTS;
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_read_char, port, SCM_UNDEFINED));
  return answer;
}


/*(c force-output)
 * (force-output port)
 * 
 * Flush buffered output from `port' or the current output port.
 */
SCM_PROC (s_force_output, "force-output", 0, 1, 0, scm_force_output);
SCM
scm_force_output (SCM port)
{
  int fd;
  int errn;
  int got;
  SCM answer;

  if ((port == SCM_UNDEFINED) || (port == SCM_BOOL_F))
    port = scm_cur_outp;
  SCM_ASSERT (SCM_INUMP (port) || scm_is_port (port), port, scm_arg1, s_force_output);

  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else
    fd = SCM_FD (port);
  if (!vfdbuf_is_buffered (fd))
    got = 0;
  else
    got = vfdbuf_flush (&errn, fd);
  if (got < 0)
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_force_output, port, SCM_UNDEFINED));
  return answer;
}


/*(c peek-char)
 * (peek-char port)
 * 
 * Return the next character from `port' without consuming that
 * character from the input SCM_STREAM.  `port' must be a buffered
 * input port.
 */
SCM_PROC (s_peek_char, "peek-char", 0, 1, 0, scm_peek_char);
SCM
scm_peek_char (SCM port)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int got;
  t_uchar * buffer;
  long buffered;
  SCM answer;

  if ((port == SCM_UNDEFINED) || (port == SCM_BOOL_F))
    port = scm_cur_inp;
  SCM_ASSERT (SCM_INUMP (port) || scm_is_port (port), port, scm_arg1, s_close_port);


  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else
    fd = SCM_FD (port);
  if (0 > vfdbuf_getbuf (&errn, 0, 0, &buffer, &buffered, 0, fd))
    answer = scm_makerrno (errn);
  else
    {
      if (buffered > 0)
	answer = scm_int_to_char (buffer[0]);
      else
	{
	  got = vfdbuf_more (&errn, 0, 0, &buffer, &buffered, 0, fd, 0);
	  if (got < 0)
	    answer = scm_makerrno (errn);
	  else if (got == 0)
	    answer = SCM_EOF_VAL;
	  else
	    answer = scm_int_to_char (buffer[0]);
	}
    }
  SCM_ALLOW_INTS;
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_listify (proc_peek_char, port, SCM_UNDEFINED));
  return answer;
}


/*(c char-ready?)
 * (char-ready? port)
 * 
 * Return `#t' if there is input ready on `port', `#f' otherwise.
 * 
 * \BROKEN/ -- the current implementation always returns `#t'.
 */
SCM_PROC (s_char_ready_p, "char-ready?", 0, 1, 0, scm_char_ready_p);
SCM
scm_char_ready_p (SCM port)
{
  int fd;
  SCM answer;

  if ((port == SCM_UNDEFINED) || (port == SCM_BOOL_F))
    port = scm_cur_outp;
  SCM_ASSERT (SCM_INUMP (port) || scm_is_port (port), port, scm_arg1, s_close_port);

  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else
    fd = SCM_FD (port);
  answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}



/*c scm_is_port)
 * int scm_is_port (SCM obj);
 * 
 * Return 1 if `obj' is a port, 0 otherwise.
 */
int
scm_is_port (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && SCM_FDP (obj);
}



/*c scm_port_putc)
 * int scm_port_putc (int * errn, SCM port, int c);
 * 
 * Write one character on `port'.  
 * 
 * Return 0 on success, -1 and an errno value otherwise.
 */
int
scm_port_putc (int * errn, SCM port, int c)
{
  SCM_INTS_DISABLED;
  int fd;
  int got;
  t_uchar b;

  fd = SCM_FD (port);
  b = c;
 retry:
  got = vu_write (errn, fd, &b, 1);
  if (got < 0)
    return -1;
  else if (got == 0)
    goto retry;
  else
    return c;
}


/*c scm_port_getc)
 * int scm_port_getc (int * errn, SCM port);
 * 
 * Read one character from `port' and return that character.
 * 
 * Return the character on success, -1 and an errno value otherwise.
 */
int
scm_port_getc (int * errn, SCM port)
{
  SCM_INTS_DISABLED;
  int fd;
  int got;
  t_uchar b;

  fd = SCM_FD (port);
  got = vu_read (errn, fd, &b, 1);
  if (got < 0)
    return -1;
  else if (got == 0)
    {
      *errn = 0;
      return -1;
    }
  else
    return b;
}


/*c scm_port_ungetc)
 * int scm_port_ungetc (int * errn, SCM port, int c);
 * 
 * Return the character `c' to `port' (which must be a buffered input
 * port) so that it is the next character read.
 * 
 * Return `c' on success, -1 and an errno value otherwise. 
 */
int
scm_port_ungetc (int * errn, SCM port, int c)
{
  SCM_INTS_DISABLED;
  int fd;
  t_uchar b;

  fd = SCM_FD (port);
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }
  b = c;
  if (0 > vfdbuf_return (errn, fd, &b, 1))
    return -1;
  return c;
}


/*c scm_port_write)
 * long scm_port_write (int * errn, SCM port, t_uchar * buf, long len);
 * 
 * Write `len' characters from `buf' on `port'.
 * 
 * Return the number of characters written on success, -1 and an errno
 * value otherwise.
 */
long
scm_port_write (int * errn, SCM port, t_uchar * buf, long len)
{
  SCM_INTS_DISABLED;
  int fd;
  int got;
  int so_far;

  fd = SCM_FD (port);
  so_far = 0;
 retry:
  got = vu_write (errn, fd, buf, len);
  if (got < 0)
    {
      if (*errn == EINTR)
	goto retry;
      return -1;
    }
  else if (got < len)
    {
      so_far += got;
      buf += got;
      len -= got;
      goto retry;
    }
  else
    return so_far + got;
}


/*c scm_port_puts)
 * long scm_port_puts (int * errn, SCM port, t_uchar * str);
 * 
 * Write the `0'-terminated string `str' on `port' (not including the
 * final 0).
 * 
 * Return the number of characters written on success, -1 and an errno
 */
long
scm_port_puts (int * errn, SCM port, t_uchar * str)
{
  SCM_INTS_DISABLED;

  return scm_port_write (errn, port, str, str_length (str));
}


/*c scm_port_flush)
 * int scm_port_flush (int * errn, SCM port);
 * 
 * Flush buffered output from `port'.
 * 
 * Return 0 on success, -1 and an errno value otherwise.
 */
int
scm_port_flush (int * errn, SCM port)
{
  SCM_INTS_DISABLED;
  int fd;
  int errnx;

  /* Allowing 0 for `errn' makes it cleaner to call this function from
   * gdb.
   */
  if (!errn)
    errn = &errnx;

  fd = SCM_FD (port);
  return vu_fsync (errn, fd);
}




void
scm_init_ports (void)
{
#include "systas/libsystas/ports.x"
}
