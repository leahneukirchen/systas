/* tag: Tom Lord Tue Dec  4 14:41:40 2001 (safe-vfdbuf.c)
 */
/* safe-vfdbuf.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/safe-vfdbuf.h"




void
safe_buffer_fd (int fd, long bufsize, int flags, int buffer_flags)
{
  int errn;
  int status;

  status = vfdbuf_buffer_fd (&errn, fd, bufsize, flags, buffer_flags);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_buffer_fd' (%d, %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_set_buffer (int fd,
		 t_uchar * buffer,
		 long bufsize,
		 long read_write_pos,
		 long buffered,
		 int add_zero_byte,
		 void (*free_buffer)(t_uchar * buf, void * closure),
		 void * free_buffer_closure)
{
  int errn;
  int status;

  status = vfdbuf_set_buffer (&errn, fd, buffer, bufsize, read_write_pos, buffered, add_zero_byte, free_buffer, free_buffer_closure);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_set_buffer' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_set_dont_flush (int fd, int setting)
{
  int errn;
  int status;

  status = vfdbuf_set_dont_flush (&errn, fd, setting);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_set_dont_flush' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}

void
safe_unbuffer (int fd)
{
  int errn;
  int status;

  status = vfdbuf_unbuffer_fd (&errn, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_unbuffer_fd' (%d, %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


int
safe_is_dont_flush (int fd)
{
  int errn;
  int status;

  status = vfdbuf_is_dont_flush (&errn, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_is_dont_flush' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }

  return status;
}


void
safe_set_auto_shift (int fd, int setting)
{
  int errn;
  int status;

  status = vfdbuf_set_auto_shift (&errn, fd, setting);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_set_auto_shift' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


int
safe_is_auto_shift (int fd)
{
  int errn;
  int status;

  status = vfdbuf_is_auto_shift (&errn, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_is_auto_shift' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }

  return status;
}


void
safe_getbuf (t_uchar ** buffer,
	     long * bufsize,
	     t_uchar ** read_write_position,
	     long * buffered,
	     int * has_zero_byte,
	     int fd)
{
  int errn;
  int status;

  status = vfdbuf_getbuf (&errn, buffer, bufsize, read_write_position, buffered, has_zero_byte, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_getbuf' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_takebuf (t_uchar ** bufbase,
	      long * bufsize,
	      t_uchar ** read_write_pos,
	      long * buffered,
	      int * has_zero_byte,
	      int fd,
	      void (*free_buffer)(t_uchar * buf, void * closure),
	      void * free_buffer_closure)
{
  int errn;
  int status;

  status = vfdbuf_takebuf (&errn, bufbase, bufsize, read_write_pos, buffered, has_zero_byte, fd, free_buffer, free_buffer_closure);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_takebuf' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_shift (int fd, long amt)
{
  int errn;
  int status;

  status = vfdbuf_shift (&errn, fd, amt);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_shift' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_advance (int fd, long amt)
{
  int errn;
  int status;

  status = vfdbuf_advance (&errn, fd, amt);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_advance' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_flush (int fd)
{
  int errn;
  int status;

  status = vfdbuf_flush (&errn, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_flush' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


void
safe_return (int fd, t_uchar * str, long len)
{
  int errn;
  int status;

  status = vfdbuf_return (&errn, fd, str, len);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_return' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}


ssize_t
safe_more (t_uchar ** buffer,
	   long * bufsize,
	   t_uchar ** read_write_pos,
	   long * buffered,
	   int * has_zero_byte,
	   int fd,
	   long opt_amt)
{
  int errn;
  int status;

  status = vfdbuf_more (&errn, buffer, bufsize, read_write_pos, buffered, has_zero_byte, fd, opt_amt);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_more' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
  return status;
}


int
safe_is_eof (int fd)
{
  int errn;
  int status;

  status = vfdbuf_is_eof (&errn, fd);

  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_is_eof' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }

  return status;
}


void
safe_clear_eof (int fd)
{
  int errn;
  int status;

  status = vfdbuf_clear_eof (&errn, fd);
  if (status < 0)
    {
      printfmt (&errn, 2, "Error during call to `vfdbuf_clear_eof' (%d: %s)\n", errn, errno_to_string (errn));
      panic ("I/O error");
    }
}

