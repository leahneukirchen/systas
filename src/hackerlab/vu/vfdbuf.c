/* vfdbuf.c - VU buffered I/O
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/vu/vfdbuf.h"


/************************************************************************
 *(h0 "Buffered File Descriptors")
 *
 * These functions allow you to perform buffered I/O on descriptors
 * using the VU functions. (See xref:"A Virtual Unix File-System
 * Interface".)
 *
 * Each buffered file is associated with these values:
 *
 * 	t_uchar * buffer;
 *		Storage for buffered characters.
 * 		The location of the buffer in memory
 * 		may change over time.
 * 
 *      long bufsize
 * 		The size of the buffer.
 * 
 *	t_uchar * read_write_position;
 * 		The read/write position, within the buffer.
 * 		This corresponds to the file offset returned
 * 		by:
 * 			vu_lseek (&errn, fd, 0, SEEK_CUR)
 * 
 * 	long buffered;
 * 		The number of buffered characters relative
 * 		to the read/write position.
 * 
 * 		All bytes between `buffer' and 
 * 		`read_write_position + buffered' are
 * 		buffered.
 * 
 * 		Note that `buffered' may be negative if the
 * 		`read_write_position' is beyond the current
 * 		end of the file.
 * 
 * Normal I/O functions like `vu_read' and `vu_write' operate
 * in the expected way: they buffer characters or read characters
 * from the buffer, flushing or refilling it as needed.
 * 
 * For high-performance I/O, there is assistance for performing
 * buffered I/O without copying characters to and from the buffer:
 * 
 * 		vfdbuf_getbuf
 * 		vfdbuf_takebuf
 * 		vfdbuf_set_buffer
 * 		vfdbuf_more
 * 		vfdbuf_advance
 * 
 * which are documented later in this chapter.
 * 
 * Buffers can be designated `dont_flush' -- which means that the buffer
 * grows rather than writing to an underlying file.  In combination with
 * pseudo-descriptors (see xref:"reserv_pseudo"), `dont_flush' buffers
 * can be used to implement in-core files (files which are actually 
 * strings).
 * 
 * Buffers can be designated `zero_byte' -- which means that an extra
 * byte is added to the buffer (at `buffer + bufsize') and that byte
 * is initialized to 0.
 */

/*(menu)
 */



struct vfdbuf_record
{
  struct vu_handler sub_handler;
  int flags;
  int eof;

  void (*free_buffer)(t_uchar * buffer, void * closure);
  void * free_buffer_closure;

  int dont_flush;
  int is_pipe;
  int zero_byte;
  int auto_shift;
  off_t fd_offset;		/* the kernel's read/write position */
  off_t buffer_offset;		/* position in the file of the buffer */
  long read_write_pos;		/* offset within the buffer */
  long buffered;		/* number of valid characters in buffer */
  long bufsize;			/* total size of buffer */
  t_uchar * buf;		/* the buffer */
};

static struct vfdbuf_record * bufs = 0;

#if 1
#define check_buffer_invariants(FD) \
	check_buffer_invariants_internal (fd, __FILE__, __LINE__)
#else
#define check_buffer_invariants(FD) 
#undef invariant
#define invariant(COND)
#endif

static void
check_buffer_invariants_internal (int fd, char * file, int line)
{
  invariant_at_file_line (vfdbuf_is_buffered (fd), file, line);

  /* temporary invariant since RDWR mode doesn't really work
   */
  invariant_at_file_line ((bufs[fd].flags & O_ACCMODE) != O_RDWR, file, line);

  invariant_at_file_line (bufs[fd].buffered >= 0, file, line);
  invariant_at_file_line (bufs[fd].buffered <= bufs[fd].bufsize, file, line);
  invariant_at_file_line (bufs[fd].read_write_pos >= 0, file, line);
  invariant_at_file_line (bufs[fd].read_write_pos <= bufs[fd].bufsize, file, line);
  invariant_at_file_line ((!bufs[fd].bufsize && !bufs[fd].zero_byte) || bufs[fd].buf, file, line);


  /* invariant_at_file_line (bufs[fd].is_pipe || ((bufs[fd].flags & O_ACCMODE) != O_RDWR), file, line); */

  /* Note that the RDWR case doesn't really work.
   */
  invariant_at_file_line ((   !bufs[fd].is_pipe
			   || (   (((bufs[fd].flags & O_ACCMODE) == O_RDONLY) && (bufs[fd].fd_offset == (bufs[fd].buffer_offset + bufs[fd].buffered)))
			       || (((bufs[fd].flags & O_ACCMODE) == O_WRONLY) && (bufs[fd].fd_offset == bufs[fd].buffer_offset))
			       || (((bufs[fd].flags & O_ACCMODE) == O_RDWR) && 0))),
			  file, line);
}




/* static void realloc_buf (int fd, long bufsize);
 * 
 * Make a buffer larger.  (`bufsize' must be >= than
 * `bufs[fd].bufsize'.)
 */
static void
realloc_buf (int fd, long bufsize)
{
  long size;
  t_uchar * oldbuf;
  t_uchar * newbuf;

  invariant (bufsize >= bufs[fd].bufsize);
  check_buffer_invariants (fd);

  oldbuf = bufs[fd].buf;
  size = bufsize + (bufs[fd].zero_byte ? 1 : 0);
  newbuf = (t_uchar *)must_malloc (size);
  mem_move (newbuf, oldbuf, bufs[fd].buffered);
  bufs[fd].buf = newbuf;
  bufs[fd].bufsize = bufsize; 

  if (bufs[fd].free_buffer)
    {
      bufs[fd].free_buffer (oldbuf, bufs[fd].free_buffer_closure);
      bufs[fd].free_buffer = 0;
    }
  else
    {
      must_free (oldbuf);
    }

  if (bufs[fd].zero_byte)
    bufs[fd].buf[size - 1] = 0;

  invariant (bufs[fd].buf != oldbuf);
  check_buffer_invariants (fd);
}


void
vfdbuf_flush_all(void)
{
  int x;
  int size;

  size = ar_size ((void *)bufs, lim_use_must_malloc, sizeof (*bufs));
  for (x = 0; x < size; ++x)
    {
      if (bufs[x].sub_handler.vtable)
	{
	  int errn;
	  vfdbuf_flush (&errn, x);
	}
    }
}

/************************************************************************
 *(h1 "Establishing Buffers")
 * 
 */

/*(c vfdbuf_buffer_fd)
 * int vfdbuf_buffer_fd (int * errn,
 *			 int fd,
 *			 long bufsize,
 *			 int flags,
 *			 int buffer_flags);
 * 
 * Arrange to buffer I/O on descriptor `fd'.
 *
 * `fd' is the descriptor to buffer.
 *
 * `bufsize' is the initial size of the buffer or 0 to use a buffer of
 * the default size.
 * 
 * `flags' indicates whether to buffer for input (`O_RDONLY'),
 * output (`O_WRONLY') or both (`O_RDWR').
 * 
 * \WARNING:/ I am fairly sure that there are bugs in the 
 * buffering code, especially for `O_RDWR' files.  Some 
 * comprehensive unit tests are needed for this module.
 * 
 * `buffer_flags' is a combination (`|') of any of the bits:
 *
 *	vfdbuf_add_zero_byte
 *	vfdbuf_auto_shift
 *
 * If `vfdbuf_add_zero_byte' is set, the actual buffer size for this
 * descriptor is one byte larger than `bufsize' (even if the buffer is
 * reallocated).  The extra byte is initialized to 0.
 *
 * If `vfdbuf_auto_shift' is set, the behavior of `vfdbuf_more' is
 * modified.  See xref:"vfdbuf_more".
 *
 * If `fd' is already buffered, this function attempts to reset the
 * buffer size, flags, and buffer flags.  If more than `bufsize'
 * characters are already buffered, the resulting buffer will be
 * larger than `bufsize'.
 *
 * Return 0 on success, -1 (and set `*errn') on error.
 */
int
vfdbuf_buffer_fd (int * errn, int fd, long bufsize, int flags, int buffer_flags)
{
  off_t offset;
  int is_pipe;
  struct vu_handler * sub_handler;

  if (bufsize <= 0)
    bufsize = 4096;

  if ((ar_size (bufs, lim_use_must_malloc, sizeof (*bufs)) > fd) && bufs[fd].sub_handler.vtable)
    {
      int had_zero_byte;

      /* already buffered */

      if (bufs[fd].flags != flags)
	{
	  *errn = EINVAL;
	  return -1;
	}

      had_zero_byte = bufs[fd].zero_byte;
      
      bufs[fd].auto_shift = !!(buffer_flags & vfdbuf_auto_shift);
      bufs[fd].zero_byte = !!(buffer_flags & vfdbuf_add_zero_byte);

      if (bufsize != bufs[fd].bufsize)
	{
	  t_uchar * oldbuf;
	  t_uchar * newbuf;

	  if (bufsize < bufs[fd].buffered)
	    bufsize = bufs[fd].buffered;

	  newbuf = must_malloc (bufsize + bufs[fd].zero_byte);
	  oldbuf = bufs[fd].buf;
	  memcpy (newbuf, oldbuf, bufs[fd].buffered);

	  if (bufs[fd].free_buffer)
	    bufs[fd].free_buffer (oldbuf, bufs[fd].free_buffer_closure);
	  else
	    must_free (oldbuf);

	  bufs[fd].buf = newbuf;

	  if (bufs[fd].zero_byte)
	    bufs[fd].buf[bufsize] = 0;
	}

      check_buffer_invariants (fd);
      return 0;
    }

  sub_handler = vu_fd_dispatch (fd);

  offset = sub_handler->vtable->lseek (errn, fd, 0, SEEK_CUR, sub_handler->closure);

  if (offset >= 0)
    is_pipe = 0;
  else
    {
      if ((*errn != ESPIPE) || ((flags & O_ACCMODE) == O_RDWR))
	return -1;
      else
	{
	  is_pipe = 1;
	  offset = 0;
	}
    }

  if (fd >= ar_size (bufs, lim_use_must_malloc, sizeof (*bufs)))
    ar_ref ((void **)&bufs, lim_use_must_malloc, fd, sizeof (struct vfdbuf_record));

  bufs[fd].sub_handler = *sub_handler;
  bufs[fd].flags = O_ACCMODE & flags;
  bufs[fd].eof = 0;
  bufs[fd].free_buffer = 0;
  bufs[fd].free_buffer_closure = 0;
  bufs[fd].dont_flush = 0;
  bufs[fd].is_pipe = is_pipe;
  bufs[fd].zero_byte = !!(buffer_flags & vfdbuf_add_zero_byte);
  bufs[fd].auto_shift = !!(buffer_flags & vfdbuf_auto_shift);
  bufs[fd].fd_offset = 0;
  bufs[fd].buffer_offset = offset;
  bufs[fd].read_write_pos = 0;
  bufs[fd].buffered = 0;
  bufs[fd].buf = must_malloc (bufsize + bufs[fd].zero_byte);
  bufs[fd].bufsize = bufsize;
  vu_set_fd_handler (fd, &vfdbuf_vtable, 0);
  check_buffer_invariants (fd);
  return 0;
}


/*(c vfdbuf_set_buffer)
 * int
 * vfdbuf_set_buffer (int * errn,
 * 		      int fd,
 * 		      t_uchar * buffer,
 * 		      long bufsize,
 * 		      long read_write_pos,
 * 		      long buffered,
 * 		      int add_zero_byte,
 *		      void (*free_buffer)(t_uchar * buf, void * closure),
 *		      void * free_buffer_closure)
 * 
 * Establish a specific region of memory as the buffer for `fd'.
 * 
 * See xref:"Buffered File Descriptors" and xref:"vfdbuf_buffer_fd".
 */
int
vfdbuf_set_buffer (int * errn,
		   int fd,
		   t_uchar * buffer,
		   long bufsize,
		   long read_write_pos,
		   long buffered,
		   int add_zero_byte,
		   void (*free_buffer)(t_uchar * buf, void * closure),
		   void * free_buffer_closure)
{
  if (!((ar_size (bufs, lim_use_must_malloc, sizeof (*bufs)) > fd) && bufs[fd].sub_handler.vtable))
    {
      /* not buffered */
    invalid:
      *errn = EINVAL;
      return -1;
    }

  if ((buffered > bufsize) || (read_write_pos > bufsize))
    goto invalid;

  if (bufs[fd].free_buffer)
    bufs[fd].free_buffer (bufs[fd].buf, bufs[fd].free_buffer_closure);

  bufs[fd].buf = buffer;
  bufs[fd].bufsize = bufsize;
  bufs[fd].read_write_pos = read_write_pos;
  bufs[fd].buffered = buffered;
  bufs[fd].zero_byte = !!add_zero_byte;
  bufs[fd].free_buffer = free_buffer;
  bufs[fd].free_buffer_closure = free_buffer_closure;
  check_buffer_invariants (fd);
  return 0;
}

int
vfdbuf_unbuffer_fd (int * errn, int fd)
{
  check_buffer_invariants (fd);
  if (!bufs[fd].dont_flush && (bufs[fd].flags != O_RDONLY) && bufs[fd].buffered && bufs[fd].read_write_pos)
    {
      int flush;

      flush = vfdbuf_flush (errn, fd);
      if (flush < 0)
	return -1;
    }

  vu_set_fd_handler (fd, bufs[fd].sub_handler.vtable, bufs[fd].sub_handler.closure);

  if (!bufs[fd].free_buffer)
    must_free (bufs[fd].buf);
  else
    bufs[fd].free_buffer (bufs[fd].buf, bufs[fd].free_buffer_closure);

  mem_set ((t_uchar *)&bufs[fd], 0, sizeof (bufs[fd]));

  return 0;
}


/*(c vfdbuf_is_buffered)
 * int vfdbuf_is_buffered (int fd);
 * 
 * Return 1 if fd has a (vfdbuf) buffer, 0 otherwise.
 */
int
vfdbuf_is_buffered (int fd)
{
  if ((ar_size (bufs, lim_use_must_malloc, sizeof (*bufs)) > fd) && bufs[fd].sub_handler.vtable)
    return 1;
  return 0;
}


/************************************************************************
 *(h1 "Controlling Buffer Flushing")
 * 
 * 
 * 
 */

/*(c vfdbuf_set_dont_flush)
 * int vfdbuf_set_dont_flush (int * errn, int fd, int setting);
 * 
 * Activate or deactivate buffer flushing for descriptor `fd'.
 * 
 * `setting' is 0 to deactivate and any other value to activate
 * flushing.
 *
 * If flushing is deactivated, writes may cause the size of the buffer
 * to be increased and calls to `vfdbuf_flush' have no effect.
 *
 * Return -1 and EINVAL if `fd' is not buffered, 0 otherwise.
 * 
 * See also xref:"vfdbuf_is_dont_flush".
 */
int
vfdbuf_set_dont_flush (int * errn, int fd, int setting)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  bufs[fd].dont_flush = !!setting;
  check_buffer_invariants (fd);
  return 0;
}


/*(c vfdbuf_is_dont_flush)
 * int vfdbuf_is_dont_flush (int * errn, int fd);
 * 
 * Return 0 if fd is buffered and flushing is enabled,
 * a positive value if fd is buffered and flushing is disabled.
 *
 * Return -1 and EINVAL if fd is not buffered.
 */
int
vfdbuf_is_dont_flush (int * errn, int fd)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  return bufs[fd].dont_flush;
}



/************************************************************************
 *(h1 "Controlling Buffer Auto-shift")
 * 
 */

/*(c vfdbuf_set_auto_shift)
 * int vfdbuf_set_auto_shift (int * errn, int fd, int setting);
 * 
 * Activate or deactivate buffer auto-shift for descriptor `fd'.
 * 
 * `setting' is 0 to deactivate and any other value to activate
 * auto-shift.
 *
 * See xref:"vfdbuf_more".
 *
 * Return -1 and EINVAL if `fd' is not buffered, 0 otherwise.
 * 
 * See also xref:"vfdbuf_is_auto_shift".
 */
int
vfdbuf_set_auto_shift (int * errn, int fd, int setting)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  bufs[fd].auto_shift = !!setting;
  check_buffer_invariants (fd);
  return 0;
}


/*(c vfdbuf_is_auto_shift)
 * int vfdbuf_is_auto_shift (int * errn, int fd);
 * 
 * Return 0 if fd is buffered and auto-shift is disabled,
 * a positive value if fd is buffered and auto-shift is enabled.
 *
 * Return -1 and EINVAL if fd is not buffered.
 */
int
vfdbuf_is_auto_shift (int * errn, int fd)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  return bufs[fd].auto_shift;
}




/* static int sync_offset (int * errn, int fd);
 * 
 * Move the descriptor's offset within the file to agree with the
 * buffer's offset within the file.
 */
static int
sync_offset (int * errn, int fd)
{
  check_buffer_invariants (fd);
  invariant (   (   ((bufs[fd].flags & O_ACCMODE) != O_RDONLY)
		 && ((bufs[fd].flags & O_ACCMODE) != O_RDWR))
	     || (   (bufs[fd].buffered == 0)
		 && (bufs[fd].read_write_pos == 0)));
  
  if (bufs[fd].buffer_offset != bufs[fd].fd_offset)
    {
      off_t got;
      got = bufs[fd].sub_handler.vtable->lseek (errn,
						fd,
						bufs[fd].buffer_offset,
						SEEK_SET,
						bufs[fd].sub_handler.closure);

      if (got != bufs[fd].buffer_offset)
	{
	  if (got >= 0)
	    *errn = EIO;
	  return -1;
	}
      bufs[fd].fd_offset = bufs[fd].buffer_offset;
      check_buffer_invariants (fd);
    }
  return 0;
}


/************************************************************************
 *(h1 "Access to Buffers")
 * 
 */

/*(c vfdbuf_getbuf)
 * int vfdbuf_getbuf (int * errn,
 *		      t_uchar ** buffer,
 *		      long * bufsize,
 *		      t_uchar ** read_write_position,
 *		      long * buffered,
 *		      int fd);
 * 
 * Return the address and size of the buffer and the address (within
 * that buffer) and number of buffered characters beyond the
 * read/write position for descriptor `fd'.
 *
 * Upon return, `*buf' is the read/write position and `*buffered' is
 * the number of buffered characters beyond that position.  If
 * `*buffered' is less than 0, then the characters between `*buffer'
 * and `*buffer - *buffered' are not yet part of the file (the read/write
 * position is beyond the end of the file).
 *
 * Any of `buffer', `bufsize', `read_write_pos' or `buffered' may be
 * 0.
 *
 * Return -1 and EINVAL if `fd' is not buffered, 0 otherwise.
 */
int
vfdbuf_getbuf (int * errn,
	       t_uchar ** buffer,
	       long * bufsize,
	       t_uchar ** read_write_position,
	       long * buffered,
	       int * has_zero_byte,
	       int fd)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if (buffer)
    *buffer = bufs[fd].buf;
  if (bufsize)
    *bufsize = bufs[fd].bufsize;
  if (read_write_position)
    *read_write_position = bufs[fd].buf + bufs[fd].read_write_pos;
  if (buffered)
    *buffered = bufs[fd].buffered - bufs[fd].read_write_pos;
  if (has_zero_byte)
    *has_zero_byte = bufs[fd].zero_byte;

  invariant (!buffer || (*buffer == bufs[fd].buf));
  invariant (!bufsize || (*bufsize == bufs[fd].bufsize));
  invariant (!read_write_position || ((*read_write_position - bufs[fd].buf) == bufs[fd].read_write_pos));
  invariant (!buffered || ((bufs[fd].read_write_pos + *buffered) == bufs[fd].buffered));
  invariant (!has_zero_byte || (*has_zero_byte == bufs[fd].zero_byte));
  return 0;
}


/*(c vfdbuf_takebuf)
 * int vfdbuf_takebuf (int * errn,
 *		       t_uchar ** bufbase,
 * 		       long * bufsize,
 *		       t_uchar ** read_write_pos,
 *		       long * buffered,
 *		       int fd,
 *		       void (*free_buffer)(t_uchar * buf, void * closure),
 *		       void * free_buffer_closure);
 * 
 * Return the address of and size of the buffer for `fd'.  Also return
 * the address of and number of buffered characters beyond the
 * read/write position.  (See `vfdbuf_getbuf'.)
 *
 * Responsibility for freeing the buffer returned (allocated by
 * `must_malloc') rests with the caller.  `vfdbuf' functions will continue
 * to use the buffer.  When the `vfdbuf' functions no longer require
 * the buffer, they call `free_buffer': 
 *
 *		free_buffer (buffer, free_buffer_closure)
 *
 * Subsequent calls to `vfdbuf_takebuf', made before `free_buffer' is
 * called will return the same buffer if the same values are passed
 * for `free_buffer' and `free_buffer_closure' and a newly allocated
 * buffer otherwise.
 */
int
vfdbuf_takebuf (int * errn,
		t_uchar ** bufbase,
		long * bufsize,
		t_uchar ** read_write_pos,
		long * buffered,
		int * has_zero_byte,
		int fd,
		void (*free_buffer)(t_uchar * buf, void * closure),
		void * free_buffer_closure)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if (   (bufs[fd].free_buffer != free_buffer)
	  || (bufs[fd].free_buffer_closure != free_buffer_closure))
    realloc_buf (fd, bufs[fd].bufsize);
  bufs[fd].free_buffer = free_buffer;
  bufs[fd].free_buffer_closure = free_buffer_closure;
  return vfdbuf_getbuf (errn, bufbase, bufsize, read_write_pos, buffered, has_zero_byte, fd);
}



/************************************************************************
 *(h1 "Controlling What is Buffered")
 * 
 * 
 */

/*(c vfdbuf_shift)
 * int vfdbuf_shift (int * errn, int fd, long amt);
 * 
 * \WARNING: there is a note in the code that says documentation
 * for this function is out of date./
 * 
 * Discard `amt' leading characters from a buffer, shifting any
 * remaining characters to make room at the end of the buffer.
 *
 * If the "don't flush" flag is set for this buffer, this call will
 * increase the size of the buffer by `amt' characters and leave existing
 * characters untouched.
 *
 * If `amt' is larger than the number of buffered characters that
 * preceed the read/write position, or if `amt' is -1, the effect is
 * the same as if `amt' were exactly the number of buffered characters
 * that preceed the read/write position.
 *
 * If the buffer is being used for output, this call will first flush
 * shifted characters that are currently part of the file.  If there
 * are characters in the buffer beyond the end of the file, however, they
 * are not flushed by this function.  The effect is that this function
 * will not make a file larger. (See `vfdbuf_buffer_to_position'.)
 *
 * Return 0 upon success, -1 upon error.  This function can fail for a
 * descriptor opened for writing if it was necessary to flush
 * characters from the buffer, but the flush operation failed.
 *
 * One useful post-condition of this function is that there is room
 * for `amt' more characters beyond the read/write position after a
 * call to this function than were available before.
 * 
 * Another useful post-condition is that the read/write position
 * within the file is not changed by this function. (See
 * `vfdbuf_advance'.)
 */
int
vfdbuf_shift (int * errn, int fd, long amt)
{
  off_t after_short_write;

  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if ((amt > bufs[fd].read_write_pos) || (amt == -1))
    amt = bufs[fd].read_write_pos;


  if (bufs[fd].dont_flush)
    {
      realloc_buf (fd, bufs[fd].bufsize + amt);
      return 0;
    }

  after_short_write = 0;
  
 retry_after_short_write:

  if (((bufs[fd].flags & O_ACCMODE) == O_WRONLY) || ((bufs[fd].flags & O_ACCMODE) == O_RDWR))
    {
      size_t to_write;
      off_t wrote;

      if (sync_offset (errn, fd) < 0)
	return -1;
      to_write = (bufs[fd].buffered < amt) ? bufs[fd].buffered : amt;
      wrote = bufs[fd].sub_handler.vtable->write (errn,
						  fd,
						  bufs[fd].buf,
						  to_write,
						  bufs[fd].sub_handler.closure);
      if (wrote < 0)
	return -1;
      bufs[fd].fd_offset += wrote;
      if (to_write > wrote)
	{
	  after_short_write = amt - wrote;
	  amt = wrote;
	}
    }

  mem_move (bufs[fd].buf, bufs[fd].buf + amt, bufs[fd].buffered - amt);

  bufs[fd].buffer_offset += amt;
  bufs[fd].read_write_pos -= amt;
  bufs[fd].buffered -= amt;

  check_buffer_invariants (fd);

  if (after_short_write)
    {
      amt = after_short_write;
      goto retry_after_short_write;
    }

  return 0;
}


/*(c vfdbuf_advance)
 * int vfdbuf_advance (int * errn, int fd, long amt);
 * 
 * Advance the read/write position of a buffered descriptor.
 *
 * If `amt' is larger than the number of buffered characters beyond
 * the read/write position, the size of the buffer is increased, and
 * the new characters initialized to 0, but no new characters are read
 * from the descriptor.
 *
 * One or more calls this function are commonly followed by a call
 * to `vfdbuf_shift' to discard the advanced-over characters from the 
 * buffer.  A good time to make such a call is before a call to 
 * `vfdbuf_more', if it is safe to discard characters from the buffer
 * at that time.
 *
 * Return -1 and EINVAL if `fd' is not buffered, 0 otherwise.
 */
int
vfdbuf_advance (int * errn, int fd, long amt)
{
  long bufsize_needed;

  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  bufsize_needed = bufs[fd].read_write_pos + amt;
  if (bufsize_needed > bufs[fd].bufsize)
    realloc_buf (fd, bufsize_needed);

  if (bufs[fd].read_write_pos + amt > bufs[fd].buffered)
    {
      long from;
      long fill_to;

      from = ((bufs[fd].read_write_pos > bufs[fd].buffered)
	      ? bufs[fd].read_write_pos
	      : bufs[fd].buffered);
      fill_to = bufs[fd].read_write_pos + amt;
      mem_set (bufs[fd].buf + from, 0, fill_to - from);
    }
  bufs[fd].read_write_pos += amt;
  check_buffer_invariants (fd);
  return 0;
}




/*(c vfdbuf_flush)
 * int vfdbuf_flush (int * errn, int fd);
 * 
 * Write buffered characters prior to the read/write position to disk.
 * This function has no effect if descriptor `fd' is not buffered, or
 * if the "don't flush" flag is set for this descriptor.  (See
 * `vfdbuf_set_dont_flush'.)
 * 
 * Flushed characters are removed from the buffer.
 * 
 * The read/write position may be past the last character of the file.
 * In that case, only the characters in the file are flushed to disk.
 * (See `vfdbuf_buffer_to_position'.)
 *
 * Return 0 upon success, -1 upon error.
 */
int
vfdbuf_flush (int * errn, int fd)
{
  off_t wrote;
  size_t to_write;

  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if (   !bufs[fd].read_write_pos
      || bufs[fd].dont_flush
      || ((bufs[fd].flags != O_WRONLY) && (bufs[fd].flags != O_RDWR)))
    return 0;

  to_write = bufs[fd].read_write_pos;
  if (to_write > bufs[fd].buffered)
    to_write = bufs[fd].buffered;

 retry:
  if (sync_offset (errn, fd) < 0)
    return -1;
  wrote = bufs[fd].sub_handler.vtable->write (errn,
					      fd,
					      bufs[fd].buf,
					      to_write,
					      bufs[fd].sub_handler.closure);
  if (wrote < 0)
    return -1;

  mem_move (bufs[fd].buf, bufs[fd].buf + wrote, bufs[fd].buffered - wrote);
  to_write -= wrote;
  bufs[fd].fd_offset += wrote;
  bufs[fd].buffer_offset += wrote;
  bufs[fd].read_write_pos -= wrote;
  bufs[fd].buffered -= wrote;
  if (to_write)
    goto retry;

  invariant ((bufs[fd].buffered == 0) || (bufs[fd].read_write_pos == 0));

  /* This would discard any characters prior to the read_write_pos
   * that are not already part of the file.  That doesn't seem like
   * the right thing to do, although it would give the invariant that
   * (read_write_pos == 0) on exit.
   *
   * bufs[fd].buffer_offset += bufs[fd].read_write_pos;
   * bufs[fd].read_write_pos = 0;
   */

  check_buffer_invariants (fd);
  return 0;
}


/*(c vfdbuf_return)
 * int vfdbuf_return (int * errn, int fd, t_uchar * str, long len);
 * 
 * Add characters to the buffer for descriptor `fd'.
 *
 * Characters are inserted immediately after the read/write position
 * so that subsequent reads on this descriptor return those characters
 * first and subsequent writes overwrite them.  Subsequent flushes of
 * a writable descriptor will ignore these characters unless the
 * read/write position is first advanced by a write or by
 * `vfdbuf_advance'.
 * 
 * If `fd' is a read-only descriptor, buffered characters prior to 
 * the read/write position may be overwritten.
 *
 * The read/write position of the descriptor is unchanged relative to
 * the beginning of the file by calls to this function.
 */
int
vfdbuf_return (int * errn, int fd, t_uchar * str, long len)
{
  long size_needed;

  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if ((bufs[fd].flags & O_ACCMODE) == O_RDONLY)
    {
      if (bufs[fd].read_write_pos >= len)
	{
	  bufs[fd].read_write_pos -= len;
	  mem_move (bufs[fd].buf + bufs[fd].read_write_pos, str, len);
	  return 0;
	}
      else
	{
	  int ign;
	  vfdbuf_shift (&ign, fd, -1);
	}
    }

  if (bufs[fd].buffered <= bufs[fd].read_write_pos)
    size_needed = bufs[fd].read_write_pos + len;
  else
    size_needed = bufs[fd].buffered + len;
  
  if (size_needed > bufs[fd].bufsize)
    realloc_buf (fd, size_needed);

  if (bufs[fd].buffered <= bufs[fd].read_write_pos)
    {
      mem_move (bufs[fd].buf + bufs[fd].read_write_pos, str, len);
      bufs[fd].buffered = bufs[fd].read_write_pos + len;
    }
  else
    {
      mem_move (bufs[fd].buf + bufs[fd].read_write_pos + len,
		bufs[fd].buf + bufs[fd].read_write_pos,
		bufs[fd].buffered - bufs[fd].read_write_pos);
      mem_move (bufs[fd].buf + bufs[fd].read_write_pos, str, len);
      bufs[fd].buffered += len;
    }

  return 0;
}


/*(c vfdbuf_more)
 * int vfdbuf_more (int * errn,
 *		    t_uchar ** buffer,
 *		    long * bufsize,
 *		    t_uchar ** read_write_pos,
 *		    long * buffered,
 *		    int fd,
 *		    long opt_amt);
 * 
 * Buffer (by reading) additional characters for descriptor `fd'.
 * Return the address of and number of buffered unread characters
 * (including previously buffered but unread characters).
 *
 * `opt_amt' is the number of additional characters to read or 0 to
 * read a default number of additional characters.
 * 
 * Ordinarilly, no characters are discarded from the buffer by this
 * function.  (See `vfdbuf_advance' and `vfdbuf_shift'.)
 *
 * If the buffer was created with the flag `vfdbuf_auto_shift',
 * `opt_amt' is 0, and the last buffered character is more than 3/4 of
 * the way through the buffer's allocated memory, then `vfdbuf_more'
 * will call `vfdbuf_shift' before doing anything else.  This is the
 * only circumstance under which `vfdbuf_more' will discard characters
 * from a buffer.
 *
 * The read/write position is not changed by this function.
 *
 * Return -1 upon error, the number of characters read upon success.
 */
ssize_t
vfdbuf_more (int * errn,
	     t_uchar ** buffer,
	     long * bufsize,
	     t_uchar ** read_write_pos,
	     long * buffered,
	     int * has_zero_byte,
	     int fd,
	     long opt_amt)
{
  long size_needed;
  
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  if (!opt_amt)
    {
      if (bufs[fd].auto_shift)
	{
	  long pos;
	  long size;
	  pos = bufs[fd].buffered;
	  size = bufs[fd].bufsize;
	  if (pos * 4 > size * 3)
	    if (vfdbuf_shift (errn, fd, -1))
	      return -1;
	}

      opt_amt = bufs[fd].bufsize - bufs[fd].buffered;

      if (!opt_amt)
	opt_amt = bufs[fd].bufsize;

      if (!opt_amt)
	opt_amt = 4096;
    }
  
  size_needed = bufs[fd].buffered + opt_amt;
  
  if (size_needed > bufs[fd].bufsize)
    realloc_buf (fd, size_needed);
  
  {
    off_t offset;
    ssize_t got;
    
    offset = bufs[fd].buffer_offset + bufs[fd].buffered;

    if (offset != bufs[fd].fd_offset)
      {
	off_t went_to;

	went_to = bufs[fd].sub_handler.vtable->lseek (errn,
						      fd,
						      offset,
						      SEEK_SET,
						      bufs[fd].sub_handler.closure);
	
	if (0 > went_to)
	  return -1;

	if (went_to != offset)
	  {
	    *errn = EIO;
	    return -1;
	  }
	    
	bufs[fd].fd_offset = offset;
      }
    got = bufs[fd].sub_handler.vtable->read (errn,
					     fd,
					     bufs[fd].buf +  bufs[fd].buffered,
					     opt_amt,
					     bufs[fd].sub_handler.closure);
    if (got < 0)
      return -1;

    if (got == 0)
      bufs[fd].eof = 1;
    else
      {
	bufs[fd].fd_offset += got;
	bufs[fd].buffered += got;
      }

    if (0 > vfdbuf_getbuf (errn, buffer, bufsize, read_write_pos, buffered, has_zero_byte, fd))
      return -1;
    check_buffer_invariants (fd);
    return got;
  }
}

/************************************************************************
 *(h1 "The End-of-File")
 * 
 * 
 * 
 */


/*(c vfdbuf_is_eof)
 * int vfdbuf_is_eof (int * errn, int fd);
 * 
 * Return 1 value if the end of file has been reached while
 * reading from buffered descriptor `fd', 0 otherwise.
 *
 * Return -1 and EINVAL if `fd' is not a buffered descriptor.
 */
int
vfdbuf_is_eof (int * errn, int fd)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  return bufs[fd].eof;
}


/*(c vfdbuf_clear_eof)
 * void vfdbuf_clear_eof (int * errn, int fd);
 * 
 * Clear the eof flag for buffered file descriptor `fd'.
 *
 * Return -1 and EINVAL if `fd' is not a buffered descriptor.
 */
int
vfdbuf_clear_eof (int * errn, int fd)
{
  if (!vfdbuf_is_buffered (fd))
    {
      *errn = EINVAL;
      return -1;
    }

  bufs[fd].eof = 0;
  return 0;
}




#define vfdbuf_make_closure vu_sys_make_closure
#define vfdbuf_free_closure vu_sys_free_closure



int
vfdbuf_access (int * errn, char * path, int mode, void * closure)
{
  return vu_sys_access (errn, path, mode, closure);
}


int
vfdbuf_chdir (int * errn, char * path, void * closure)
{
  return vu_sys_chdir (errn, path, closure);
}


int
vfdbuf_chmod (int * errn, char * path, int mode, void * closure)
{
  return vu_sys_chmod (errn, path, mode, closure);
}


int
vfdbuf_chown (int * errn, char * path, int owner, int group, void * closure)
{
  return vu_sys_chown (errn, path, owner, group, closure);
}


int
vfdbuf_chroot (int * errn, char * path, void * closure)
{
  return vu_sys_chroot (errn, path, closure);
}


int
vfdbuf_closedir (int * errn, DIR * dir, void * closure)
{
  return bufs[vu_dir_fd (dir)].sub_handler.vtable->closedir (errn, dir, closure);
}


int
vfdbuf_fchdir (int * errn, int fd, void * closure)
{
  return bufs[fd].sub_handler.vtable->fchdir (errn, fd, closure);
}


int
vfdbuf_fchmod (int * errn, int fd, int mode, void * closure)
{
  return bufs[fd].sub_handler.vtable->fchmod (errn, fd, mode, closure);
}


int
vfdbuf_fchown (int * errn, int fd, int owner, int group, void * closure)
{
  return bufs[fd].sub_handler.vtable->fchown (errn, fd, owner, group, closure);
}


int
vfdbuf_fstat (int * errn, int fd, struct stat * buf, void * closure)
{
  return bufs[fd].sub_handler.vtable->fstat (errn, fd, buf, closure);
}


int
vfdbuf_link (int * errn, char * from, char * to, void * closure)
{
  return vu_sys_link (errn, from, to, closure);
}


int
vfdbuf_lstat (int * errn, char * path, struct stat * buf, void * closure)
{
  return vu_sys_lstat (errn, path, buf, closure);
}


int
vfdbuf_mkdir (int * errn, char * path, int mode, void * closure)
{
  return vu_sys_mkdir (errn, path, mode, closure);
}


int
vfdbuf_open (int * errn, char * path, int flags, int mode, void * closure)
{
  return vu_sys_open (errn, path, flags, mode, closure);
}


int
vfdbuf_opendir (int * errn, DIR ** retv,  char * path, void * closure)
{
  return vu_sys_opendir (errn, retv,  path, closure);
}


int
vfdbuf_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure)
{
  return bufs[ vu_dir_fd (dir)].sub_handler.vtable->readdir (errn, limits, file_ret, dir, closure);
}


int
vfdbuf_readlink (int * errn, char * path, char * buf, int bufsize, void * closure)
{
  return vu_sys_readlink (errn, path, buf, bufsize, closure);
}


int
vfdbuf_rename (int * errn, char * from, char * to, void * closure)
{
  return vu_sys_rename (errn, from, to, closure);
}


int
vfdbuf_rmdir (int * errn, char * path, void * closure)
{
  return vu_sys_rmdir (errn, path, closure);
}


int
vfdbuf_stat  (int * errn, char * path, struct stat * buf, void * closure)
{
  return vu_sys_stat (errn, path, buf, closure);
}


int
vfdbuf_symlink (int * errn, char * from, char * to, void * closure)
{
  return vu_sys_symlink (errn, from, to, closure);
}


int
vfdbuf_truncate (int * errn, char * path, off_t where, void * closure)
{
  return vu_sys_truncate (errn, path, where, closure);
}


int
vfdbuf_unlink (int * errn, char * path, void * closure)
{
  return vu_sys_unlink (errn, path, closure);
}


int
vfdbuf_utime (int * errn, char * path, struct utimbuf * times, void * closure)
{
  return vu_sys_utime (errn, path, times, closure);
}


int
vfdbuf_fcntl (int * errn, int fd, int cmd, long arg, void * closure)
{
  return bufs[fd].sub_handler.vtable->fcntl (errn, fd, cmd, arg, closure);
}


int
vfdbuf_dup (int * errn, int fd, void * closure)
{
  return bufs[fd].sub_handler.vtable->dup (errn, fd, closure);
}


int
vfdbuf_dup2 (int * errn, int fd, int newfd, void * closure)
{
  return bufs[fd].sub_handler.vtable->dup2 (errn, fd, newfd, closure);
}

int
vfdbuf_move_state (int * errn, int fd, int newfd, void * closure)
{
  int sub_move;
  struct vu_handler * sub_handler;

  if (fd == newfd)
    return fd;

  sub_move = bufs[fd].sub_handler.vtable->move_state (errn,
						      fd,
						      newfd,
						      bufs[fd].sub_handler.closure);
  if (0 > sub_move)
    return -1;

  /* sub_handler might have changed the underlying handler for fd
   */
  sub_handler = vu_fd_dispatch (fd);
  if (sub_handler->vtable != &vfdbuf_vtable)
    bufs[fd].sub_handler = *sub_handler;

  /* Move the buffer to the new descriptor.
   */
  ar_ref ((void **)&bufs, lim_use_must_malloc, newfd, sizeof (struct vfdbuf_record));
  bufs[newfd] = bufs[fd];
  
  /* Remove the buffer from the old descriptor.
   */
  mem_set0 ((t_uchar *)&bufs[fd], sizeof (bufs[fd]));


  /* Make sure the old fd is now using the sub_handler and not
   * a vfdbuf handler.
   */
  vu_set_fd_handler (fd,
		     bufs[newfd].sub_handler.vtable,
		     bufs[newfd].sub_handler.closure);

  /* Make sure the new fd is now using a vfdbuf handler.
   */
  vu_set_fd_handler (newfd, &vfdbuf_vtable, 0);

  return 0;
}


int
vfdbuf_close (int * errn, int fd, void * closure)
{
  int got;

  check_buffer_invariants (fd);
  if (!bufs[fd].dont_flush && (bufs[fd].flags != O_RDONLY) && bufs[fd].buffered && bufs[fd].read_write_pos)
    {
      int flush;

      flush = vfdbuf_flush (errn, fd);
      if (flush < 0)
	return -1;
    }

  got = bufs[fd].sub_handler.vtable->close (errn,
					    fd,
					    bufs[fd].sub_handler.closure);
  bufs[fd].sub_handler.vtable->free_closure (bufs[fd].sub_handler.closure);
  if (!bufs[fd].free_buffer)
    must_free (bufs[fd].buf);
  else
    bufs[fd].free_buffer (bufs[fd].buf, bufs[fd].free_buffer_closure);
  mem_set ((t_uchar *)&bufs[fd], 0, sizeof (bufs[fd]));
  return got;
}


int
vfdbuf_fsync (int * errn, int fd, void * closure)
{
  if ((bufs[fd].flags != O_RDONLY) && bufs[fd].buffered && bufs[fd].read_write_pos)
    {
      if (bufs[fd].dont_flush)
	{
	  *errn = EINVAL;
	  return -1;
	}
      if (vfdbuf_flush (errn, fd) < 0)
	return -1;
    }

  return bufs[fd].sub_handler.vtable->fsync (errn, fd, bufs[fd].sub_handler.closure);
}


int
vfdbuf_ftruncate (int * errn, int fd, off_t where, void * closure)
{
  int got;

  got = bufs[fd].sub_handler.vtable->ftruncate (errn,
						fd,
						where,
						bufs[fd].sub_handler.closure);
  if (got < 0)
    return -1;

  if (bufs[fd].buffer_offset >= got)
    {
      bufs[fd].buffer_offset += bufs[fd].read_write_pos;
      bufs[fd].read_write_pos = 0;
      bufs[fd].buffered = 0;
    }
  else if (bufs[fd].buffer_offset + bufs[fd].buffered >= got)
    {
      long retained;

      retained = got - bufs[fd].buffer_offset;
      mem_set (bufs[fd].buf + retained, 0, bufs[fd].buffered - retained);
      bufs[fd].buffered = retained;
    }

  check_buffer_invariants (fd);
  return 0;
}


ssize_t
vfdbuf_read (int * errn, int fd, char * buf, size_t count, void * closure)
{
  ssize_t orig_count;
  ssize_t avail;
  ssize_t filled_in;

  orig_count = count;
  if (bufs[fd].buffered > bufs[fd].read_write_pos)
    avail = bufs[fd].buffered - bufs[fd].read_write_pos;
  else
    avail = 0;

  filled_in = (avail < count ? avail : count);
  mem_move (buf, bufs[fd].buf + bufs[fd].read_write_pos, filled_in);
  bufs[fd].read_write_pos += filled_in;
  buf += filled_in;
  count -= filled_in;

  check_buffer_invariants (fd);

  if (!count)
    return orig_count;
  else
    {
      ssize_t got;

      /* We have consumed all buffered characters without
       * completing the read.
       */
      invariant (bufs[fd].buffered == bufs[fd].read_write_pos);

      if (!bufs[fd].dont_flush)
	{
	  /* bufs[fd].dont_flush == 0
	   *
	   * Now read buffer-sized chunks directly into the 
	   * caller's buffer, but do this only if we are 
	   * permitted to flush the vfdbuf buffer.
	   */

	  if (0 > vfdbuf_shift (errn, fd, -1))
	    {
	      if (orig_count == count)
		return -1;
	      else
		{
		  check_buffer_invariants (fd);
		  return orig_count - count;
		}
	    }

	  /* The vfdbuf buffer is empty: 
	   */
	  invariant (bufs[fd].read_write_pos == 0);
	  invariant (bufs[fd].buffered == 0);

	  if (sync_offset (errn, fd) < 0)
	    {
	      if (orig_count == count)
		return -1;
	      else
		{
		  check_buffer_invariants (fd);
		  return orig_count - count;
		}
	    }
	  
	  while (count > bufs[fd].bufsize)
	    {
	      /* The following invariant is true because either we:
	       *
	       *	1. just called sync_offset
	       * or 2. added equal amounts to both `buffer_offset' and `fd_offset'.
	       *
	       */
	      invariant (bufs[fd].buffer_offset == bufs[fd].fd_offset);
	      got = bufs[fd].sub_handler.vtable->read (errn,
						       fd,
						       buf,
						       bufs[fd].bufsize,
						       bufs[fd].sub_handler.closure);
	      if (got < 0)
		{
		  if (count == orig_count)
		    return -1;
		  check_buffer_invariants (fd);
		  return orig_count - count;
		}
	      if (got == 0)
		{
		  bufs[fd].eof = 1;
		  check_buffer_invariants (fd);
		  return orig_count - count;
		}
	      bufs[fd].fd_offset += got;
	      bufs[fd].buffer_offset += got;
	      buf += got;
	      count -= got;
	    }
	}

      /* There may be more to read because either
       *
       * 	1. We weren't permitted to read buffersize chunks
       *	   directly into the caller's buffer because
       *	   `bufs[fd].dont_flush != 0'.
       *
       *  or	2. We read buffersize chunks, but there remains
       *	   more characters (smaller than the size of the 
       * 	   buffer) yet to read.
       *
       */
      if (count)
	{
	  /* Buffer some additional characters internally, then copy
	   * those to the caller's buffer.
	   */
	  if (vfdbuf_more (errn, 0, 0, 0, 0, 0, fd, 0) < 0)
	    {
	      if (orig_count == count)
		return -1;
	      else
		{
		  check_buffer_invariants (fd);
		  return orig_count - count;
		}
	    }
	  
	  avail = bufs[fd].buffered - bufs[fd].read_write_pos;
	  filled_in = (avail < count ? avail : count);
	  mem_move (buf,
		    bufs[fd].buf + bufs[fd].read_write_pos,
		    filled_in);
	  bufs[fd].read_write_pos += filled_in;
	  buf += filled_in;
	  count -= filled_in;
	}
      check_buffer_invariants (fd);
      return orig_count - count;
    }
}


ssize_t
vfdbuf_write (int * errn, int fd, char * buf, size_t count, void * closure)
{
  ssize_t orig_count;
  ssize_t avail;
  ssize_t filled_in;

  orig_count = count;
  avail = bufs[fd].bufsize - bufs[fd].read_write_pos;
  if (bufs[fd].dont_flush && (avail < count))
    {
      ssize_t added;
      added = count - avail;
      realloc_buf (fd, bufs[fd].bufsize + added);
      avail = bufs[fd].bufsize - bufs[fd].read_write_pos;
      invariant (avail >= count);
    }
  filled_in = (avail < count ? avail : count);
  mem_move (bufs[fd].buf + bufs[fd].read_write_pos,
	    buf,
	    filled_in);
  bufs[fd].read_write_pos += filled_in;
  if (bufs[fd].read_write_pos > bufs[fd].buffered)
    bufs[fd].buffered = bufs[fd].read_write_pos;
  buf += filled_in;
  count -= filled_in;

  check_buffer_invariants (fd);
  if (!count)
    {
      invariant (filled_in == orig_count);
      return orig_count;
    }
  else
    {
      int errn2;

      if (bufs[fd].buffered < bufs[fd].read_write_pos)
	{
	  /* We are about to write buffered characters.  The
	   * read_write_pos is past the last buffered character so there
	   * has been an lseek (or similar operation) that left characters
	   * in the buffer which are not yet part of the file.  The
	   * current write ensures that the characters between the
	   * end-of-flie and the current read_write_pos are now part of
	   * the file.  To make sure that `flush' writes those characters,
	   * we do this:
	   */
	  bufs[fd].buffered = bufs[fd].read_write_pos;
	}
      if (vfdbuf_flush (&errn2, fd) < 0)
	{
	  if (filled_in)
	    {
	      check_buffer_invariants (fd);
	      return filled_in;
	    }
	  else
	    {
	      *errn = errn2;
	      return -1;
	    }
	}
      /* This is not a general invariant of `vfdbuf_flush', but
       * works here because we made sure that all characters before
       * the read_write_pos were considered buffered before calling
       * `vfdbuf_flush'.
       *
       * This invariant is important before the call to `sync_offset'.
       */
      invariant (bufs[fd].read_write_pos == 0);
      if (sync_offset (errn, fd) < 0)
	{
	  if (count == orig_count)
	    return -1;
	  else
	    {
	      check_buffer_invariants (fd);
	      return orig_count - count;
	    }
	}
      while (count > bufs[fd].bufsize)
	{
	  ssize_t wrote;
	  wrote = bufs[fd].sub_handler.vtable->write (errn,
						      fd,
						      buf,
						      bufs[fd].bufsize,
						      bufs[fd].sub_handler.closure);
	  if (wrote < 0)
	    return -1;
	  bufs[fd].fd_offset += wrote;
	  buf += wrote;
	  count -= wrote;
	  if (wrote < bufs[fd].bufsize)
	    {
	      check_buffer_invariants (fd);
	      return orig_count - count;
	    }
	}
      mem_move (bufs[fd].buf, buf, count);
      bufs[fd].buffer_offset = bufs[fd].fd_offset;
      bufs[fd].read_write_pos = count;
      bufs[fd].buffered = count;
      check_buffer_invariants (fd);
      return orig_count;
    }
}


off_t
vfdbuf_lseek (int * errn, int fd, off_t offset, int whence, void * closure)
{
  check_buffer_invariants (fd);
  if (bufs[fd].is_pipe)
    {
      if (whence == SEEK_SET)
	{
	  if ((bufs[fd].flags & O_ACCMODE) == O_RDONLY)
	    {
	      /* is_pipe, SEEK_SET, O_RDONLY */
	      if (offset == bufs[fd].fd_offset)
		return offset;
	      else
		{
		espipe_error:
		  *errn = ESPIPE;
		  return -1;
		}
	    }
	  else
	    {
	      /* is_pipe, SEEK_SET, O_WRONLY */
	      if (offset == (bufs[fd].buffer_offset + bufs[fd].buffered))
		return offset;
	      else
		goto espipe_error;
	    }
	  /* not reached */
	}
      else if ((whence == SEEK_CUR) && (offset == 0))
	{
	  if ((bufs[fd].flags & O_ACCMODE) == O_RDONLY)
	    {
	      /* is_pipe, SEEK_CUR, O_RDONLY */
	      return bufs[fd].fd_offset;
	    }
	  else
	    {
	      /* is_pipe, SEEK_CUR, O_WRONLY */
	      return bufs[fd].fd_offset + bufs[fd].buffered;
	    }
	}
      else
	goto espipe_error;

      /* is_pipe, not reached */
    }

  /* !is_pipe */
  switch (whence)
    {
    default:
      *errn = EINVAL;
      return -1;
      
    case SEEK_SET:
      break;
      
    case SEEK_CUR:
      offset += (bufs[fd].buffer_offset + bufs[fd].read_write_pos);
      break;

    case SEEK_END:
      {
	struct stat statb;

	if (0 > vu_fstat (errn, fd, &statb))
	  return -1;

	offset += statb.st_size;
      }
      break;
    }

  if ((offset >= bufs[fd].buffer_offset)
      && (offset <= (bufs[fd].buffer_offset + bufs[fd].buffered)))
    bufs[fd].read_write_pos = offset - bufs[fd].buffer_offset;
  else
    {
      if (((bufs[fd].flags & O_ACCMODE) != O_RDONLY) && bufs[fd].buffered && bufs[fd].read_write_pos)
	{
	  /* This is a writable descriptor and there are valid buffered characters
	   * preceeding the read/write position.
	   */
	  if (bufs[fd].dont_flush)
	    {
	      *errn = EINVAL;
	      return -1;
	    }
	  if (0 > vfdbuf_flush (errn, fd))
	    return -1;
	  invariant (!bufs[fd].buffered);
	}
      /* Either this buffer is read-only, or there are no valid characters 
       * in the buffer.
       */
      bufs[fd].buffer_offset = offset;
      bufs[fd].read_write_pos = 0;
      bufs[fd].buffered = 0;
    }
  check_buffer_invariants (fd);
  return bufs[fd].buffer_offset + bufs[fd].read_write_pos;
}




struct vu_fs_discipline vfdbuf_vtable \
= { VU_FS_DISCIPLINE_INITIALIZERS (vfdbuf_) };

