/* tag: Tom Lord Tue Dec  4 14:41:41 2001 (vu-utils-vfdbuf.c)
 */
/* vu-utils-vfdbuf.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */




#include "hackerlab/os/errno.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/char/str.h"
#include "hackerlab/vu/vu-utils-vfdbuf.h"



/*(c vfdbuf_next_line)
 * int vfdbuf_next_line (int * errn, t_uchar ** line, long * len, int fd);
 * 
 * Return the next (newline terminated) line of input from `fd'.
 * 
 * `fd' should be a buffered file.  If it is not, a buffer is imposed
 * using `vfdbuf_buffer_fd', passing the `vfdbuf_auto_shift' flag.
 * 
 * If no error occurs, return 0.  The line is returned in `*line' and
 * its length in `*len'.  The line is stored in the buffer for `fd' and
 * may be overwritten by subsequent calls to this or other I/O functions
 * operating on `fd'.
 * 
 * If an error occurs, return -1 and fill `*errn'.
 * 
 * The newline character is included in the line returned.  If the
 * file ends with a non-empty line that is not terminated by `\n', the
 * partial line (not ending in newline) is returned.
 * 
 * Because it does not copy input from the file's buffer to other
 * storage, this function can be used efficiently process an input
 * file, one line at a time.
 */
int
vfdbuf_next_line (int * errn, t_uchar ** line, long * len, int fd)
{
  long buffered;
  t_uchar * eol;

  if (!vfdbuf_is_buffered (fd))
    {
      if (vfdbuf_buffer_fd (errn, fd, 0, O_RDONLY, vfdbuf_auto_shift))
	return -1;
    }

  if (vfdbuf_getbuf (errn, 0, 0, line, &buffered, 0, fd))
    return -1;
  
  while (1)
    {
      eol = str_chr_index_n (*line, (size_t)buffered, '\n');

      if (!eol && vfdbuf_is_eof (errn, fd))
	{
	  if (!buffered)
	    {
	      *line = 0;
	      *len = 0;
	      return 0;
	    }
	  else
	    eol = *line + buffered - 1;
	}

      if (eol)
	{
	  *len = (eol - *line) + 1;
	  vfdbuf_advance (errn, fd, *len);
	  return 0;
	}

      if (   (0 > vfdbuf_more (errn, 0, 0, line, &buffered, 0, fd, 0))
	  && (*errn != EINTR))
	return -1;
    }
}


