/* vu-utils.c - vu helper functions
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/char/str.h"
#include "hackerlab/vu/vu-utils.h"


/************************************************************************
 *(h0 "VU Utilities")
 * 
 * These functions provide higher-level abstractions that capture common
 * ways of using the lower-level VU functions.
 */



/*(c vu_file_to_string)
 * int vu_file_to_string (int * errn,
 *                        t_uchar ** buf,
 *                        size_t * len,
 *                        int fd);
 * 
 * Read the entire contents of `fd' into a newly allocated string.
 * 
 * If no error occurs, return 0.  The new string is returned in `*buf';
 * its length in `*len'.
 * 
 * If an I/O error occurs, return -1 and fill `*errn'.
 * 
 * If `vu_fstat' is able to report the length of the file, a single call to 
 * `vu_read_retry' is used to read its contents.  Otherwise, repeated calls
 * to `vu_read_retry' are used.
 */
int
vu_file_to_string (int * errn,
		   t_uchar ** buf,
		   size_t * len,
		   int fd)
{
  struct stat sb;

  if (!vu_fstat (errn, fd, &sb))
    {
      *buf = (t_uchar *)must_malloc (sb.st_size);

      if (sb.st_size != vu_read_retry (errn, fd, *buf, sb.st_size))
	{
	  must_free (*buf);
	  return -1;
	}
	  
      *len = sb.st_size;
      return 0;
    }
  else
    {
#define chunk 8192
      size_t total;

      *buf = (t_uchar *)must_malloc (chunk);
      *len = chunk;
      total = 0;

      while (1)
	{
	  size_t amt;
      
	  amt = vu_read_retry (errn, fd, *buf + total, chunk);
	  if (amt < 0)
	    {
	      must_free (*buf);
	      return -1;
	    }
	  if (amt == 0)
	    {
	      *buf = (t_uchar *)must_realloc ((void *)*buf, total);
	      *len = total;
	      return 0;
	    }
	  *buf = (t_uchar *)must_realloc ((void *)*buf, total + chunk);
	}
    }
}




/*(c vu_move_fd)
 * int vu_move_fd (int * errn, int fd, int newfd);
 * 
 * Relocate `fd' to `newfd'.  `fd' must already be handled by VU.
 * 
 * Thus function performs a `vu_dup' or `vu_dup2'
 * 
 * This is useful when performing file redirections after `fork' and
 * before `exec'.
 * 
 */
int
vu_move_fd (int * errn, int fd, int newfd)
{
  int ign;

  if (fd < 0)
    {
      *errn = EINVAL;
      return -1;
    }
  if (fd == newfd)
    return 0;
  
  if (newfd == -1)
    newfd = vu_dup (errn, fd);
  else
    newfd = vu_dup2 (errn, fd, newfd);

  if (newfd < 0)
    return -1;

  if (0 > vu_move_state (errn, fd, newfd))
    {
      vu_close (&ign, newfd);
      return -1;
    }

  vu_close (&ign, fd);
  return newfd;
}




/*(c vu_file_is_directory)
 * int vu_file_is_directory (int * errn, t_uchar * name);
 * 
 * Return 1 if `name' names a directory, 0 if not, -1 on error.
 * 
 * Non-existence of any file called `name' is not an error.
 */
int
vu_file_is_directory (int * errn, t_uchar * name)
{
  int e;
  struct stat sb;

  if (0 > vu_stat (&e, name, &sb))
    {
      if (e == ENOENT)
	return 0;
      *errn = e;
      return -1;
    }

  return (S_ISDIR (sb.st_mode));
}
