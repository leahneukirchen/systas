/* cwd.c: 
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "hackerlab/os/unistd.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fs/cwd.h"



char *
current_working_directory (int * errn, struct alloc_limits * limits)
{
  char * path;
  size_t sizeof_path;

  sizeof_path = 4096;
  path = lim_malloc (limits, sizeof_path);
  if (!path)
    {
    enomem_error:
      *errn = ENOMEM;
      if (path)
	lim_free (limits, path);
      return 0;
    }

  while (1)
    {
      if (getcwd (path, sizeof_path))
	{
	  char * answer;
	  answer = lim_realloc (limits, path, str_length (path) + 1);
	  if (!answer)
	    goto enomem_error;
	  return answer;
	}
      else if (errno == ERANGE)
	{
	  char * new_path;
	  sizeof_path *= 2;
	  new_path = lim_realloc (limits, path, sizeof_path);
	  if (!new_path)
	    goto enomem_error;
	}
      else
	{
	  *errn = errno;
	  lim_free (limits, path);
	  return 0;
	}
    }
}



/* tag: Tom Lord Fri Feb 22 04:10:53 2002 (cwd.c)
 */
