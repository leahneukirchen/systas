/* tmp-files.c: 
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/os/stdlib.h"
#include "hackerlab/os/sys/types.h"
#include "hackerlab/os/unistd.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/fs/file-names.h"
#include "hackerlab/fs/tmp-files.h"



char *
tmp_dir (struct alloc_limits * limits)
{
  t_uchar * a;

  a = getenv ("TMPDIR");

  if (!a)
    a = "/tmp";

  return str_save (limits, a);
}


int
tmp_open (int * errn,
	  char ** name,
	  struct alloc_limits * limits,
	  char * basename,
	  int flags, int mode)
{
  char * dir;
  char * file;
  int len;

  dir = 0;
  file = 0;
  flags |= (O_CREAT | O_EXCL);

  dir = tmp_dir (limits);
  if (!dir)
    {
    enomem_error:
      *errn = ENOMEM;
    other_error:
      if (dir)
	lim_free (limits, dir);
      if (file)
	lim_free (limits, file);
      return -1;
    }

  file = file_name_in_vicinity (limits, dir, basename);
  if (!file)
    goto enomem_error;

  len = str_length (dir) + str_length (basename);

  {
    char * t;
    t = lim_realloc (limits, file, len + 64);
    if (!t)
      goto enomem_error;
    file = t;
  }

  file[len] = '.';
  ++len;
  
  {
    t_ulong seq;

    seq = (t_ulong)getpid ();

    while (1)
      {
	int fd;

	cvt_ulong_to_decimal (file + len, seq);
	fd = vu_open (errn, file, flags, mode);
	if (fd >= 0)
	  {
	    *name = file;
	    lim_free (limits, dir);
	    return fd;
	  }
	if (*errn != EEXIST)
	  goto other_error;
	++seq;
      }
  }
  panic ("tmp-files: not reached!");
  return -1;
}


int
tmp_open_anonymous (int * errn, int flags, int mode)
{
  char * file;
  int fd;
  int ign;

  fd = tmp_open (errn, &file, lim_use_malloc, "anon", flags, mode);
  if (fd < 0)
    return -1;

  if (0 > vu_unlink (&ign, file))
    {
      safe_printfmt (2, "\n");
      safe_printfmt (2, "tmp_open_anonymous: unable to unlink anonymous tmp file\n");
      safe_printfmt (2, "    file: %s\n", file);
      safe_printfmt (2, "    error: %s (%d)\n", errno_to_string (ign), ign);
      safe_printfmt (2, "\n");
      panic ("unable to unlink anonymous tmp file\n");
    }
  lim_free (lim_use_malloc, file);
  
  return fd;
}




/* tag: Tom Lord Fri Feb 22 13:46:41 2002 (tmp-files.c)
 */
