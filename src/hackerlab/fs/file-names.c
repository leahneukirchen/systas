/* file-names.c - file-name manipulations
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/stdlib.h"
#include "hackerlab/os/pwd.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/fs/file-names.h"


/************************************************************************
 *(h0 "File-name Manipulation Functions"
 *    :include ("fs/file-names.h"))
 * 
 * The functions in this chapter are for managing strings which are
 * file names.  Though the actual implementation is unix-specific, the
 * calling conventions for these functions (based on the design of GNU
 * Emacs) should support other operating systems as well.
 *
 * \WARNING: some of the functions in this chapter use `must_malloc' 
 * when it would be better to use some other allocation function.
 * This is likely to be fixed in a future release./
 */
/*(menu)
 */

/************************************************************************
 *(h1 "Home Directories")
 * 
 * 
 * 
 */

/*(c file_name_home_directory)
 * char * file_name_home_directory (void);
 *
 * Return the user's home directory.
 *
 * The path returned should not be freed by the caller and
 * may be returned more than once by `file_name_home_directory'.
 *
 * The home directory is defined to be the value of the
 * environment variable HOME, unless that variable is unbound
 * or is bound to an empty string.  If HOME is undefined, then
 * the home directory is found using `getpwuid' and the 
 * real user id of the process.
 *
 * If the home directory can not be computed, or storage can not
 * be allocated, `panic' is called to force the process to exit.
 *
 * The directory name returned is formatted as if returned
 * by `file_name_as_directory' (it will end with a single `/').
 */
t_uchar *
file_name_home_directory (void)
{
  static t_uchar * path = 0;
  t_uchar * env;
  int uid;
  struct passwd * pwent;

  if (path)
    return path;

  env = getenv ("HOME");
  if (env && env[0])
    path = file_name_as_directory (lim_use_must_malloc, env);
  else
    {
      uid = getuid ();
      pwent = getpwuid (uid);
      if (!pwent)
	panic ("unable to compute a home directory");
      path = file_name_as_directory (lim_use_must_malloc, pwent->pw_dir);
    }
  return path;
}


/*(c file_name_is_absolute)
 * int file_name_is_absolute (t_uchar * f);
 * 
 * 	return f && f[0] && (f[0] == '/');
 */
int
file_name_is_absolute (t_uchar * f)
{
  return f && f[0] && (f[0] == '/');
}


/*(c file_name_tilde_expand)
 * t_uchar * file_name_tilde_expand (alloc_limits limits, char * fname);
 *
 * Return a newly allocated copy of `fname' with a leading `~' or
 * `~user' expanded to the appropriate home directory.
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 * 
 * If asked to expand `~', and unable to find a home directory for the
 * current process, this function panics and exits.
 *
 * If asked to expand `~user', and unable to find a home directory for
 * `user', this function returns a newly allocated copy of `fname'.
 *
 */
t_uchar *
file_name_tilde_expand (alloc_limits limits, t_uchar * fname)
{
  if (fname[0] != '~')
    return str_save (limits, fname);
  else if (!fname[1] || (fname[1] == '/'))
    {
      t_uchar * home;
      home = file_name_home_directory ();
      if (!fname[1])
	{
	  /* Return a file name without a trailing "/"
	   */
	  return file_name_from_directory (limits, home);
	}
      else if (!fname[2])
	{
	  /* Return a file name with a trailing "/"
	   */
	  return str_save (limits, home);
	}
      else
	{
	  t_uchar * pos;
	  pos = fname + 2;
	  while (*pos == '/')
	    {
	      ++pos;
	    }
	  if (!*pos)
	    {
	      /* Return a file name with a trailing "/"
	       */
	      return str_save (limits, home);
	    }
	  else
	    {
	      /* Combine the ~-expansion with the remaining file-name.
	       */
	      return file_name_in_vicinity (limits, home, pos);
	    }
	}
    }
  else
    {
      t_uchar * uname_end;
      int uname_len;
      t_uchar * uname;
      struct passwd * pwent;

      uname_end = str_chr_index (fname + 1, '/');
      if (!uname_end)
	uname_end = fname + str_length (fname);

      uname_len = uname_end - (fname + 1);
      uname = (t_uchar *)alloca (uname_len + 1);
      str_cpy_n (uname, fname + 1, uname_len);
      uname[uname_len] = 0;
      
      pwent = getpwnam (uname);
      if (!pwent)
	{
	  /* unknown user -- return fname unmodified
	   */
	  return str_save (limits, fname);
	}
      if (*uname_end && uname_end[1])
	{
	  /* ~uname/relative-path
	   *
	   * Return relative-path relative to the expanded
	   * home directory.
	   */
	  return file_name_in_vicinity (limits, pwent->pw_dir, uname_end + 1);
	}
      else if (*uname_end)
	{
	  /* ~uname/
	   * 
	   * Return the expanded home directory, with
	   * a trailing "/".
	   */
	  return file_name_as_directory (limits, pwent->pw_dir);
	}
      else
	{
	  /* ~uname
	   * 
	   * Return the expanded home directory, without
	   * a trailing "/".
	   */
	  return file_name_from_directory (limits, pwent->pw_dir);
	}
    }
}

/************************************************************************
 *(h1 "File-name Algebra")
 * 
 * 
 * 
 */


/*(c file_name_tail)
 * char * file_name_tail (alloc_limits limits, char * fname);
 * 
 * Return a newly allocated string containing only the last element of
 * the file-name `fname'.  The last element is everything following
 * the last `"/"' or all of `fname' if it contains no `"/"'
 * characters.
 *
 *	"/usr/bin/ls"	=>	"ls"
 *	"/usr/bin/"	=>	""
 *	"ls"		=>	"ls"
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar *
file_name_tail (alloc_limits limits, t_uchar * fname)
{
  t_uchar * s;

  s = str_chr_rindex (fname, '/');
  return str_save (limits, s ? s + 1 : fname);
}


/*(c file_name_as_directory)
 * t_uchar * file_name_as_directory (alloc_limits limits, t_uchar * f);
 * 
 * Return a newly allocated copy of file-name `f', ensuring that the
 * copy ends in a single `"/"'.
 *
 *	"/usr/bin/ls"	=>	"/usr/bin/ls/"
 *	"/usr/bin/"	=>	"/usr/bin/"
 *	"/usr/bin///"	=>	"/usr/bin/"
 *	"ls"		=>	"ls/"
 * 	"/"		=>	"/"
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar *
file_name_as_directory (alloc_limits limits, t_uchar * f)
{
  int len;
  int already_is;

  len = str_length (f);
  if (!len)
    return str_save (limits, f);
  already_is = (f[len - 1] == '/');
  while (len && (f[len - 1] == '/'))
    --len;

  if (already_is)
    return str_save_n (limits, f, len + 1);
  else
    {
      t_uchar * answer;
      answer = lim_malloc (limits, len + 2);
      if (!answer)
	return 0;
      mem_move (answer, f, len);
      answer[len] = '/';
      answer[len + 1] = 0;
      return answer;
    }
}


/*(c file_name_from_directory)
 * t_uchar * file_name_from_directory (alloc_limits limits, t_uchar * f);
 * 
 * Return a newly allocated copy of file-name `f', removing all 
 * trailing `"/"' characters.  The root directory is treated specially:
 *
 *	"/usr/bin/ls"	=>	"/usr/bin/ls"
 *	"ls"		=>	"ls"
 *	"/usr/bin/"	=>	"/usr/bin"
 *	"/usr/bin///"	=>	"/usr/bin"
 *	"/"		=>	"/"
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar * 
file_name_from_directory (alloc_limits limits, t_uchar * f)
{
  t_uchar * answer;
  int len;

  answer = str_save (limits, f);
  if (!answer)
    return 0;
  len = str_length (f);
  if (len == 1)
    return answer;
  while (len-- && (answer[len] == '/'))
    answer[len] = 0;
  return answer;
}


/*(c file_name_directory)
 * t_uchar * file_name_directory (alloc_limits limits, t_uchar * f);
 * 
 * Return a newly allocated copy of the directory portion of file-name
 * `f', ensuring that the copy ends with a single `"/"'.
 *
 * The directory portion is everything except the last component of a
 * file-name.  If `f' already ends with a `"/"', then this function is
 * the same as `file_name_as_directory'.  If `f' has no directory
 * component, this function returns 0:
 *
 *	"/usr/bin/ls"	=>	"/usr/bin/"
 *	"/usr/bin///ls"	=>	"/usr/bin/"
 *	"/usr/bin/"	=>	"/usr/bin/"
 *	"/usr/bin"	=>	"/usr/"
 *	"ls"		=>	0
 *	"/"		=>	"/"
 * 
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar *
file_name_directory (alloc_limits limits, t_uchar * f)
{
  int len;
  int already_is;

  len = str_length (f);
  if (!len)
    return 0;
  already_is = (f[len - 1] == '/');
  while (len && (f[len - 1] == '/'))
    --len;

  if (already_is)
    return str_save_n (limits, f, len + 1);
  else
    {
      t_uchar * answer;
      while (len && (f[len - 1] != '/'))
	--len;
      if (!len)
	return 0;
      while (len && (f[len - 1] == '/'))
	--len;
      ++len;
      answer = lim_malloc (limits, len + 1);
      if (!answer)
	return 0;
      mem_move (answer, f, len);
      answer[len] = 0;
      return answer;
    }
}


/*(c file_name_in_vicinity)
 * t_uchar * file_name_in_vicinity (alloc_limits limits,
 * 			  	    t_uchar * dir,
 * 			  	    t_uchar * file);
 * 
 * Return a newly allocated string which is `dir', followed by `"/"',
 * followed by `file'.  If `dir' is a directory name and `file' is a
 * relative file-name, then the result is a name for `file', relative
 * to `dir'.  If `dir' already ends in `"/"', no additional `"/"' is
 * added.
 *
 * If `dir' is 0 or `""', a newly allocated copy of `file' is returned.
 *
 * If `file' is an absolute file name (begins with `"/"') a newly allocated copy
 * of `file' is returned.
 *
 *	"/usr/bin", "ls"	=>	"/usr/bin/ls"
 *	"/usr/bin/", "ls"	=>	"/usr/bin/ls"
 *	"/usr/bin///", "ls"	=>	"/usr/bin/ls"
 *	"/etc", "/usr/bin/ls"	=>	"/usr/bin/ls"
 *	0, "ls"			=>	"ls"
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar *
file_name_in_vicinity (alloc_limits limits,
		       t_uchar * dir,
		       t_uchar * name)
{
  int dl;
  int nl;
  t_uchar * answer;

  if (name && (*name == '/'))
    return str_save (limits, name);

  dl = str_length (dir);
  if (dl && (dir[dl - 1] == '/'))
    {
      while (dl && (dir[dl - 1] == '/'))
	--dl;
      ++dl;
    }
  nl = str_length (name);
  answer = (t_uchar *)lim_malloc (limits, dl + nl + 2);
  if (!answer)
    return 0;
  if (dir && dir[0])
    {
      mem_move (answer, dir, dl);
      if (answer[dl - 1] != '/')
	answer[dl++] = '/';
      str_cpy (answer + dl, name);
    }
  else
    {
      mem_move (answer, name, nl);
      answer[nl] = 0;
    }
  return answer;
}


/************************************************************************
 *(h1 "Paths")
 * 
 * 
 * 
 */

/*(c path_parse)
 * t_uchar ** path_parse (alloc_limits limits, char * path);
 * 
 * Convert a path of file-names separated by ':' to form a 0-terminated
 * array of file-names.
 *
 * An empty file-name is converted to `"."'.
 *
 * The array is allocated by the variable sized array functions and
 * should be freed using `ar_free' (see xref:"Variable Size Arrays").
 * Note that `ar_size' returns the size of the array, including the
 * final 0.
 *
 * The elements of the array are allocated by `must_malloc' and should be
 * freed using `must_free'.  *THIS IS A BUG.*
 *
 * The function `free_path' can be used to free a path allocated by
 * `path_parse'.
 *
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar **
path_parse (alloc_limits limits, t_uchar * path)
{
  t_uchar ** answer;
  t_uchar ** loc;

  answer = 0;
  while (*path)
    {
      if (*path == ':')
	{
	  loc = (t_uchar **)ar_push ((void **)&answer, limits, sizeof (t_uchar *));
	  if (!loc)
	    {
	      int x;
	    espace_error:

	      x = ar_size ((void *)answer, limits, sizeof (*answer));
	      while (x--)
		{
		  lim_free (limits, answer[x]);
		}
	      ar_free ((void **)&answer, limits);
	      return 0;
	    }
	  *loc = (t_uchar *)str_save (limits, ".");
	  if (!*loc)
	    goto espace_error;
	  ++path;
	}
      else
	{
	  t_uchar * end;

	  end = str_chr_index (path, ':');
	  if (!end)
	    end = path + str_length (path);
	  loc = (t_uchar **)ar_push ((void **)&answer, lim_use_must_malloc, sizeof (t_uchar *));
	  if (!loc)
	    goto espace_error;
	  *loc = (t_uchar *)str_save_n (lim_use_must_malloc, path, end - path);
	  if (!*loc)
	    goto espace_error;
	  path = end;
	  if ((*path == ':') && (*(path + 1)))
	    ++path;
	}
    }
  loc = (t_uchar **)ar_push ((void **)&answer, lim_use_must_malloc, sizeof (t_uchar *));
  if (!loc)
    goto espace_error;
  *loc = 0;
  return answer;
}


/*(c free_path)
 * void free_path (alloc_limits limits, t_uchar ** path)
 * 
 * Free a path allocated by `path_parse'.
 */
void
free_path (alloc_limits limits, t_uchar ** path)
{
  if (path)
    {
      int x;
      for (x = 0; path[x]; ++x)
	lim_free (limits, path[x]);
      ar_free ((void **)&path, limits);
    }
}

/*(c path_find_executable)
 * t_uchar * path_find_executable (alloc_limits limits, 
 *				   t_uchar ** path,
 *				   t_uchar * tail);
 * 
 * Search on `path' for a file named `tail' that is executable by 
 * this process.  Returns a newly allocated copy of the name of such a file 
 * or 0.
 *
 * The file-name returned is formed by `file_name_in_vicinity (limits, path[N], tail)' 
 * and should be freed by `must_free'.
 *
 * The effective user and group ids are used to check whether or not the
 * file can be executed (which should be the same rule used by the kernel).
 * 
 * If allocation fails (but does not call `panic') this function returns 0.
 */
t_uchar *
path_find_executable (alloc_limits limits,
		      t_uchar ** path,
		      t_uchar * tail)
{
  int x;
  t_uchar * answer;
  int euid;
  int egid;

  euid = geteuid ();
  egid = getegid ();
  x = 0;
  while (path[x])
    {
      struct stat stat;
      int stat_ok;
      int errn;
      
      answer = file_name_in_vicinity (limits, path[x], tail);
      if (!answer)
	return 0;
      stat_ok = vu_stat (&errn, answer, &stat);
      if (stat_ok == 0)
	{
	  if (euid == stat.st_uid)
	    {
	      if (stat.st_mode & S_IXUSR)
		return answer;
	    }
	  if (egid == stat.st_gid)
	    {
	      if (stat.st_mode & S_IXGRP)
		return answer;
	    }
	  if (stat.st_mode & S_IXOTH)
	    return answer;
	}
      lim_free (limits, answer);
      ++x;
    }

  answer = 0;
  return answer;
}

