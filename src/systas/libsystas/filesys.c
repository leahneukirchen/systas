 /* filesys.c - unix file system procedures
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stddef.h>
#include <limits.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <pwd.h>
#include <stdio.h>
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vu-utils.h"
#include "hackerlab/vu/vfdbuf.h"
#include "hackerlab/arrays/ar.h"
#include "systas/libsystas/scm.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/weaks.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/read-print.h"

#ifdef FD_SET
#define SELECT_TYPE 		fd_set
#define SELECT_SET_SIZE 	FD_SETSIZE
#else
#define SELECT_TYPE 		int
#define SELECT_SET_SIZE 	(sizeof (int) * 8)
#define FD_SET(n, p) 		(*(p) |= (1 << (n)))
#define FD_CLR(n, p) 		(*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) 		(*(p) & (1 << (n)))
#define FD_ZERO(p) 		(*(p) = 0)
#endif


/************************************************************************
 *(h0 "File System and Descriptor Procedures")
 *
 * The procedures in this section are a complete interface to the unix
 * file system system-calls and related functions.
 *
 * File descriptors are referred to by "file descriptor objects"
 * A file descriptor object has two parts: an integer descriptor number,
 * and a flag that says whether or not the file should be automatically
 * closed when the descriptor object is garbage collected.  In most
 * cases (everywhere it makes sense) an integer may be passed in place
 * of a descriptor object.
 * 
 * In Systas Scheme, file descriptor objects and ports are the same
 * thing.  Some descriptors may have an associated buffer.  Only
 * buffered descriptors are suitable for use with `read'.
 *
 * The procedure `integer->fd' is provided to facilitate the handling
 * of descriptors inherited from the exec'ing process.
 * 
 * The procedure `all-file-descriptors' is provided to facilitate closing
 * unwanted descriptors before calling one of the variants of `%exec'.
 *
 * Opened directories are represented by "directory objects".
 *
 * File statistics (from `%stat', `%lstat', and `%fstat') and directory
 * entries (from `%readdir') are returned in keyword/argument lists.
 *  
 * Functions whose name begins with "%" report system errors by
 * returning a symbol (the CPP macro name of the errno number).  Other
 * kinds of error, such as passing a parameter of the wrong type,
 * cause exceptions.
 *
 * The documentation for these functions frequently refer to unix man
 * pages.  When a man page refers to a constant bound to a CPP macro,
 * you should use a symbol having that same name (e.g. the symbol
 * `O_RDWR' may be passed to the Scheme procedure `%open'.)  is
 * usually a Scheme variable bound to the same value.  For example,
 * `O_RDWR', is defined for use with `%open'.
 */
/*(menu)
 */



SCM_SYMBOL (s_flock_type, "flock-type");
SCM_SYMBOL (s_SEEK_CUR, "SEEK_CUR");
SCM_SYMBOL (s_SEEK_SET, "SEEK_SET");
SCM_SYMBOL (s_SEEK_END, "SEEK_END");
SCM_SYMBOL (s_F_RDLCK, "F_RDLCK");
SCM_SYMBOL (s_F_WRLCK, "F_WRLCK");
SCM_SYMBOL (s_F_UNLCK, "F_UNLCK");
SCM_SYMBOL (s_fcntl_cmd, "fcntl-cmd");
SCM_SYMBOL (s_O_APPEND, "O_APPEND");
SCM_SYMBOL (s_O_NONBLOCK, "O_NONBLOCK");
SCM_SYMBOL (s_O_RDONLY, "O_RDONLY");
SCM_SYMBOL (s_O_RDWR, "O_RDWR");
SCM_SYMBOL (s_O_WRONLY, "O_WRONLY");
SCM_SYMBOL (s_O_ASYNC, "O_ASYNC");

SCM_SYMBOL (s_F_SETFD_flag, "F_SETFD-flag");
SCM_SYMBOL (s_FD_CLOEXEC, "FD_CLOEXEC");
SCM_SYMBOL (s_file_mode, "file-mode");
SCM_SYMBOL (s_open_flag, "open-flag");
SCM_SYMBOL (s_lseek_flag, "lseek-flag");
SCM_SYMBOL (s_S_IRUSR, "S_IRUSR");
SCM_SYMBOL (s_S_IWUSR, "S_IWUSR");
SCM_SYMBOL (s_S_IXUSR, "S_IXUSR");
SCM_SYMBOL (s_S_IRGRP, "S_IRGRP");
SCM_SYMBOL (s_S_IWGRP, "S_IWGRP");
SCM_SYMBOL (s_S_IXGRP, "S_IXGRP");
SCM_SYMBOL (s_S_IROTH, "S_IROTH");
SCM_SYMBOL (s_S_IWOTH, "S_IWOTH");
SCM_SYMBOL (s_S_IXOTH, "S_IXOTH");
SCM_SYMBOL (s_S_ISUID, "S_ISUID");
SCM_SYMBOL (s_S_ISGID, "S_ISGID");
SCM_SYMBOL (s_S_ISVTX, "S_ISVTX");
#if 0
SCM_SYMBOL (s_S_ISTXT, "S_ISTXT");
#endif
SCM_SYMBOL (s_S_IFDIR, "S_IFDIR");
SCM_SYMBOL (s_S_IFCHR, "S_IFCHR");
SCM_SYMBOL (s_S_IFBLK, "S_IFBLK");
SCM_SYMBOL (s_S_IFREG, "S_IFREG");
SCM_SYMBOL (s_S_IFLNK, "S_IFLNK");
SCM_SYMBOL (s_S_IFSOCK, "S_IFSOCK");
SCM_SYMBOL (s_S_IFIFO, "S_IFIFO");

SCM_KEYWORD (kw_dev, "dev");
SCM_KEYWORD (kw_ino, "ino");
SCM_KEYWORD (kw_nlink, "nlink");
SCM_KEYWORD (kw_uid, "uid");
SCM_KEYWORD (kw_gid, "gid");
SCM_KEYWORD (kw_size, "size");
SCM_KEYWORD (kw_atime, "atime");
SCM_KEYWORD (kw_mtime, "mtime");
SCM_KEYWORD (kw_ctime, "ctime");

SCM_KEYWORD (kw_permissions, "permissions");
SCM_KEYWORD (kw_permission_bits, "permission-bits");
SCM_KEYWORD (kw_type, "type");



SCM * fd_table = 0;


/****************************************************************
 *(h1 "File Permissions")
 */

/*(c %chown)
 * (%chown path owner group)
 * SCM scm_sys_chown (SCM path, SCM owner, SCM group);
 * 
 * Change the owner and group of a file.  
 *
 * `path' must be a read-only string.
 *
 * `owner' and `group' are integer ids.  If either is -1, that
 * id is not changed.
 */
SCM_PROC (s_sys_chown, "%chown", 3, 0, 0, scm_sys_chown);
SCM 
scm_sys_chown (SCM path, SCM owner, SCM group)
{
  SCM_INTS_ENABLED;
  int val;
  int errn;
  SCM answer;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_chown);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));
  SCM_ASSERT (SCM_INUMP (owner), owner, scm_arg2, s_sys_chown);
  SCM_ASSERT (SCM_INUMP (group), group, scm_arg3, s_sys_chown);

  SCM_DEFER_INTS;
  val = vu_chown (&errn, SCM_RO_CHARS (path), SCM_INUM (owner), SCM_INUM (group));
  answer = val ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c file-mode->integer)
 * (file-mode->integer mode)
 * 
 * Convert to an integer flags for system calls that accept a `mode' parameter.
 *
 * `flags' may be an integer, symbol, or list of integers and symbols.
 *
 * Symbols must have the names of CPP macros used with the C functions such as `chmod',
 * e.g.: `S_IRUSR', or `S_IRGRP'.
 * 
 */
SCM_PROC (s_file_mode_to_integer, "file-mode->integer", 1, 0, 0, scm_file_mode_to_integer);
SCM
scm_file_mode_to_integer (SCM mode)
{
  return scm_integer_logior_cpp_constants (s_file_mode, mode, scm_arg1, s_file_mode_to_integer);
}


SCM
scm_mode2scm (int mode)
{
  SCM_INTS_ENABLED;
  int type;
  int perm_bits;
  SCM type_name;
  SCM perms;
  SCM answer;


  {
    perm_bits = mode & (S_ISUID|S_ISGID|S_IRWXU|S_IRWXG|S_IRWXO);  /* S_ISTXT */
    perms = SCM_EOL;
    if (S_IRUSR & mode)
      perms = scm_cons (s_S_IRUSR, perms);
    if (S_IWUSR & mode)
      perms = scm_cons (s_S_IWUSR, perms);
    if (S_IXUSR & mode)
      perms = scm_cons (s_S_IXUSR, perms);
    
    if (S_IRGRP & mode)
      perms = scm_cons (s_S_IRGRP, perms);
    if (S_IWGRP & mode)
      perms = scm_cons (s_S_IWGRP, perms);
    if (S_IXGRP & mode)
      perms = scm_cons (s_S_IXGRP, perms);
    
    if (S_IROTH & mode)
      perms = scm_cons (s_S_IROTH, perms);
    if (S_IWOTH & mode)
      perms = scm_cons (s_S_IWOTH, perms);
    if (S_IXOTH & mode)
      perms = scm_cons (s_S_IXOTH, perms);
    
    if (S_ISUID & mode)
      perms = scm_cons (s_S_ISUID, perms);
    if (S_ISGID & mode)
      perms = scm_cons (s_S_ISGID, perms);
#if 0
    if (S_ISTXT & mode)
      perms = scm_cons (s_S_ISTXT, perms);
#endif
  }

  {
    type = (S_IFMT & mode);
    if (S_IFIFO == type)
      type_name = s_S_IFIFO;
    else if (S_IFCHR == type)
      type_name = s_S_IFCHR;
    else if (S_IFDIR == type)
      type_name = s_S_IFDIR;
    else if (S_IFBLK == type)
      type_name = s_S_IFBLK;
    else if (S_IFREG == type)
      type_name = s_S_IFREG;
    else if (S_IFLNK == type)
      type_name = s_S_IFLNK;
    else if (S_IFSOCK == type)
      type_name = s_S_IFSOCK;
  }

  answer = scm_listify (kw_permissions, perms,
			kw_permission_bits, scm_long2num (perm_bits),
			kw_type, type_name,
			SCM_UNDEFINED);

  return answer;
}


/*(c integer->file-mode)
 * (integer->file-mode mode)
 * 
 * Return a symbolic form of an integer file mode.
 */
SCM_PROC (s_integer_to_file_mode, "integer->file-mode", 1, 0, 0, scm_integer_to_file_mode);
SCM
scm_integer_to_file_mode (SCM mode)
{
  SCM_ASSERT (SCM_INUMP (mode), mode, scm_arg1, s_integer_to_file_mode);
  return scm_mode2scm (SCM_INUM (mode));
}


/*(c %chmod)
 * (%chmod path mode)
 * 
 * Change the permissions of a file.  
 * See the manual page "chmod".
 *
 * `path' must be a read-only string.
 * `mode' must be an integer.
 */
SCM_PROC (s_sys_chmod, "%chmod", 2, 0, 0, scm_sys_chmod);
SCM 
scm_sys_chmod (SCM path, SCM smode)
{
  SCM_INTS_ENABLED;
  int mode;
  int rv;
  int errn;
  SCM answer;

  mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg2, s_sys_chmod);
  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_chmod);

  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  rv = vu_chmod (&errn, SCM_RO_CHARS (path), mode);
  answer = rv ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c numeric-permissions)
 * (numeric-permissions permissions)
 * 
 * Convert the file mode specification `permissions' to an integer
 * file mode specification.
 * 
 * If `permissions' is an integer, it is simply returned.
 * 
 * If `permissions' is a symbol or list of symbols, they are interpreted
 * as file mode constants from the set:
 * 
 *	!!! be more precise?
 */
SCM_PROC (s_numeric_permissions, "numeric-permissions", 1, 0, 0, scm_numeric_permissions);
SCM
scm_numeric_permissions (SCM perms)
{
  int mode;
  mode = scm_integer_logior_cpp_constants (s_file_mode, perms, scm_arg1, s_numeric_permissions);
  return SCM_MAKINUM (mode);
}


/*(c %fchmod)
 * (%fchmod fd mode)
 * 
 * Change the permissions of an open file.
 * See the manual page "fchmod".
 *
 * `fd' must be a file descriptor object or integer.
 * `mode' must be an integer.
 */
SCM_PROC (s_sys_fchmod, "%fchmod", 2, 0, 0, scm_sys_fchmod);
SCM 
scm_sys_fchmod (SCM fd, SCM smode)
{
  SCM_INTS_ENABLED;
  int d;
  int mode;
  int rv;
  int errn;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (fd) || (!SCM_IS_IMMEDIATE (fd) && SCM_FDP (fd)), fd, scm_arg1, s_sys_fchmod);
  mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg2, s_sys_fchmod);

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd))
    d = SCM_INUM (fd);
  else
    d = SCM_FD (fd);
  rv = vu_fchmod (&errn, d, mode);
  answer = rv ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return answer;
}



/*(c umask)
 * (umask (:optional mode))
 * 
 * Set the current umask.
 * See the manual page "umask".
 *
 * `mode' must be an integer.  If `mode' is not supplied, 
 * the umask is set to 0.
 */
SCM_PROC (s_umask, "umask", 0, 1, 0, scm_umask);
SCM 
scm_umask (SCM smode)
{
  SCM_INTS_ENABLED;
  int mode;

  if (SCM_UNBNDP (smode))
    mode = 0;
  else
    mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg1, s_umask);
  SCM_DEFER_INTS;
  mode = umask (mode);
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (mode);
}


/****************************************************************
 *(h1 "File Descriptor Objects")
 */

static int 
scm_fdprint (SCM sexp, SCM port, int writing __attribute__((unused)))
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<fd ");
  scm_intprint (SCM_CDR (sexp), 10, port);
  scm_port_puts (&errn, port, ">");
  return 1;
}

static size_t 
scm_fd_free (SCM p)
{
  SCM_INTS_DISABLED;
  int fd;
  int flags;
  int errn;

  fd = SCM_FD (p);
  flags = SCM_FD_FLAGS (p);
  if ((scm_close_fd_on_gc & flags) && (scm_fd_is_open & flags))
    {
      if (vfdbuf_is_buffered (fd))
	vfdbuf_set_dont_flush (&errn, fd, 0);
      vu_close (&errn, fd);
    }
  
  *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, fd, sizeof (SCM)) = 0;
  return 0;
}

long scm_tc16_fd;
static scm_small_object_functions fd_smob = {scm_mark0, scm_fd_free, scm_fdprint, 0};


/****************************************************************
 * File Descriptors
 */

/* scm_makefd
 * 
 * Construct a new file descriptor object and store it
 * in the file descriptor table.
 */
SCM
scm_makefd (int fd, int flags)
{
  SCM_INTS_NESTED;
  SCM it;
  SCM cell;

  SCM_NEWCELL (cell);

  SCM_REDEFER_INTS;
  it = *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, fd, sizeof (SCM));
  if (it)
    {
      SCM_CAR (it) = (scm_tc16_fd | (flags << 16));
      SCM_REALLOW_INTS;
      return it;
    }
  it = cell;
  SCM_CAR (it) = (scm_tc16_fd | (flags << 16));
  SCM_CDR (it) = (SCM)fd;
  *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, fd, sizeof (SCM)) = it;
  SCM_REALLOW_INTS;
  return it;
}


/*(c file-descriptor?)
 * (file-descriptor? obj)
 * 
 * Return #t if `obj' is a file descriptor object, #f otherwise.
 */
SCM_PROC (s_file_descriptor_p, "file-descriptor?", 1, 0, 0, scm_file_descriptor_p);
SCM
scm_file_descriptor_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return ((!SCM_IS_IMMEDIATE (x) && SCM_FDP (x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c integer->file-descriptor)
 * (integer->file-descriptor n)
 * 
 * If `n' is an integer, return a file descriptor object for 
 * file descriptor `n'. If a descriptor already exists for `n', 
 * return that.
 *
 * If `n' is already a file descriptor object, return `n'.
 *
 * If this descriptor is later garbage collected, file `n' will
 * be automatically closed.
 */
SCM_PROC (s_integer_to_file_descriptor,
	  "integer->file-descriptor", 1, 0, 0, scm_integer_to_file_descriptor);
SCM
scm_integer_to_file_descriptor (SCM n)
{
  SCM_INTS_ENABLED;
  if (!SCM_IS_IMMEDIATE (n) && SCM_FDP (n))
    return n;
  SCM_ASSERT (SCM_INUMP (n), n, scm_arg1, s_integer_to_file_descriptor);
  return scm_makefd (SCM_INUM (n), scm_fd_is_open | scm_close_fd_on_gc);
}


/*(c integer->existing-file-descriptor)
 * (integer->existing-file-descriptor n)
 * 
 * If `n' is an integer, return an existing file descriptor object for
 * file descriptor `n'. If no descriptor already exists for `n', return
 * #f.
 *
 * If `n' is already a file descriptor object, return `n'.
 */
SCM_PROC (s_integer_to_existing_file_descriptor,
	  "integer->existing-file-descriptor", 1, 0, 0, scm_integer_to_existing_file_descriptor);
SCM
scm_integer_to_existing_file_descriptor (SCM n)
{
  SCM_INTS_ENABLED;
  int fd;
  SCM it;

  if (!SCM_IS_IMMEDIATE (n) && SCM_FDP (n))
    return n;
  SCM_ASSERT (SCM_INUMP (n), n, scm_arg1, s_integer_to_file_descriptor);
  fd = SCM_INUM (n);
  SCM_DEFER_INTS;
  it = *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, fd, sizeof (SCM));
  SCM_ALLOW_INTS;
  if (it)
    return it;
  else
    return SCM_BOOL_F;
}


/* int scm_fileno (SCM obj);
 * 
 * Return the descriptor number associated with `obj'.
 * `obj' must be an integer or file descriptor object.
 */
int
scm_fileno (SCM obj)
{
  SCM_INTS_DISABLED;
  if (SCM_INUMP (obj))
    return SCM_INUM (obj);
  else if (!SCM_IS_IMMEDIATE (obj) && SCM_FDP (obj))
    return SCM_FD (obj);
  else
    return -1;
}

/*(c file-descriptor->integer)
 * (file-descriptor->integer fd)
 * 
 * Return the numeric descriptor associated with file descriptor object `fd'.
 *
 * If `fd' is already an integer, return `fd'.
 */
SCM_PROC (s_file_descriptor_to_integer,
	  "file-descriptor->integer", 1, 0, 0, scm_file_descriptor_to_integer);
SCM
scm_file_descriptor_to_integer (SCM x)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP (x))
    return x;
  SCM_ASSERT ((!SCM_IS_IMMEDIATE (x) && SCM_FDP (x)), x, scm_arg1, s_file_descriptor_to_integer);
  return (SCM_MAKINUM (SCM_FD (x)));
}


/*(c all-file-descriptors)
 * (all-file-descriptors)
 * 
 * Return a newly constructed list of all file live file descriptor objects.
 */
SCM_PROC (s_all_file_descriptors, "all-file-descriptors", 0, 0, 0, scm_all_file_descriptors);
SCM
scm_all_file_descriptors (void)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int x;
  int size;

  SCM_DEFER_INTS;
  answer = SCM_EOL;
  size = ar_size ((void *)fd_table, lim_use_must_malloc, sizeof (*fd_table));
  for (x = 0; x < size; ++x)
    {
      SCM fd;
      fd = fd_table[x];
      if (fd)
	answer = scm_cons (fd, answer);
    }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c autoclose-file-descriptor?)
 * (autoclose-file-descriptor? fd)
 * 
 * Return #t if the file associated with descriptor object `fd' will
 * be automatically closed when `fd' is garbage collected, #f otherwise.
 */
SCM_PROC (s_autoclose_file_descriptor_p,
	  "autoclose-file-descriptor?", 1, 0, 0, scm_autoclose_file_descriptor_p);
SCM
scm_autoclose_file_descriptor_p (SCM x)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT ((!SCM_IS_IMMEDIATE (x) && SCM_FDP (x)), x, scm_arg1, s_autoclose_file_descriptor_p);
  return ((scm_close_fd_on_gc & SCM_FD_FLAGS(x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c set-autoclose-file-descriptor!)
 * (set-autoclose-file-descriptor! fd value)
 * 
 * Set the flag that determines whether the file associated with descriptor 
 * object `fd' will be automatically closed when `fd' is garbage collected.
 * #f means that file will not be closed; all other values mean that the file
 * will be closed.
 */
SCM_PROC (s_set_autoclose_file_descriptor_x,
	  "set-autoclose-file-descriptor!", 2, 0, 0, scm_set_autoclose_file_descriptor_x);
SCM
scm_set_autoclose_file_descriptor_x (SCM x, SCM f)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT ((!SCM_IS_IMMEDIATE (x) && SCM_FDP (x)), x, scm_arg1, s_autoclose_file_descriptor_p);
  SCM_DEFER_INTS;
  if (f == SCM_BOOL_F)
    SCM_CAR (x) = (scm_tc16_fd | ((SCM_FD_FLAGS(x) & ~scm_close_fd_on_gc) << 16));
  else
    SCM_CAR (x) = (scm_tc16_fd | ((SCM_FD_FLAGS(x) | scm_close_fd_on_gc) << 16));
  SCM_ALLOW_INTS;
  return f;
}


/*(c fd-is-open?)
 * (fd-is-open? fd)
 * 
 * Return #t if the descriptor `fd' is currently open, 
 * #f if it has been closed.
 */
SCM_PROC (s_fd_is_open_p, "fd-is-open?", 1, 0, 0, scm_fd_is_open_p);
SCM
scm_fd_is_open_p (SCM obj)
{
  SCM_ASSERT (!SCM_IS_IMMEDIATE (obj) && SCM_FDP (obj), obj, scm_arg1, s_fd_is_open_p);
  return (SCM_FD_FLAGS (obj) & scm_fd_is_open) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(c %pipe)
 * (%pipe)
 * 
 * Return a list containing two file descriptor objects that are
 * ends of a newly constructed pipe.
 * 
 * See the manual page "pipe".
 */
SCM_PROC (s_sys_pipe, "%pipe", 0, 0, 0, scm_sys_pipe);
SCM 
scm_sys_pipe (void)
{
  SCM_INTS_ENABLED;
  int fd[2];
  int rv;
  SCM a;
  SCM d;
  SCM ans;

  SCM_DEFER_INTS;
  rv = pipe (fd);
  if (rv)
    {
      int en;
      en = errno;
      ans = scm_makerrno (en);
      goto done;
    }
  a = scm_makefd (fd[0], scm_fd_is_open | scm_close_fd_on_gc);
  d = scm_makefd (fd[1], scm_fd_is_open | scm_close_fd_on_gc);
  ans = scm_listify (a, d, SCM_UNDEFINED);
 done:
  SCM_ALLOW_INTS;
  return ans;
}


/*(c open-flags->integer)
 * (open-flags->integer flags)
 * 
 * Convert flags for the `%open' system call to an integer.
 *
 * `flags' may be an integer, symbol, or list of integers and symbols.
 *
 * Symbols must have the names of CPP macros used with the C function `open()',
 * e.g.: `O_RDONLY' or `O_CREAT'.
 */
SCM_PROC (s_open_flags_to_integer, "open-flags->integer", 1, 0, 0, scm_open_flags_to_integer);
SCM
scm_open_flags_to_integer (SCM flags)
{
  return scm_integer_logior_cpp_constants (s_open_flag, flags, scm_arg1, s_open_flags_to_integer);
}


/*(c %open)
 * (%open path :optional flags mode)
 * 
 * Open a file; return a file descriptor object.
 * See the manual page "open".
 *
 * `path' must be a read-only string.
 *
 * `flags' and `mode' must be integers.
 */
SCM_PROC (s_sys_open, "%open", 1, 2, 0, scm_sys_open);
SCM
scm_sys_open (SCM path, SCM sflags, SCM smode)
{
  SCM_INTS_ENABLED;
  int flags;
  int mode;
  int fd;
  int errn;
  SCM sfd;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_open);
  flags = scm_integer_logior_cpp_constants (s_open_flag, sflags, scm_arg2, s_sys_open);
  mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg3, s_sys_open);

  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  fd = vu_open (&errn, SCM_RO_CHARS (path), flags, mode);
  if (fd == -1)
    sfd = scm_makerrno (errn);
  else
    sfd = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return scm_return_first (sfd, path);
}


/*(c %create)
 * (%create path mode)
 * 
 * Create a new file.
 * See the manual page "creat".
 *
 * `path' must be a read-only string.
 * `mode' must be an integer.
 */
SCM_PROC (s_sys_create, "%create", 2, 0, 0, scm_sys_create);
SCM
scm_sys_create (SCM path, SCM smode)
{
  SCM_INTS_ENABLED;
  int mode;
  int fd;
  SCM sfd;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_create);
  mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg2, s_sys_create);

  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  fd = creat (SCM_RO_CHARS (path), mode);
  if (fd == -1)
    sfd = scm_makerrno (errno);
  else
    sfd = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;

  return scm_return_first (sfd, path);
}



/*(c %close)
 * (%close fd)
 * 
 * Close an open file.
 * See the manual page "close".
 *
 * `fd' must be a file descriptor object or integer.
 */
SCM_PROC (s_sys_close, "%close", 1, 0, 0, scm_sys_close);
SCM
scm_sys_close (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int got;

  if ((sfd == SCM_UNDEFINED) || (sfd == SCM_BOOL_F))
    sfd = scm_cur_outp;

  
  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_close);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  got = vu_close (&errn, fd);

  if (got != -1)
    {
      if (   SCM_INUMP (sfd)
	  && (fd <= ar_size ((void *)fd_table, lim_use_must_malloc, sizeof (*fd_table)))
	  && fd_table[fd])
	sfd = fd_table[fd];

      if (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd))
	SCM_CAR (sfd) = scm_tc16_fd;
    }

  SCM_ALLOW_INTS;

  return (got == -1 ? scm_makerrno (errn) : SCM_BOOL_T);
}


/*(c %write)
 * (%write fd string)
 * 
 * Write the contents of `string' on descriptor `fd'.
 * See the manual page "write".
 *
 * `fd' must be a file descriptor object or integer.
 * `string' must be a read-only string.
 */
SCM_PROC (s_sys_write, "%write", 2, 0, 0, scm_sys_write);
SCM
scm_sys_write (SCM sfd, SCM buf)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  int written;
  int errn;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_write);
  SCM_ASSERT (scm_is_ro_string (buf), buf, scm_arg2,  s_sys_write);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  written = vu_write (&errn, fd, SCM_RO_CHARS (buf), SCM_RO_LENGTH (buf));
  if (written == -1)
    answer = scm_makerrno (errn);
  else
    answer = SCM_MAKINUM (written);
  SCM_ALLOW_INTS;
  return scm_return_first (answer, buf);
}

/*(c %write-retry)
 * (%write-retry fd string)
 * 
 * Write the contents of `string' on descriptor `fd'.
 * See the manual page `vu_write_retry'.
 *
 * `fd' must be a file descriptor object or integer.
 * `string' must be a read-only string.
 */
SCM_PROC (s_sys_write_retry, "%write-retry", 2, 0, 0, scm_sys_write_retry);
SCM
scm_sys_write_retry (SCM sfd, SCM buf)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  int written;
  int errn;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_write_retry);
  SCM_ASSERT (scm_is_ro_string (buf), buf, scm_arg2,  s_sys_write_retry);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  written = vu_write_retry (&errn, fd, SCM_RO_CHARS (buf), SCM_RO_LENGTH (buf));
  if (written == -1)
    answer = scm_makerrno (errn);
  else
    answer = SCM_MAKINUM (written);
  SCM_ALLOW_INTS;

  return scm_return_first (answer, buf);
}


/*(c %read)
 * (%read fd string)
 * 
 * Read data from a file into a string.
 * See the manual page "read".
 *
 * `fd' must be a file descriptor object or integer.
 * `string' must be a writable string.
 */
SCM_PROC (s_sys_read, "%read", 2, 0, 0, scm_sys_read);
SCM
scm_sys_read (SCM sfd, SCM buf)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  char * bytes;
  int len;
  int got;
  int errn;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_read);
  SCM_ASSERT (scm_is_string (buf), buf, scm_arg2,  s_sys_read);

  bytes = SCM_STRING_CHARS (buf);
  len = SCM_STRING_LENGTH (buf);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  got = vu_read (&errn, fd, bytes, len);
  if (got == -1)
    answer = scm_makerrno (errn);
  else
    answer = SCM_MAKINUM (got);
  SCM_ALLOW_INTS;

  return scm_return_first (answer, buf);
}


/*(c %read-retry)
 * (%read-retry fd string)
 * 
 * Read data from a file into a string.
 * See the manual page "vu_read_retry".
 *
 * `fd' must be a file descriptor object or integer.
 * `string' must be a writable string.
 */
SCM_PROC (s_sys_read_retry, "%read-retry", 2, 0, 0, scm_sys_read_retry);
SCM
scm_sys_read_retry (SCM sfd, SCM buf)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  char * bytes;
  int len;
  int got;
  int errn;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_read);
  SCM_ASSERT (scm_is_string (buf), buf, scm_arg2,  s_sys_read);

  bytes = SCM_STRING_CHARS (buf);
  len = SCM_STRING_LENGTH (buf);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  got = vu_read_retry (&errn, fd, bytes, len);
  if (got == -1)
    answer = scm_makerrno (errn);
  else
    answer = SCM_MAKINUM (got);
  SCM_ALLOW_INTS;

  return scm_return_first (answer, buf);
}


/*(c %fsync)
 * (%fsync fd)
 * 
 * Flush changes to an open file to disk.
 * See the manual page "fsync".
 *
 * `fd' must be a file descriptor object or integer.
 */
SCM_PROC (s_sys_fsync, "%fsync", 1, 0, 0, scm_sys_fsync);
SCM
scm_sys_fsync(SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int status;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_fsync);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  status = vu_fsync (&errn, fd);
  if (status)
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return answer;
}


/*(c %ftruncate)
 * (%ftruncate fd where)
 * 
 * Modify the size of a file open for writing.
 * See the manual page "ftruncate".
 *
 * `fd' must be a file descriptor object or integer.
 */
SCM_PROC (s_sys_ftruncate, "%ftruncate", 2, 0, 0, scm_sys_ftruncate);
SCM
scm_sys_ftruncate (SCM sfd, SCM where)
{
  SCM_INTS_ENABLED;
  int fd;
  long w;
  int errn;
  int status;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_ftruncate);
  w = scm_num2long (where, scm_arg2, s_sys_ftruncate);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  status = vu_ftruncate (&errn, fd, w);
  if (status)
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return answer;
}


/*(c lseek-flag->integer)
 * (lseek-flag->integer flag)
 * 
 * Convert a flag for the `%lseek' system call to an integer.
 *
 * `flag' may be an integer or symbol.
 *
 * Symbols must have the names of CPP macros used with the C function `lseek()',
 * e.g.: `SEEK_CUR' or `SEEK_SET'.
 */
SCM_PROC (s_lseek_flag_to_integer, "lseek-flag->integer", 1, 0, 0, scm_lseek_flag_to_integer);
SCM
scm_lseek_flag_to_integer (SCM flag)
{
  return scm_integer_cpp_constant (s_lseek_flag, flag, scm_arg1, s_lseek_flag_to_integer);
}


/*(c %lseek)
 * (%lseek fd offset :optional whence)
 * 
 * Reposition the offset of the file descriptor `fd'.
 * See the manual page "leek".
 *
 * `fd' must be a file descriptor object or integer.
 * `offset' must be an integer.
 * `whence', if provided, must be an integer.
 */
SCM_PROC (s_sys_lseek, "%lseek", 2, 1, 0, scm_sys_lseek);
SCM
scm_sys_lseek (SCM sfd, SCM offset, SCM whence)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  int errn;
  long off;
  int wh;
  long got;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1,  s_sys_read);

  off = scm_num2long (offset, scm_arg2, s_sys_lseek);
  if (SCM_UNBNDP (whence))
    wh = SEEK_SET;
  else
    wh = scm_integer_cpp_constant (s_lseek_flag, whence, scm_arg3, s_sys_lseek);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  got = vu_lseek (&errn, fd, off, wh);
  if (got == -1)
    answer = scm_makerrno (errn);
  SCM_ALLOW_INTS;

  if (got != -1)
    answer = scm_long2num (got);
  return answer;
}


/*(c %dup)
 * (%dup fd)
 * 
 * Create a new file descriptor which is a copy of `fd'.
 * See the manual page "dup".
 *
 * `fd' must be a file descriptor object or integer.
 */
SCM_PROC (s_sys_dup, "%dup", 1, 0, 0, scm_sys_dup);
SCM
scm_sys_dup (SCM oldfd)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  int errn;
  int nfd;

  SCM_ASSERT (SCM_INUMP (oldfd) || (!SCM_IS_IMMEDIATE (oldfd) && SCM_FDP (oldfd)), oldfd, scm_arg1,  s_sys_dup);

  SCM_DEFER_INTS;
  if (SCM_INUMP (oldfd))
    fd = SCM_INUM (oldfd);
  else
    fd = SCM_FD (oldfd);
  nfd = vu_dup (&errn, fd);
  answer = (nfd == -1
	    ? scm_makerrno (errn)
	    : scm_makefd (nfd, scm_fd_is_open | scm_close_fd_on_gc));
  SCM_ALLOW_INTS;

  return answer;
}


/*(c %dup2)
 * (%dup2 oldfd newfd)
 * 
 * Copy file descriptor `oldfd' to descriptor `newfd'.
 * See the manual page "dup2".
 *
 * `oldfd' and `newfd' must be a file descriptor objects or integers.
 */
SCM_PROC (s_sys_dup2, "%dup2", 2, 0, 0, scm_sys_dup2);
SCM
scm_sys_dup2 (SCM oldfd, SCM newfd)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;
  int nfd;
  int errn;

  SCM_ASSERT (SCM_INUMP (oldfd) || (!SCM_IS_IMMEDIATE (oldfd) && SCM_FDP (oldfd)), oldfd, scm_arg1,  s_sys_dup2);
  SCM_ASSERT (SCM_INUMP (newfd) || (!SCM_IS_IMMEDIATE (newfd) && SCM_FDP (newfd)), newfd, scm_arg1,  s_sys_dup2);

  SCM_DEFER_INTS;
  if (SCM_INUMP (oldfd))
    fd = SCM_INUM (oldfd);
  else
    fd = SCM_FD (oldfd);
  if (SCM_INUMP (newfd))
    nfd = SCM_INUM (newfd);
  else
    nfd = SCM_FD (newfd);

  nfd = vu_dup2 (&errn, fd, nfd);
  answer = (nfd == -1
	    ? scm_makerrno (errn)
	    : scm_makefd (nfd, scm_fd_is_open | scm_close_fd_on_gc));
  SCM_ALLOW_INTS;

  return answer;
}


/*(c %move-fd)
 * (%move-fd oldfd newfd)
 * 
 * Move descriptor `oldfd' to descriptor `newfd'.  This moves all I/O
 * system state for `oldfd' to `newfd'.  For example, if `oldfd' is
 * buffered, the buffer is moved to `newfd'.  Return the new
 * descriptor.
 * 
 * If `oldfd' and `newfd' are the same, return `oldfd' with
 * no side effects.
 * 
 * If `newfd' is -1 or #f, allocate a new descriptor.
 *
 * If an fd object exists for `oldfd', its descriptor number is changed
 * to the target descriptor.
 * 
 * If an fd object exists for `newfd', its descriptor number is changed
 * to the source descriptor, which when this procedure returns, will have
 * have been closed.
 *
 * Return a descriptor object for the target descriptor.  If `oldfd' is
 * a descriptor object, or if a descriptor object previously existed for `oldfd',
 * that object is returned.  Otherwise, a new descriptor object is returned
 * with the `close-on-gc' flag set #t.
 */
SCM_PROC (s_sys_move_fd, "%move-fd", 2, 0, 0, scm_sys_move_fd);
SCM
scm_sys_move_fd (SCM oldfd, SCM newfd)
{
  SCM_INTS_ENABLED;
  int errn;
  SCM answer;
  int fd;
  int nfd;
  SCM prev_fd;

  SCM_ASSERT (SCM_INUMP (oldfd) || (!SCM_IS_IMMEDIATE (oldfd) && SCM_FDP (oldfd)), oldfd, scm_arg1,  s_sys_move_fd);
  SCM_ASSERT ((newfd == SCM_BOOL_F) || SCM_INUMP (newfd) || (!SCM_IS_IMMEDIATE (newfd) && SCM_FDP (newfd)), newfd, scm_arg1,  s_sys_move_fd);

  SCM_DEFER_INTS;

  if (SCM_INUMP (oldfd))
    {
      fd = SCM_INUM (oldfd);
      oldfd = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
    }
  else
    fd = SCM_FD (oldfd);

  if (SCM_BOOL_F == newfd)
    nfd = -1;
  else if (SCM_INUMP (newfd))
    nfd = SCM_INUM (newfd);
  else
    nfd = SCM_FD (newfd);

  if (fd == nfd)
    {
      answer = oldfd;
      goto return_answer;
    }

  /* Move the port.
   */
  nfd = vu_move_fd (&errn, fd, nfd);
  if (nfd == -1)
    {
      answer = scm_makerrno (errn);
      goto return_answer;
    }

  /* If another port (fd object) exists for the destination fd,
   * relocate that port to the fd we just freed up (which is now
   * closed).
   */
  prev_fd = *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, nfd, sizeof (SCM));
  if (prev_fd)
    SCM_CDR (prev_fd) = (SCM)fd;

  /* Relocate the old port object to the new fd.
   */
  SCM_CDR (oldfd) = (SCM)nfd;

  /* Update the port table.
   */
  *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, fd, sizeof (SCM)) = prev_fd;
  *(SCM *)ar_ref ((void **)&fd_table, lim_use_must_malloc, nfd, sizeof (SCM)) = oldfd;

  /* Update the buffer string tables. */
  {
    SCM buffer_str;

    buffer_str = scm_hashq_ref (scm_buffer_strings, SCM_MAKINUM (fd), SCM_BOOL_F);
    scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (nfd), buffer_str);
    scm_hashq_set_x (scm_buffer_strings, SCM_MAKINUM (fd), SCM_BOOL_F);
  }

  /* Update the reader and printer tables. */
  scm_swap_read_records (fd, nfd);

  answer = oldfd;

 return_answer:
  SCM_ALLOW_INTS;

  return answer;
}

/*(c %fstat)
 * (%fstat fd)
 * 
 * Return statistics about a file.
 *
 * This function returns a vector.  The function
 * `statbuf-ref' in the module `(unix structures)' can
 * be used to access statistics by name.
 */
SCM_PROC (s_sys_fstat, "%fstat", 1, 0, 0, scm_sys_fstat);
SCM 
scm_sys_fstat (SCM fd)
{
  SCM_INTS_ENABLED;
  int d;
  int rv;
  struct stat stat_temp;
  SCM ans;
  int errn;

  SCM_ASSERT (SCM_INUMP (fd) || (!SCM_IS_IMMEDIATE (fd) && SCM_FDP (fd)), fd, scm_arg1, s_sys_fstat);

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd))
    d = SCM_INUM (fd);
  else
    d = SCM_FD (fd);

  rv = vu_fstat (&errn, d, &stat_temp);
  ans = rv ? scm_makerrno (errn) : scm_stat2scm (&stat_temp);
  SCM_ALLOW_INTS;

  return ans;
}


static void
scm_to_flock (struct flock * answer, SCM arg, SCM msg, SCM proc)
{
  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg) && SCM_CONSP (arg), arg, msg, proc);
  answer->l_type = scm_integer_cpp_constant (s_flock_type, SCM_CAR (arg), msg, proc);
  arg = SCM_CDR (arg);

  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg) && SCM_CONSP (arg), arg, msg, proc);
  answer->l_whence = scm_integer_cpp_constant (s_lseek_flag, SCM_CAR (arg), msg, proc);
  arg = SCM_CDR (arg);

  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg) && SCM_CONSP (arg), arg, msg, proc);
  answer->l_start = scm_num2long (SCM_CAR (arg), msg, proc);
  arg = SCM_CDR (arg);

  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg) && SCM_CONSP (arg), arg, msg, proc);
  answer->l_len = scm_num2long (SCM_CAR (arg), msg, proc);
}


static SCM
scm_flock_to_scm (struct flock * fl)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = SCM_EOL;

  answer = scm_cons (scm_long2num (fl->l_len), answer);
  answer = scm_cons (scm_long2num (fl->l_start), answer);

  if (fl->l_type == SEEK_CUR)
    answer = scm_cons (s_SEEK_CUR, answer);
  else if (fl->l_type == SEEK_SET)
    answer = scm_cons (s_SEEK_SET, answer);
  else if (fl->l_type == SEEK_END)
    answer = scm_cons (s_SEEK_END, answer);
  else
    answer = scm_cons (SCM_MAKINUM (fl->l_type), answer);

  if (fl->l_type == F_RDLCK)
    answer = scm_cons (s_F_RDLCK, answer);
  else if (fl->l_type == F_WRLCK)
    answer = scm_cons (s_F_WRLCK, answer);
  else if (fl->l_type == F_UNLCK)
    answer = scm_cons (s_F_UNLCK, answer);
  else
    answer = scm_cons (SCM_MAKINUM (fl->l_type), answer);

  answer = scm_cons (scm_long2num ((long)fl->l_pid), answer);
  return answer;
}


/*(c %fcntl)
 * (%fcntl fd cmd :optional arg)
 * 
 * `fcntl' is pretty random.
 */
SCM_PROC (s_sys_fcntl, "%fcntl", 2, 1, 0, scm_sys_fcntl);
SCM
scm_sys_fcntl (SCM sfd, SCM scmd, SCM sarg)
{
  SCM_INTS_ENABLED;
  int fd;
  int errn;
  int cmd;
  long arg;
  int rv;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), fd, scm_arg1, s_sys_fcntl);
  cmd = scm_integer_cpp_constant (s_fcntl_cmd, scmd, scm_arg2, s_sys_fcntl);

  switch (cmd)
    {
    default:
      SCM_ALLOW_INTS;
      scm_wta (scmd, scm_arg2, s_sys_fcntl);
      return SCM_BOOL_F;

    case F_DUPFD:
      if (SCM_UNDEFINED ==  sarg)
	{
	wrong_number_of_arguments:
	  SCM_ALLOW_INTS;
	  scm_wta (s_sys_fcntl, scm_wna, s_sys_fcntl);
	}

      SCM_ASSERT (SCM_INUMP (sarg) || (!SCM_IS_IMMEDIATE (sarg) && SCM_FDP (sarg)), sarg, scm_arg3, s_sys_fcntl);

      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      if (SCM_INUMP (sarg))
	arg = SCM_INUM (sarg);
      else
	arg = SCM_FD (sarg);

      rv = vu_fcntl (&errn, fd, cmd, arg);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else if (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd))
	answer = scm_makefd (rv, SCM_FD_FLAGS (sfd));
      else if (!SCM_IS_IMMEDIATE (sarg) && SCM_FDP (sarg))
	answer = sarg;
      else
	answer = SCM_MAKINUM (rv);
      SCM_ALLOW_INTS;
      return rv;
	
    case F_GETFD:
      if (SCM_UNDEFINED != sarg)
	goto wrong_number_of_arguments;
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, 0);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	{
	  answer = SCM_EOL;
	  if (answer & FD_CLOEXEC)
	    answer = scm_cons (s_FD_CLOEXEC, answer);
	}
      SCM_ALLOW_INTS;
      return answer;

    case F_SETFD:
      if (SCM_UNDEFINED == sarg)
	goto wrong_number_of_arguments;
      arg = scm_integer_logior_cpp_constants (s_F_SETFD_flag, sarg, scm_arg3, s_sys_fcntl);
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, arg);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	answer = SCM_BOOL_T;
      SCM_ALLOW_INTS;
      return answer;


    case F_GETFL:
      if (SCM_UNDEFINED != sarg)
	goto wrong_number_of_arguments;
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, arg);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	{
	  answer = SCM_EOL;
	  if (rv & O_APPEND)
	    answer = scm_cons (s_O_APPEND, answer);
	  if (rv & O_NONBLOCK)
	    answer = scm_cons (s_O_NONBLOCK, answer);
	  if (rv & O_RDONLY)
	    answer = scm_cons (s_O_RDONLY, answer);
	  if (rv & O_RDWR)
	    answer = scm_cons (s_O_RDWR, answer);
	  if (rv & O_WRONLY)
	    answer = scm_cons (s_O_WRONLY, answer);
	}
      SCM_ALLOW_INTS;
      return answer;


    case F_SETFL:
      if (SCM_UNDEFINED == sarg)
	goto wrong_number_of_arguments;
      arg = scm_integer_logior_cpp_constants (s_open_flag, sarg, scm_arg3, s_sys_fcntl);
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, arg);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	answer = SCM_BOOL_T;
      SCM_ALLOW_INTS;
      return answer;


    case F_GETOWN:
      if (SCM_UNDEFINED != sarg)
	goto wrong_number_of_arguments;
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, 0);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	answer = scm_long2num (rv);
      SCM_ALLOW_INTS;
      return answer;


    case F_SETOWN:
      if (SCM_UNDEFINED == sarg)
	goto wrong_number_of_arguments;
      arg = scm_num2long (sarg, scm_arg3, s_sys_fcntl);
      SCM_DEFER_INTS;
      if (SCM_INUMP (sfd))
	fd = SCM_INUM (sfd);
      else
	fd = SCM_FD (sfd);
      
      rv = vu_fcntl (&errn, fd, cmd, arg);
      if (rv < 0)
	answer = scm_makerrno (errn);
      else
	answer = SCM_BOOL_T;
      SCM_ALLOW_INTS;
      return answer;


    case F_GETLK:
    case F_SETLK:
    case F_SETLKW:
      {
	struct flock fl;
	
	if (SCM_UNDEFINED == sarg)
	  goto wrong_number_of_arguments;
	scm_to_flock (&fl, sarg, scm_arg3, s_sys_fcntl);
	arg = (long)&fl;
	SCM_DEFER_INTS;
	if (SCM_INUMP (sfd))
	  fd = SCM_INUM (sfd);
	else
	  fd = SCM_FD (sfd);
	
	rv = vu_fcntl (&errn, fd, cmd, arg);
	if (rv < 0)
	  answer = scm_makerrno (errn);
	else
	  answer = SCM_BOOL_T;
	SCM_ALLOW_INTS;
	if ((cmd == F_GETLK) && (answer == SCM_BOOL_T))
	  answer = scm_flock_to_scm (&fl);
	return answer;
      }
    }
}



/*(c %isatty?)
 * (%isatty? fd)
 * 
 * Return #t if `fd' is a descriptor for a tty device.
 * See the manual page "isatty".
 * 
 * `fd' must be a file descriptor object or integer.
 */
SCM_PROC (s_sys_isatty_p, "%isatty?", 1, 0, 0, scm_sys_isatty_p);
SCM 
scm_sys_isatty_p (SCM fd)
{
  SCM_INTS_ENABLED;

  int d;
  SCM ans;

  SCM_ASSERT (SCM_INUMP (fd) || (!SCM_IS_IMMEDIATE (fd) && SCM_FDP (fd)), fd, scm_arg1, s_sys_isatty_p);

  SCM_DEFER_INTS;
  if (SCM_INUMP (fd))
    d = SCM_INUM (fd);
  else
    d = SCM_FD (fd);
  if (d < 0)
    ans = scm_makerrno (errno);
  else
    ans = (isatty (d) ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_ALLOW_INTS;

  return ans;
}


/*(c %ttyname)
 * (%ttyname descriptor)
 * 
 * Return the name of the terminal device of the indicated file.
 * `descriptor' may be an integer file descriptor, a file descriptor
 * object, or a port.
 *
 * See the manual page "ttyname".
 */
SCM_PROC (s_ttyname, "%ttyname", 1, 0, 0, scm_sys_ttyname);
SCM 
scm_sys_ttyname (SCM port)
{
  SCM_INTS_ENABLED;
  int fd;
  char *ans;
  SCM sans;

  SCM_ASSERT (SCM_INUMP (port) || (!SCM_IS_IMMEDIATE (port) && SCM_FDP (port)), port, scm_arg1, s_ttyname);

  SCM_DEFER_INTS;
  {
    if (SCM_INUMP (port))
      fd = SCM_INUM (port);
    else
      fd = SCM_FD  (port);
    if (fd != -1)
      ans = ttyname (fd);
    sans = (((fd != -1) && ans)
	    ? scm_makfromstr0 (ans)
	    : scm_makerrno (errno));
  }
  SCM_ALLOW_INTS;
  return sans;
}


/*(c %ctermid)
 * (%ctermid)
 * 
 * Return the name of the device of the controlling terminal.
 * See the manual page "ctermid".
 */
SCM_PROC (s_ctermid, "%ctermid", 0, 0, 0, scm_sys_ctermid);
SCM 
scm_sys_ctermid (void)
{
  SCM_INTS_ENABLED;
  char * result;
  SCM answer;

  SCM_DEFER_INTS;
  result = ctermid (NULL);
  answer = *result == '\0' ? scm_makerrno (errno) : scm_makfromstr0 (result);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %tcgetpgrp)
 * (%tcgetpgrp descriptor)
 * 
 * Return the process group id of the foreground process of
 * the indicated device.
 *
 * `descriptor' may be an integer file descriptor, a file descriptor
 * object, or a port.
 *
 * See the manual page "tcgetpgrp".
 */
SCM_PROC (s_tcgetpgrp, "%tcgetpgrp", 1, 0, 0, scm_sys_tcgetpgrp);
SCM 
scm_sys_tcgetpgrp (SCM port)
{
  SCM_INTS_ENABLED;
  int fd;
  pid_t pgid;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (port) || (!SCM_IS_IMMEDIATE (port) && SCM_FDP (port)), port, scm_arg1, s_tcgetpgrp);

  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else 
    fd = SCM_FD  (port);

  if (fd == -1 || (pgid = tcgetpgrp (fd)) == -1)
    answer = scm_makerrno (errno);
  else
    answer = SCM_MAKINUM (pgid);
  SCM_ALLOW_INTS;

  return answer;
}    


/*(c %tcsetpgrp)
 * (%tcsetpgrp descriptor process-group-id)
 * 
 * Set the foreground process group id of the indicated device.
 * 
 * `descriptor' may be an integer file descriptor, a file descriptor
 * object, or a port.
 *
 * `process-group-id' must be an integer.
 *
 * See the manual page "tcsetpgrp".
 */
SCM_PROC (s_tcsetpgrp, "%tcsetpgrp", 2, 0, 0, scm_sys_tcsetpgrp);
SCM 
scm_sys_tcsetpgrp (SCM port, SCM pgid)
{
  SCM_INTS_ENABLED;
  SCM answer;
  int fd;

  SCM_ASSERT (SCM_INUMP (port) || (!SCM_IS_IMMEDIATE (port) && SCM_FDP (port)), port, scm_arg1, s_tcsetpgrp);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, scm_arg2, s_tcsetpgrp);

  SCM_DEFER_INTS;
  if (SCM_INUMP (port))
    fd = SCM_INUM (port);
  else
    fd = SCM_FD  (port);
  if (fd == -1 || tcsetpgrp (fd, SCM_INUM (pgid)) == -1)
    answer = scm_makerrno (errno);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return answer;
}    



/****************************************************************
 *(h1 "File Statistics")
 */

/* scm_stat2scm
 * 
 * Create a vector containing values which represent
 * a stat buffer.
 */
SCM 
scm_stat2scm (struct stat *stat_temp)
{
  SCM_INTS_ENABLED;
  SCM ans;

  ans = scm_list_append_x (scm_listify (scm_mode2scm (stat_temp->st_mode), 
					scm_listify (kw_dev, scm_ulong2num ((unsigned long) stat_temp->st_dev),
						     kw_ino, scm_ulong2num ((unsigned long) stat_temp->st_ino),
						     kw_nlink, scm_ulong2num ((unsigned long) stat_temp->st_nlink),
						     kw_uid, scm_ulong2num ((unsigned long) stat_temp->st_uid),
						     kw_gid, scm_ulong2num ((unsigned long) stat_temp->st_gid),
						     kw_size, scm_ulong2num ((unsigned long) stat_temp->st_size),
						     kw_atime, scm_ulong2num ((unsigned long) stat_temp->st_atime),
						     kw_mtime, scm_ulong2num ((unsigned long) stat_temp->st_mtime),
						     kw_ctime, scm_ulong2num ((unsigned long) stat_temp->st_ctime),
						     SCM_UNDEFINED),
					SCM_UNDEFINED));
  return ans;
}


/*(c %stat)
 * (%stat path)
 * 
 * Return statistics about a file.
 * See the manual page "stat".
 *
 * `path' must be a read-only string.
 *
 * This function returns a list of keywords and arguments to 
 * keywords.  E.g.:
 * 
 * 	(%stat "/etc/passwd")
 *	=>
 *	(:permissions (S_IROTH S_IRGRP S_IWUSR S_IRUSR)
 *	 :permission-bits 420
 *	 :type S_IFREG
 *	 :dev 131072
 *	 :ino 378
 *	 :nlink 1
 *	 :uid 0
 *	 :gid 0
 *	 :size 1010
 *	 :atime 995807934
 *	 :mtime 963714308
 *	 :ctime 963714308)
 * 
 */
SCM_PROC (s_sys_stat, "%stat", 1, 0, 0, scm_sys_stat);
SCM 
scm_sys_stat (SCM path)
{
  SCM_INTS_ENABLED;
  int rv;
  struct stat stat_temp;
  SCM ans;
  int errn;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_stat);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  rv = vu_stat (&errn, SCM_RO_CHARS (path), &stat_temp);
  ans = rv ? scm_makerrno (errn) : scm_stat2scm (&stat_temp);
  SCM_ALLOW_INTS;
  return ans;
}


/*(c file-exists?)
 * (file-exists? f)
 * 
 * Using `%stat', return `#t' if the named file exits.
 * 
 * This function returns `#f' if the file exists as a symbolic
 * link, but the link points to a non-existent file.
 */

/*(c file-is-directory?)
 * (file-is-directory? filename)
 * 
 * Using `%stat', return `#t' if `filename' is the name of an existing
 * directory.
 */



/*(c %utime)
 * (%utime pathname :optional accesstime modtime)
 * 
 * Set the access and modification times for a file.
 * `accesstime' and `modtime' should be integers or SCM_BOOL_F, 
 * if provided.  If either is ommitted, the current time is
 * used.
 *
 * See the manual page "utime".
 */
SCM_PROC (s_sys_utime, "%utime", 1, 2, 0, scm_sys_utime);
SCM 
scm_sys_utime (SCM pathname, SCM actime, SCM modtime)
{
  SCM_INTS_ENABLED;
  int rv;
  struct utimbuf utm_tmp;
  SCM answer;

  SCM_ASSERT (scm_is_ro_string (pathname), pathname, scm_arg1, s_sys_utime);

  if (scm_is_ro_substr (pathname))
    pathname = scm_makfromstr (SCM_RO_CHARS (pathname), SCM_RO_LENGTH (pathname));

  if (SCM_UNBNDP (actime) || (actime == SCM_BOOL_F))
    {
      SCM_DEFER_INTS;
      time (&utm_tmp.actime);
      SCM_ALLOW_INTS;
    }
  else
    utm_tmp.actime = scm_num2ulong (actime, scm_arg2, s_sys_utime);

  if (SCM_UNBNDP (modtime) || (modtime == SCM_BOOL_F))
    {
      SCM_DEFER_INTS;
      time (&utm_tmp.modtime);
      SCM_ALLOW_INTS;
    }
  else
    utm_tmp.modtime = scm_num2ulong (modtime, scm_arg3, s_sys_utime);

  SCM_DEFER_INTS;
  rv = utime (SCM_RO_CHARS (pathname), &utm_tmp);
  answer = rv ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}



/****************************************************************
 *(h1 "Modifying Directories")
 */

/*(c %link)
 * (%link oldpath newpath)
 * 
 * Create a new link to an existing file.
 * See the manual page "link".
 *
 * `oldpath' and `newpath' must be read-only strings.
 */
SCM_PROC (s_sys_link, "%link", 2, 0, 0, scm_sys_link);
SCM 
scm_sys_link (SCM oldpath, SCM newpath)
{
  SCM_INTS_ENABLED;
  int val;
  SCM ans;
  int errn;

  SCM_ASSERT (scm_is_ro_string (oldpath), oldpath, scm_arg1, s_sys_link);
  if (scm_is_ro_substr (oldpath))
    oldpath = scm_makfromstr (SCM_RO_CHARS (oldpath), SCM_RO_LENGTH (oldpath));

  SCM_ASSERT (scm_is_ro_string (newpath), newpath, scm_arg2, s_sys_link);
  if (scm_is_ro_substr (newpath))
    newpath = scm_makfromstr (SCM_RO_CHARS (newpath), SCM_RO_LENGTH (newpath));

  SCM_DEFER_INTS;
  val = vu_link (&errn, SCM_RO_CHARS (oldpath), SCM_RO_CHARS (newpath));
  ans = val ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c %unlink)
 * (%unlink path)
 * 
 * Remove a link to a file.
 * See the manual page "unlink".
 *
 * `path' must be a read-only string.
 */
SCM_PROC (s_sys_unlink, "%unlink", 1, 0, 0, scm_sys_unlink);
SCM 
scm_sys_unlink (SCM path)
{
  SCM_INTS_ENABLED;
  int val;
  SCM ans;
  int errn;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_unlink);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  val = vu_unlink (&errn, SCM_RO_CHARS (path));
  ans = val ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c %rename)
 * (%rename oldpath newpath)
 * 
 * Rename a file.
 * See the manual page "rename".
 *
 * `oldpath' and `newpath' must be read-only strings.
 */
SCM_PROC (s_sys_rename, "%rename", 2, 0, 0, scm_sys_rename);
SCM 
scm_sys_rename (SCM oldname, SCM newname)
{
  SCM_INTS_ENABLED;
  int rv;
  SCM ans;
  int errn;

  SCM_ASSERT (scm_is_ro_string (oldname), oldname, scm_arg1, s_sys_rename);
  SCM_ASSERT (scm_is_ro_string (newname), newname, scm_arg2, s_sys_rename);

  if (scm_is_ro_substr (oldname))
    oldname = scm_makfromstr (SCM_RO_CHARS (oldname), SCM_RO_LENGTH (oldname));

  if (scm_is_ro_substr (newname))
    newname = scm_makfromstr (SCM_RO_CHARS (newname), SCM_RO_LENGTH (newname));

  SCM_DEFER_INTS;
  rv = vu_rename (&errn, SCM_RO_CHARS (oldname), SCM_RO_CHARS (newname));
  ans = rv ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c %mkdir)
 * (%mkdir path :optional mode)
 * 
 * Create a new directory.
 * See the manual page "mkdir".
 *
 * `path' must be a read-only string.
 * `mode', if provided, must be an integer.
 * 
 * If `mode' is not provided, the mode #o777 is used.
 */
SCM_PROC (s_sys_mkdir, "%mkdir", 1, 1, 0, scm_sys_mkdir);
SCM 
scm_sys_mkdir (SCM path, SCM smode)
{
  SCM_INTS_ENABLED;
  int rv;
  int mode;
  SCM ans;
  int errn;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_mkdir);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  if (SCM_UNBNDP (smode))
    mode = 0777;
  else
    mode = scm_integer_logior_cpp_constants (s_file_mode, smode, scm_arg2, s_sys_mkdir);

  SCM_DEFER_INTS;
  rv = vu_mkdir (&errn, SCM_RO_CHARS (path), mode);
  ans = rv ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return ans;
}


/*(c %rmdir)
 * (%rmdir path)
 * 
 * Remove a directory.
 * See the manual page "rmdir".
 *
 * `path' must be a read-only string.
 */
SCM_PROC (s_sys_rmdir, "%rmdir", 1, 0, 0, scm_sys_rmdir);
SCM 
scm_sys_rmdir (SCM path)
{
  SCM_INTS_ENABLED;
  int val;
  int errn;
  SCM ans;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_rmdir);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  val = vu_rmdir (&errn, SCM_RO_CHARS (path));
  ans = val ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return ans;
}


/****************************************************************
 * Directory Objects
 */

#define SCM_OPN		(1L<<16) /* Is the port open? */
#define SCM_OPENP(x) (SCM_OPN & SCM_CAR(x))
#define SCM_CLOSEDP(x) (!SCM_OPENP(x))

static int 
scm_dir_print (SCM exp, SCM port, int writing __attribute__((unused)))
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<directory ");
  scm_intprint (exp, 16, port);
  scm_port_putc (&errn, port, '>');
  return 1;
}

static size_t 
scm_dir_free (SCM p)
{
  SCM_INTS_UNKNOWN;
  int errn;

  if (SCM_OPENP (p))
    vu_closedir (&errn, (DIR *) SCM_CDR (p));
  return 0;
}

long scm_tc16_dir;
static scm_small_object_functions dir_smob = {scm_mark0, scm_dir_free, scm_dir_print, 0};


/****************************************************************
 *(h1 "Examining Directories")
 */

/*(c %opendir)
 * (%opendir path)
 * 
 * Open a directory.
 * See the manual page "opendir".
 * 
 * `path' must be a read-only string.
 */
SCM_PROC (s_sys_opendir, "%opendir", 1, 0, 0, scm_sys_opendir);
SCM 
scm_sys_opendir (SCM dirname)
{
  SCM_INTS_ENABLED;
  DIR *ds;
  int errn;
  int got;
  SCM dir;

  SCM_ASSERT (scm_is_ro_string (dirname), dirname, scm_arg1, s_sys_opendir);
  if (scm_is_ro_substr (dirname))
    dirname = scm_makfromstr (SCM_RO_CHARS (dirname), SCM_RO_LENGTH (dirname));
  SCM_NEWCELL (dir);

  SCM_DEFER_INTS;
  got = vu_opendir (&errn, &ds, SCM_RO_CHARS (dirname));
  if (got < 0)
    dir = scm_makerrno (errn);
  else
    {
      SCM_CAR (dir) = scm_tc16_dir | SCM_OPN;
      SCM_CDR (dir) = (SCM)ds;
    }
  SCM_ALLOW_INTS;
  return dir;
}


/*(c %readdirname)
 * (%readdirname directory)
 * 
 * Return the next file name from a directory.
 *
 * `directory' must be a directory object (see `%opendir').
 */
SCM_PROC (s_sys_readdirname, "%readdirname", 1, 0, 0, scm_sys_readdirname);
SCM 
scm_sys_readdirname (SCM port)
{
  SCM_INTS_ENABLED;
  int errn;
  int got;
  char * name;
  SCM ans;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (port) && SCM_OPDIRP (port), port, scm_arg1, s_sys_readdirname);

  SCM_DEFER_INTS;
  errn = 0;
  got = vu_readdir (&errn, 0, &name, (DIR *) SCM_CDR (port));
  if (0 > got)
    {
      if (errn)
	ans = scm_makerrno (errn);
      else
	ans = SCM_BOOL_F;
    }
  else
    {
      ans = scm_take_str0 (name);
    }
  SCM_ALLOW_INTS;
  return ans;
}



#if 0
/*s
 * %rewinddir directory
 * 
 * Reposition a directory stream to the beginning.
 * See the manual page "rewinddir".
 *
 * `directory' must be a directory object (see `%opendir').
 */
SCM_PROC (s_sys_rewinddir, "%rewinddir", 1, 0, 0, scm_sys_rewinddir);
SCM 
scm_sys_rewinddir (SCM port)
{
  SCM_INTS_ENABLED;
  int errn;
  SCM ans;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (port) && SCM_OPDIRP (port), port, scm_arg1, s_sys_rewinddir);

  SCM_DEFER_INTS;
  if (0 > vu_rewinddir (&errn, (DIR *) SCM_CDR (port)))
    ans = scm_makerrno (errn);
  else
    ans = SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return ans;
}
#endif

/*(c %closedir)
 * (%closedir directory)
 * 
 * Close a directory stream.
 * See the manual page "closedir".
 *
 * `directory' must be a directory object (see `%opendir').
 */
SCM_PROC (s_sys_closedir, "%closedir", 1, 0, 0, scm_sys_closedir);
SCM 
scm_sys_closedir (SCM port)
{
  SCM_INTS_ENABLED;
  int errn;
  int sts;
  SCM ans;


  SCM_ASSERT (!SCM_IS_IMMEDIATE (port) && SCM_DIRP (port) && !SCM_CLOSEDP (port),
	      port, scm_arg1, s_sys_closedir);

  SCM_DEFER_INTS;
  sts = vu_closedir (&errn, (DIR *) SCM_CDR (port));
  if (sts)
    ans = scm_makerrno (errno);
  else
    ans = SCM_BOOL_T;
  SCM_CAR (port) = scm_tc16_dir;
  SCM_ALLOW_INTS;

  return SCM_BOOL_T;
}



/****************************************************************
 *(h1 "The Current Directory")
 */


/*(c %chdir)
 * (%chdir path)
 * 
 * Change the current working directory.
 * See the manual page "chdir".
 *
 * `path' must be a read-only string.
 */
SCM_PROC (s_sys_chdir, "%chdir", 1, 0, 0, scm_sys_chdir);
SCM 
scm_sys_chdir (SCM str)
{
  SCM_INTS_ENABLED;
  int ans;
  int errn;
  SCM sans;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_sys_chdir);
  if (scm_is_ro_substr (str))
    str = scm_makfromstr (SCM_RO_CHARS (str), SCM_RO_LENGTH (str));
  SCM_DEFER_INTS;
  ans = vu_chdir (&errn, SCM_RO_CHARS (str));
  sans = ans ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return sans;
}


/*(c %fchdir)
 * (%fchdir fd)
 * 
 * Change directory to the directory open as descriptor `fd'.
 * See the manual page "fchdir".
 */
SCM_PROC (s_sys_fchdir, "%fchdir", 1, 0, 0, scm_sys_fchdir);
SCM 
scm_sys_fchdir (SCM sfd)
{
  SCM_INTS_ENABLED;
  int fd;
  int ans;
  int errn;
  SCM sans;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_fchdir);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  ans = vu_fchdir (&errn, fd);
  sans = ans ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;

  return sans;
}


/*(c %getcwd)
 * (%getcwd)
 * 
 * Return the current working directory.
 * See the manual page "getcwd".
 */
SCM_PROC (s_sys_getcwd, "%getcwd", 0, 0, 0, scm_sys_getcwd);
SCM 
scm_sys_getcwd (void)
{
  SCM_INTS_ENABLED;
  char *rv;
  char wd[PATH_MAX];
  SCM result;

  SCM_DEFER_INTS;
  rv = getcwd (wd, PATH_MAX);
  if (rv != 0)
    result = scm_makfromstr0 (wd);
  else
    result = scm_makerrno (errno);
  SCM_ALLOW_INTS;
  return result;
}


/****************************************************************
 *(h1 "The Interface to select")
 */


/* fill_select_type
 * 
 * Given a list of file descriptors (integers or descriptor objects)
 * fill a select bitset from that list.
 */
static void
fill_select_type (SELECT_TYPE * set, SCM list)
{
  SCM_INTS_DISABLED;

  while (list != SCM_EOL)
    {
      if (scm_is_port (SCM_CAR (list)))
	{
	  int fd;
	  fd = scm_fileno (SCM_CAR (list));
	  if (fd != -1)
	    FD_SET (fd, set);
	}
      else if (SCM_INUMP (SCM_CAR (list)))
	FD_SET (SCM_INUM (SCM_CAR (list)), set);
      else if (!SCM_IS_IMMEDIATE (SCM_CAR (list)) && SCM_FDP (SCM_CAR (list)))
	FD_SET (SCM_FD (SCM_CAR (list)), set);
      list = SCM_CDR (list);
    }
}


/* retrieve_select_type
 * 
 * Given a list of file descriptors (integers or descriptor objects),
 * return a list of the elements of that list which are members
 * of a select bitset.
 */
static SCM 
retrieve_select_type (SELECT_TYPE * set, SCM list)
{
  SCM_INTS_DISABLED;
  SCM answer;

  answer = SCM_EOL;
  while (list != SCM_EOL)
    {
      if (scm_is_port (SCM_CAR (list)))
	{
	  int fd;
	  fd = scm_fileno (SCM_CAR (list));
	  if (   (fd != -1)
	      && FD_ISSET (fd, set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      else if (SCM_INUMP (SCM_CAR (list)))
	{
	  if (FD_ISSET (SCM_INUM (SCM_CAR (list)), set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      else if (!SCM_IS_IMMEDIATE (SCM_CAR (list)) && SCM_FDP (SCM_CAR (list)))
	{
	  if (FD_ISSET (SCM_FD (SCM_CAR (list)), set))
	    answer = scm_cons (SCM_CAR (list), answer);
	}
      list = SCM_CDR (list);
    }
  answer = scm_list_reverse_x (answer, SCM_EOL);
  return answer;
}


/*(c %select)
 * (%select reads writes exceptions :optional seconds milliseconds)
 * 
 * Return a list of three lists: descriptors ready to be read,
 * descriptors ready to be written, and descriptors in exceptional
 * states.
 *
 * See the manual page "select".
 *
 * `reads', `writes', and `exceptions' must be lists of descriptor
 * objects or integers.
 *
 * `seconds' and `milliseconds', if provided, must be integers.
 * If not provided, those parameters default to 0.
 */
SCM_PROC (s_sys_select, "%select", 3, 2, 0, scm_sys_select);
SCM
scm_sys_select (SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs)
{
  SCM_INTS_ENABLED;
  SCM errobj;
  SCM errpos;
  int type_error = 1;
  struct timeval timeout;
  struct timeval * time_p;
  SELECT_TYPE read_set;
  SELECT_TYPE write_set;
  SELECT_TYPE except_set;
  int sreturn;
  SCM ans;

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  SCM_DEFER_INTS;
  type_error = 0;
  if (scm_ilength (reads) < 0)
    {
      errobj = reads;
      errpos = scm_arg1;
      type_error = 1;
    }
  else if (scm_ilength (writes) < 0)
    {
      errobj = writes;
      errpos = scm_arg2;
      type_error = 1;
    }
  else if (scm_ilength (excepts) < 0)
    {
      errobj = excepts;
      errpos = scm_arg3;
      type_error = 1;
    }
  else
    {
      fill_select_type (&read_set, reads);
      fill_select_type (&write_set, writes);
      fill_select_type (&except_set, excepts);
    }
  SCM_ALLOW_INTS;

  if (type_error)
    SCM_ASSERT (0, errobj, errpos, s_sys_select);

  if (SCM_UNBNDP (secs))
    time_p = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (secs), secs, scm_arg4, s_sys_select);
      if (SCM_UNBNDP (msecs))
	msecs = SCM_INUM0;
      else
	SCM_ASSERT (SCM_INUMP (msecs), msecs, scm_arg5, s_sys_select);

      timeout.tv_sec = SCM_INUM (secs);
      timeout.tv_usec = 1000 * SCM_INUM (msecs);
      time_p = &timeout;
    }

  SCM_DEFER_INTS;
  sreturn = select (SELECT_SET_SIZE,
		    &read_set, &write_set, &except_set, time_p);
  if (sreturn < 0)
    ans = scm_makerrno (errno);
  else
    ans = scm_listify (retrieve_select_type (&read_set, reads),
		       retrieve_select_type (&write_set, writes),
		       retrieve_select_type (&except_set, excepts),
		       SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return ans;
}


/****************************************************************
 *(h1 "Symbolic Links")
 */

/*(c %symlink)
 * (%symlink oldpath newpath)
 * 
 * Create a symbolic link.
 * See the manual page "symlink".
 *
 * `oldpath' and `newpath' must be read-only strings.
 */
SCM_PROC (s_sys_symlink, "%symlink", 2, 0, 0, scm_sys_symlink);
SCM
scm_sys_symlink(SCM oldpath, SCM newpath)
{
  SCM_INTS_ENABLED;
  int val;
  int errn;
  SCM ans;

  SCM_ASSERT(scm_is_ro_string(oldpath), oldpath, scm_arg1, s_sys_symlink);
  SCM_ASSERT(scm_is_ro_string(newpath), newpath, scm_arg2, s_sys_symlink);

  if (scm_is_ro_substr (oldpath))
    oldpath = scm_makfromstr (SCM_RO_CHARS (oldpath), SCM_RO_LENGTH (oldpath));

  SCM_DEFER_INTS;
  val = vu_symlink (&errn, SCM_RO_CHARS(oldpath), SCM_RO_CHARS(newpath));
  ans = val ? scm_makerrno (errn) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c %readlink)
 * (%readlink path)
 * 
 * Return the contents of a symbolic link.
 * See the manual page "readlink".
 * 
 * `path' must be a read-only string.
 */
SCM_PROC (s_sys_readlink, "%readlink", 1, 0, 0, scm_sys_readlink);
SCM
scm_sys_readlink(SCM path)
{
  SCM_INTS_ENABLED;
  int rv;
  int errn;
  char buf[PATH_MAX];
  SCM result;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_readlink);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));

  SCM_DEFER_INTS;
  rv = vu_readlink (&errn, SCM_RO_CHARS (path), buf, (size_t) PATH_MAX);
  if (rv != -1)
    result = scm_makfromstr (buf, rv);
  else
    result = scm_makerrno (errn);
  SCM_ALLOW_INTS;

  return result;
}


/*(c %lstat)
 * (%lstat path)
 * 
 * Return file statistics for a symbolic link.
 * See the manual page "lstat".
 * 
 * `path' must be a read-only string.
 *
 * This function returns a vector.  The function
 * `statbuf-ref' in the module `(unix structures)' can
 * be used to access statistics by name.
 */
SCM_PROC (s_sys_lstat, "%lstat", 1, 0, 0, scm_sys_lstat);
SCM
scm_sys_lstat(SCM str)
{
  SCM_INTS_ENABLED;
  int i;
  int errn;
  struct stat stat_temp;
  SCM ans;

  SCM_ASSERT(scm_is_ro_string(str), str, scm_arg1, s_sys_lstat);
  if (scm_is_ro_substr (str))
    str = scm_makfromstr (SCM_RO_CHARS (str), SCM_RO_LENGTH (str));

  SCM_DEFER_INTS;
  i = vu_lstat (&errn, SCM_RO_CHARS(str), &stat_temp);
  ans = i ? scm_makerrno (errn) :  scm_stat2scm(&stat_temp);
  SCM_ALLOW_INTS;

  return ans;
}



void
scm_init_filesys (void)
{
  SCM_INTS_DISABLED;


  scm_tc16_fd = scm_newsmob (&fd_smob);
  scm_tc16_dir = scm_newsmob (&dir_smob);
#include "systas/libsystas/filesys.x"


  /****************************************************************
   * Flags to lseek
   */
  scm_declare_integer_cpp_constant (s_lseek_flag, "SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_declare_integer_cpp_constant (s_lseek_flag, "SEEK_END", SCM_MAKINUM (SEEK_END));
  scm_declare_integer_cpp_constant (s_lseek_flag, "SEEK_SET", SCM_MAKINUM (SEEK_SET));

  /****************************************************************
   * Flags to open(2)
   */
#ifdef O_CREAT
  scm_declare_integer_cpp_constant (s_open_flag, "O_CREAT", SCM_MAKINUM (O_CREAT));
#endif 
#ifdef O_EXCL
  scm_declare_integer_cpp_constant (s_open_flag, "O_EXCL", SCM_MAKINUM (O_EXCL));
#endif 
#ifdef O_NOCTTY
  scm_declare_integer_cpp_constant (s_open_flag, "O_NOCTTY", SCM_MAKINUM (O_NOCTTY));
#endif 
#ifdef O_TRUNC
  scm_declare_integer_cpp_constant (s_open_flag, "O_TRUNC", SCM_MAKINUM (O_TRUNC));
#endif 
#ifdef O_WRONLY
  scm_declare_integer_cpp_constant (s_open_flag, "O_WRONLY", SCM_MAKINUM (O_WRONLY));
#endif 
#ifdef O_RDWR
  scm_declare_integer_cpp_constant (s_open_flag, "O_RDWR", SCM_MAKINUM (O_RDWR));
#endif 
#ifdef O_RDONLY
  scm_declare_integer_cpp_constant (s_open_flag, "O_RDONLY", SCM_MAKINUM (O_RDONLY));
#endif 
#ifdef O_APPEND
  scm_declare_integer_cpp_constant (s_open_flag, "O_APPEND", SCM_MAKINUM (O_APPEND));
#endif 
#ifdef O_NONBLOCK
  scm_declare_integer_cpp_constant (s_open_flag, "O_NONBLOCK", SCM_MAKINUM (O_NONBLOCK));
#endif 
#ifdef O_NDELAY
  scm_declare_integer_cpp_constant (s_open_flag, "O_NDELAY", SCM_MAKINUM (O_NDELAY));
#endif 
#ifdef O_ASYNC
  scm_declare_integer_cpp_constant (s_open_flag, "O_ASYNC", SCM_MAKINUM (O_ASYNC));
#endif 


  /****************************************************************
   * File type/permission bits.
   */
#ifdef S_IRUSR
  scm_declare_integer_cpp_constant (s_file_mode, "S_IRUSR", SCM_MAKINUM (S_IRUSR));
#endif
#ifdef S_IWUSR
  scm_declare_integer_cpp_constant (s_file_mode, "S_IWUSR", SCM_MAKINUM (S_IWUSR));
#endif
#ifdef S_IXUSR
  scm_declare_integer_cpp_constant (s_file_mode, "S_IXUSR", SCM_MAKINUM (S_IXUSR));
#endif
#ifdef S_IRWXU
  scm_declare_integer_cpp_constant (s_file_mode, "S_IRWXU", SCM_MAKINUM (S_IRWXU));
#endif

#ifdef S_IRGRP
  scm_declare_integer_cpp_constant (s_file_mode, "S_IRGRP", SCM_MAKINUM (S_IRGRP));
#endif
#ifdef S_IWGRP
  scm_declare_integer_cpp_constant (s_file_mode, "S_IWGRP", SCM_MAKINUM (S_IWGRP));
#endif
#ifdef S_IXGRP
  scm_declare_integer_cpp_constant (s_file_mode, "S_IXGRP", SCM_MAKINUM (S_IXGRP));
#endif
#ifdef S_IRWXG
  scm_declare_integer_cpp_constant (s_file_mode, "S_IRWXG", SCM_MAKINUM (S_IRWXG));
#endif

#ifdef S_IROTH
  scm_declare_integer_cpp_constant (s_file_mode, "S_IROTH", SCM_MAKINUM (S_IROTH));
#endif
#ifdef S_IWOTH
  scm_declare_integer_cpp_constant (s_file_mode, "S_IWOTH", SCM_MAKINUM (S_IWOTH));
#endif
#ifdef S_IXOTH
  scm_declare_integer_cpp_constant (s_file_mode, "S_IXOTH", SCM_MAKINUM (S_IXOTH));
#endif
#ifdef S_IRWXO
  scm_declare_integer_cpp_constant (s_file_mode, "S_IRWXO", SCM_MAKINUM (S_IRWXO));
#endif

#ifdef S_ISUID
  scm_declare_integer_cpp_constant (s_file_mode, "S_ISUID", SCM_MAKINUM (S_ISUID));
#endif
#ifdef S_ISGID
  scm_declare_integer_cpp_constant (s_file_mode, "S_ISGID", SCM_MAKINUM (S_ISGID));
#endif
#ifdef S_ISVTX
  scm_declare_integer_cpp_constant (s_file_mode, "S_ISVTX", SCM_MAKINUM (S_ISVTX));
#endif

#ifdef S_IFMT
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFMT", SCM_MAKINUM (S_IFMT));
#endif
#ifdef S_IFDIR
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFDIR", SCM_MAKINUM (S_IFDIR));
#endif
#ifdef S_IFCHR
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFCHR", SCM_MAKINUM (S_IFCHR));
#endif
#ifdef S_IFBLK
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFBLK", SCM_MAKINUM (S_IFBLK));
#endif
#ifdef S_IFREG
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFREG", SCM_MAKINUM (S_IFREG));
#endif
#ifdef S_IFLNK
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFLNK", SCM_MAKINUM (S_IFLNK));
#endif
#ifdef S_IFSOCK
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFSOCK", SCM_MAKINUM (S_IFSOCK));
#endif
#ifdef S_IFIFO
  scm_declare_integer_cpp_constant (s_file_mode, "S_IFIFO", SCM_MAKINUM (S_IFIFO));
#endif

  /****************************************************************
   * F_SETFD
   */
  scm_declare_integer_cpp_constant (s_F_SETFD_flag, "FD_CLOEXEC", SCM_MAKINUM (FD_CLOEXEC));

  /****************************************************************
   * fcntl commands
   */
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_DUPFD", SCM_MAKINUM (F_DUPFD));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_GETFD", SCM_MAKINUM (F_GETFD));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_SETFD", SCM_MAKINUM (F_SETFD));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_GETFL", SCM_MAKINUM (F_GETFL));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_SETFL", SCM_MAKINUM (F_SETFL));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_GETOWN", SCM_MAKINUM (F_GETOWN));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_SETOWN", SCM_MAKINUM (F_SETOWN));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_GETLK", SCM_MAKINUM (F_GETLK));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_SETLK", SCM_MAKINUM (F_SETLK));
  scm_declare_integer_cpp_constant (s_fcntl_cmd, "F_SETLKW", SCM_MAKINUM (F_SETLKW));

  scm_declare_integer_cpp_constant (s_flock_type, "F_RDLCK", SCM_MAKINUM (F_RDLCK));
  scm_declare_integer_cpp_constant (s_flock_type, "F_WRLCK", SCM_MAKINUM (F_WRLCK));
  scm_declare_integer_cpp_constant (s_flock_type, "F_UNLCK", SCM_MAKINUM (F_UNLCK));
}


/*(include-documentation "vuprocs.c")
 */

/****************************************************************
 *h1 "Unix File System Procedures Internals")
 *
 * Descriptor objects are represented by a non-immediate value:
 *
 *  ........flags........scm_tc16_fd  ............int fd..............
 *
 * There are two flags:
 *
 *  scm_close_fd_on_gc 	indicates whether the file should be automatically
 *	    		closed when the descriptor is garbage collected.
 *  scm_fd_is_open   	indicates whether the file is currently open.
 *
 * The scm_sys_protects entry "fd_table" is a dynamic array, indexed by
 * descriptor number, whose elements are descriptor objects.  This
 * array is does not GC protect its members (see `scm_fd_free'.)
 *
 * Directory objects are represented by a non-immediate value:
 *
 *  ......flags...scm_tc16_directory  .......struct dirent *..........
 *
 * There is only one flag and it indicates whether or not the directory
 * is currently open.
 */

/****************************************************************
 *(h1 "Rationale -- Unix File System Procedures")
 *
 * Unix file descriptors are represented as integers in C.
 *
 * A new Scheme type was introduced for descriptors in order to 
 * integrate descriptor handling sanely with garbage collection.
 *
 * Consider this code:
 *
 *		(lambda (data)
 *  		  (let ((fd (%open "/tmp/,x" O_WRONLY #o777)))
 * 		    ...
 * 		    (%write fd data)
 * 		    ...
 * 		    (%close fd)))
 * 
 * If `fd' were simply an integer, an exception-causing error anywhere
 * in the body of the `let' would cause a descriptor leak.  Descriptor
 * objects prevent this.
 */
