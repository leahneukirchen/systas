/* vu.c - virtual unix file-system interface
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/reserv.h"
#include "hackerlab/vu/vu-bad-arg.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/vu/vu.h"


/************************************************************************
 *(h0 "A Virtual Unix File-System Interface"
 *    :include ("vu/vu.h"))
 *
 * \INTERFACES DESCRIBED IN THIS CHAPTER ARE SLIGHTLY UNSTABLE/
 * We think that the design of this subsystem is pretty much right,
 * but that some subtle details are probably wrong.  If you use these
 * functions, you will probably need to modify your program (slightly)
 * to work with future releases.
 * 
 * VU provides a "virtual file-system interface" -- a way for programs
 * to redefine the meaning of the basic file-system functions for a
 * particular file descriptor or for a particular part of the
 * file-system namespace.  When a process defines a virtual
 * file-system, its definitions apply to that process only.
 */
/*(menu)
 */


/****************************************************************
 *(h1 "The VU File-System Functions")
 * 
 * For the purposes of this section, the unix file system interface is
 * defined as these functions:
 *
 *
 *	access chdir chmod chown chroot close closedir fchdir fchmod
 *	fchown fstat fsync ftruncate link lseek lstat mkdir open
 *	opendir read readdir readlink rename rmdir stat
 *	symlink truncate unlink utime write fcntl
 *	dup dup2
 *
 * For each of those functions, there is a corresponding `vu_' function:
 *
 *	vu_access vu_chdir vu_chmod ...
 *
 * The function prototypes of the `vu_' functions are slightly
 * different from the traditional unix functions.  For one thing, all
 * `vu_' functions accept an output paramter, `errn', as the first
 * argument:
 *
 *	int vu_open (int * errn, char * path, int flags, int mode)
 *
 * That parameter takes the place of the global variable `errno'.
 * When an error occurs in a `vu_' function, the error number is
 * returned in `*errn'.
 *
 * The other interface difference is the functions for reading
 * directories.  The `vu_' functions return an integer to indicate
 * success (0) or failure (-1) and return other information through
 * output parameters:
 *
 * 	int vu_opendir (int * errn, DIR ** retv,  char * path)
 *	int vu_readdir (int * errn,
 *			struct alloc_limits * limits,
 *			char ** file_ret,
 *			DIR * dir)
 * 
 * Aside from those difference, `vu_' functions can be substituted
 * freely for their unix counterparts.  If no other changes are made,
 * the modified program will run just the same as the original.
 * 
 * There are two additional `vu_' functions:
 *
 *	vu_read_retry vu_write_retry
 *
 * which are similar to `read' and `write' but which
 * automatically restart after a `EINTR' or `EAGAIN'.
 *
 */

/****************************************************************
 *(h1 "Virtual File-System Functions")
 * 
 * Programs can provide their own definitions of the `vu_' file-system
 * functions.  These definitions can be made to apply to specific file
 * descriptors or to a subset of the names in the file-system
 * namespace of the process.
 *
 * To define a set of virtual functions, a program defines functions
 * having the same return types and almost the same parameters as the
 * `vu_' virtual functions.  The virtual functions take one extra
 * (final) argument, `void * closure'.  For example:
 *
 *	int my_virtual_open (int * errn, 
 *			     char * path, 
 *			     int flags,
 * 			     int mode,
 *			     void * closure)
 * 
 * All of these functions must be named consistently by adding a
 * prefix to the root name of the file system functions:
 *
 *	my_virtual_access my_virtual_chdir my_virtual_chmod ...
 * 
 * Two additional functions must be provided:
 *
 *	void * my_virtual_make_closure (void * closure)
 *	void my_virtual_free_closure (void * closure)
 *
 * These are explained below.
 *
 * Having defined a set of virtual file system functions, a program
 * must declare a "vtable" for the functions:
 *
 *	#include "hackerlab/vu/vu.h"
 *	...
 *	
 *	struct vu_fs_discipline my_fs_vtable
 * 	 = { VU_FS_DISCIPLINE_INITIALIZERS (my_virtual_) };
 *
 * Note that the prefix used to name virtual functions is used by
 * the macro `VU_FS_DISCIPLINE_INITIALIZERS' to initialize the vtable.
 *
 * Finally, the functions may be applied to either a portion of the 
 * file-system namespace or to a particular descriptor:
 *
 *	// Applying `my_virtual_' functions to a particular 
 *	// descriptor:
 *	//
 *	vu_set_fd_handler (fd, &my_fs_vtable, closure);
 *
 *	// Applying `my_virtual_' functions to a part
 *	// of the file-system namespace.  Note that
 *	// preg is a `regex_t' -- a regular expression
 *	// compiled by `regcomp'.  `eflags' is like the
 *	// parameter of the same name to `regexec':
 *	//
 *	vu_push_name_handler (name, doc, preg, eflags, 
 *			      &my_fs_vtable, closure, 0);
 * 
 * After those calls, a call to a `vu_' file-system using descriptor
 * `fd' will do its work by calling a `my_virtual_' function (e.g.
 * `vu_read' will call `my_virtual_read' to read from `fd').
 * 
 * A call to a `vu_' function with a path that matches the regexp
 * `preg' will also do its work by calling a `my_virtual_' function
 * (e.g. `vu_open' will call `my_virtual_open' to open a matching
 * file-name).  If a new file is successfully opened this way, VU
 * automatically calls `vu_set_fd_handler' to establish the same
 * vtable as the handler for the new descriptor.
 *
 * Name-space handlers are kept on a stack and searched from the
  * top of stack down.  Each handler has a name and an array of
 * strings which are its documentation.
 */

/****************************************************************
 *(h1 "VU Closures")
 *
 * When a set of virtual file system functions is installed by
 * `vu_set_fd_handler' or `vu_push_name_handler' the caller provides a
 * closure.  This closure is an opaque value to `vu' itself.  A copy
 * of the closure is passed as the final parameter to the caller's
 * virtual functions.  For example, to open a file that has matched a
 * regular expression passed to `vu_push_name_handler', `vu_open' uses
 * the sequence:
 *
 *     fd = handler->vtable->open (errn, path, flags, mode, 
 *				   handler->closure);
 *	
 * VU doesn't save a copy of the closure directly.  Instead, it calls
 * the `make_closure' function from the vtable to create the value
 * to save and the `free_closure' function when that copy is being
 * discarded.  `make_closure' is called once when
 * `vu_push_name_handler' is called, and once each time
 * `vu_set_fd_handler' is called.   `free_closure' is called each time
 * `vu_close' or `vu_closedir' is called.
 */

/*(c vu_handler :category type)
 * struct vu_handler;
 *
 * For each VU namespace handler and file-descriptor handler, there
 * is a `struct vu_handler' that indicates the `vtable' and `closure'
 * to use for that portion of the file-system.
 *
 *    struct vu_handler
 *    {
 *      struct vu_fs_discipline * vtable;
 *      void * closure;
 *    };
 *
 */

/****************************************************************
 *(h1 "Pseudo-Descriptors")
 *
 * The `vu_' functions can operate on file descriptors which are
 * created by the program itself without the knowledge of the
 * operating system kernel.  The advantage of such descriptors is that
 * they take up no kernel resources so it is practical to create a
 * large number of them.
 *
 * Pseudo-descriptors which are guaranteed to be distinct from all
 * kernel descriptors can be created using the function
 * `reserv_pseudo' and destroyed using the function `unreserv_pseudo'.
 * For example:
 *
 *	fd = reserv_pseudo ();
 *	vu_set_fd_handler (fd, &my_fs_vtable, 0);
 *
 * and:
 *
 *	int
 *	my_virtual_close (int * errn, int fd, void * closure)
 *	{
 * 	  unreserv_pseudo (fd);
 *	  return 0;
 *	}
 *
 * One way to use a pseudo-descriptor is to combine it with
 * buffered-I/O to create a file that corresponds to a string in the
 * program's address space.  (See xref:"Buffered File Descriptors".)
 */



/* static struct _vu_namespace_handler * _vu_fs_handlers;
 * 
 * A stack (using `ar_push' and `ar_pop') of VU namespace handlers.
 */
struct _vu_namespace_handler * _vu_fs_handlers = 0;


/* static struct _vu_namespace_handler * _vu_optional_fs_handlers;
 * 
 * A stack (using `ar_push' and `ar_pop') of optional VU namespace 
 * handlers.
 */
struct _vu_namespace_handler * _vu_optional_fs_handlers = 0;


/* static struct vu_handler * fd_handlers;
 * 
 * An array (using `ar_ref') of VU descriptor handlers, indexed by
 * descriptor numbers.
 */
static struct vu_handler * fd_handlers = 0;


/* static DIR * known_dirs;
 *
 * An array (using `ar_ref') of open directories known to VU.
 * Indexed by descriptor numbers.
 */
static DIR ** known_dirs = 0;


/* static struct vu_handler bad_arg_handler;
 * 
 * The vu_handler for non-descriptors (descriptor numbers less than 0).
 */
static struct vu_handler bad_arg_handler = { &_vu_bad_arg_functions, 0 };


/* static struct vu_handler default_handler;
 * 
 * The vu_handler for file names and descriptors with no other handler.
 */
static struct vu_handler default_handler = { &_vu_system_fs_vtable, 0 };




/************************************************************************
 *(h1 "Establishing VU Handlers")
 * 
 */

/* Dynamically setting rules for dispatching on file names and
 * descriptor numbers:
 */

/*(c vu_push_name_handler)
 * void vu_push_name_handler (t_uchar * name,
 *			      t_uchar ** doc,
 *			      regex_t * preg,
 *			      int eflags,
 *			      struct vu_fs_discipline * vtable,
 *			      void * closure,
 *			      int is_optional);
 * 
 * `vu_push_name_handler' establishs a vtable of virtual file system
 * functions for a portion of the file-system namespace.
 *
 * `name' is the name for the handler recognized by
 * `vu_enable_optional_name_handler'.  Conventionally,
 * this name may be used as an option argument to the 
 * command line options `-N' or `--namespace'.  For optimal 
 * help message formatting, `name' should be no longer than
 * 30 characters.  A pointer to `name' is kept by this function.
 *
 * `doc' is a documentation string for the handler, printed in the
 * outpupt of `vu_help_for_optional_handlers'.  For optimal help
 * message formatting, each line of `doc' should be no longer than 40
 * characters.  The last element of the array `doc' must be 0.
 * A pointer to `doc' is kept by this function.
 *
 * File-names matching `preg' are handled by the functions in
 * `vtable'.  Matching is performed by `regexec':
 *
 *	regexec (preg, path, 0, 0, eflags)
 *
 * If a matching file is successfully opened, `vu_open' and
 * `vu_opendir' call:
 *
 *	vu_set_fd_handler (new_fd, vtable, closure)
 *
 * If `is_optional' is not 0, the file-name handler is recorded
 * but not enabled.  It can be made active by a call to 
 * `vu_enable_optional_name_handler'.
 */
void
vu_push_name_handler (t_uchar * name,
		      t_uchar ** doc,
		      regex_t * preg,
		      int eflags,
		      struct vu_fs_discipline * vtable,
		      void * closure,
		      int is_optional)
{
  struct _vu_namespace_handler * name_handler;

  if (is_optional)
    name_handler = (struct _vu_namespace_handler *)ar_push ((void **)&_vu_optional_fs_handlers, lim_use_must_malloc, sizeof (*name_handler));
  else
    name_handler = (struct _vu_namespace_handler *)ar_push ((void **)&_vu_fs_handlers, lim_use_must_malloc, sizeof (*name_handler));
  name_handler->preg = preg;
  name_handler->eflags = eflags;
  name_handler->handler.vtable = vtable;
  name_handler->handler.closure = vtable->make_closure (closure);
  name_handler->name = name;
  name_handler->doc = doc;
}


/*(c vu_enable_optional_name_handler)
 * int vu_enable_optional_name_handler (t_uchar * name);
 *
 * Push the named namespace handler on the VU namespace stack.
 *
 * The named handler must have previously been established by a 
 * call to `vu_push_optional_name_handler'.
 *
 * Return 0 on success, -1 if the named handler was not found.
 */
int
vu_enable_optional_name_handler (t_uchar * name)
{
  int x;
  int len;

  len = ar_size (_vu_optional_fs_handlers, lim_use_must_malloc, sizeof (*_vu_optional_fs_handlers));
  for (x = 0; x < len; ++x)
    {
      if (!str_cmp (name, _vu_optional_fs_handlers[x].name))
	{
	  vu_push_name_handler (_vu_optional_fs_handlers[x].name,
				_vu_optional_fs_handlers[x].doc,
				_vu_optional_fs_handlers[x].preg,
				_vu_optional_fs_handlers[x].eflags,
				_vu_optional_fs_handlers[x].handler.vtable,
				_vu_optional_fs_handlers[x].handler.closure,
				0);
	  return 0;
	}
    }
  return -1;
}


static void
close_vu_files ()
{
  int n_dirs;
  int n_handlers;
  int x;

  n_dirs = ar_size ((void *)known_dirs, lim_use_must_malloc, sizeof (*known_dirs));
  for (x = 0; x < n_dirs; ++x)
    {
      int errn;
      if (known_dirs[x])
	vu_closedir (&errn, known_dirs[x]);
    }
  n_handlers = ar_size ((void *)fd_handlers, lim_use_must_malloc, sizeof (*fd_handlers));
  for (x = 0; x < n_handlers; ++x)
    {
      if (fd_handlers[x].vtable)
	{
	  int errn;
	  vu_close (&errn, x);
	}
    }
}


/*(c vu_set_fd_handler)
 * void vu_set_fd_handler (int fd,
 *			   struct vu_fs_discipline * vtable,
 *			   void * closure);
 * 
 * Establish a vtable of virtual file system functions for a
 * particular descriptor or pseudo-descriptor.
 *
 * The handler is automatically removed by `vu_close'.
 */
void
vu_set_fd_handler (int fd,
		   struct vu_fs_discipline * vtable,
		   void * closure)
{
  static int initialized = 0;
  struct vu_handler * handler;

  if (!initialized)
    {
      if (0 > atexit (close_vu_files))
	panic ("error registering atexit function in vu_set_fd_handler");
      initialized = 1;
    }

  handler = (struct vu_handler *)ar_ref ((void **)&fd_handlers, lim_use_must_malloc, fd, sizeof (*handler));
  handler->vtable = vtable;
  handler->closure = vtable->make_closure (closure);
}


void
vu_reinit_after_unexec (void)
{
#ifdef __FreeBSD__
  if (0 > atexit (close_vu_files))
    panic ("error registering atexit function in vu_reinit_after_unexec");
#endif
}

/*(c vu_move_state)
 * int vu_move_state (int * errn, int fd, int newfd);
 * 
 * Move the VU handler for `fd' to `newfd'.
 *
 */
int
vu_move_state (int * errn, int fd, int newfd)
{
  struct vu_handler * handler;
  int rv;

  handler = vu_fd_dispatch (fd);
  rv = handler->vtable->move_state (errn, fd, newfd, handler->closure);
  return rv;
}





/************************************************************************
 *(h1 "Looking Up VU Handlers")
 * 
 * 
 * 
 */

/*(c vu_path_dispatch)
 * struct vu_handler * vu_path_dispatch (char * path);
 * 
 * Return the vtable and closure that handle the file-name `path'.
 * (See xref:"vu_handler".)
 */
struct vu_handler *
vu_path_dispatch (char * path)
{
  int x;
  for (x = ar_size ((void *)_vu_fs_handlers, lim_use_must_malloc, sizeof (*_vu_fs_handlers)) - 1; x >= 0; --x)
    {
      if (!regexec (_vu_fs_handlers[x].preg, path, 0, 0, _vu_fs_handlers[x].eflags))
	return &_vu_fs_handlers[x].handler;
    }
  return &default_handler;
}


/*(c vu_fd_dispatch)
 * struct vu_handler * vu_fd_dispatch (int fd);
 * 
 * Return the vtable and closure that handle the descriptor `fd'.
 * (See xref:"vu_handler".)
 */
struct vu_handler *
vu_fd_dispatch (int fd)
{
  if (fd < 0)
    return &bad_arg_handler;

  if ((fd >= ar_size ((void *)fd_handlers, lim_use_must_malloc, sizeof (*fd_handlers))) || !fd_handlers[fd].vtable)
    return &default_handler;

  return &fd_handlers[fd];
}


/*(c vu_dir_dispatch)
 * struct vu_handler * vu_dir_dispatch (DIR * dir);
 * 
 * Return the vtable and closure that handle the directory `dir'.
 * (See xref:"vu_handler".)
 */
struct vu_handler *
vu_dir_dispatch (DIR * dir)
{
  return vu_fd_dispatch (vu_dir_fd (dir));
}



/************************************************************************
 *(h1 "Stacking Descriptor Handlers")
 *
 * The function `vu_fd_dispatch' returns a pointer to a `struct
 * vu_handler' which in turn holds a pointer to the vtable and closure
 * used to handle `vu_' functions for a particular descriptor (see
 * xref:"vu_handler").
 *
 *
 * Using `vu_fd_dispatch', descriptor handlers can be stacked.  For
 * example, the buffered-I/O functions (`vfdbuf_') work by imposing a
 * file-system vtable that maintains a buffer but that performs actual
 * I/O by calling functions from an underlying vtable.  To establish
 * the buffering vtable, the function `vfdbuf_buffer_fd' uses a
 * sequence of operations like:
 *
 *	int
 *	vfdbuf_buffer_fd (int * errn, int fd,
 *			  long bufsize, int flags, int zero_buffer)
 * 	{
 *	  struct vu_handler * sub_handler;
 * 
 * 	  ...
 *	  sub_handler = vu_fd_dispatch (fd);
 * 
 *	  ... remember that sub_handler does I/O for fd:
 *	  bufs[fd].sub_handler = sub_handler;
 *	  ...
 * 
 * 	  ... Establish the buffering functions as the new vtable for
 * 	      `fd'.
 * 
 *	  vu_set_fd_handler (fd, &vfdbuf_vtable, 0);
 *	}
 *
 * The `vfdbuf_' file-system functions follow this example:
 *
 * 	int
 * 	vfdbuf_fsync (int * errn, int fd, void * closure)
 * 	{
 * 	  ... Empty the buffer.
 *	
 * 	  if (vfdbuf_flush (errn, fd) < 0)
 * 	    return -1;
 * 	  
 * 	  ... Perform the actual `fsync' using the vtable set aside
 *	      in vfdbuf_buffer_fd:
 * 
 * 	  return bufs[fd].sub_handler.vtable->fsync 
 *			(errn, fd, bufs[fd].sub_handler.closure);
 * 	}
 *
 * Note that when closing a file, it is the responsibility of the
 * `vfdbuf_' functions to free the closure for the underlying vtable:
 *
 * 	int
 * 	vfdbuf_close (int * errn, int fd, void * closure)
 * 	{
 * 	  int errn
 * 	  int got;
 * 	  int ign;
 * 	
 *	  ...
 * 	  got = bufs[fd].sub_handler.vtable->close 
 *			(errn, fd, bufs[fd].sub_handler.closure);
 *
 * 	  bufs[fd].sub_handler.vtable->free_closure
 * 		(bufs[fd].sub_handler.closure);
 *	  ...
 * 	}
 *
 */

/************************************************************************
 *(h1 "The VU File-system Interface")
 * 
 * These functions approximately mirror the traditional unix system call
 * interface, but have these improvments:
 *
 * 	1. Error numbers are not stored in a global, but
 *	   in a return value.
 * 
 *	2. Functions dispatch on file names and descriptors,
 *	   permitting these functions to work on objects other than
 *	   ordinary files and sockets and to work on ordinary files
 *	   and sockets in unusual ways.
 */

/*(c vu_access)
 * int vu_access (int * errn, char * path, int mode);
 * 
 * See the manual page for `access'.
 */
int
vu_access (int * errn, char * path, int mode)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->access(errn, path, mode, handler->closure);
}


/*(c vu_chdir)
 * int vu_chdir (int * errn, char * path);
 * 
 * See the manual page for `chdir'.
 */
int
vu_chdir (int * errn, char * path)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->chdir(errn, path, handler->closure);
}


/*(c vu_chmod)
 * int vu_chmod (int * errn, char * path, int mode);
 * 
 * See the manual page for `chmod'.
 */
int
vu_chmod (int * errn, char * path, int mode)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->chmod(errn, path, mode, handler->closure);
}


/*(c vu_chown)
 * int vu_chown (int * errn, char * path, int owner, int group);
 * 
 * See the manual page for `chown'.
 */
int
vu_chown (int * errn, char * path, int owner, int group)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->chown(errn, path, owner, group, handler->closure);
}


/*(c vu_chroot)
 * int vu_chroot (int * errn, char * path);
 * 
 * See the manual page for `chroot'.
 */
int
vu_chroot (int * errn, char * path)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->chroot(errn, path, handler->closure);
}


/*(c vu_close)
 * int vu_close (int * errn, int fd);
 * 
 * See the manual page for `close'.
 */
int
vu_close (int * errn, int fd)
{
  struct vu_handler * handler;
  int status;

  handler = vu_fd_dispatch (fd);
  status = handler->vtable->close(errn, fd, handler->closure);
  if (status >= 0)
    {
      if (handler != &default_handler)
	{
	  if (handler->vtable->free_closure)
	    handler->vtable->free_closure (handler->closure);
	  handler->vtable = 0;
	  handler->closure = 0;
	}
    }
  return status;
}

/*(c vu_closedir)
 * int vu_closedir (int * errn, DIR * dir);
 * 
 * See the manual page for `closedir'.
 */
int
vu_closedir (int * errn, DIR * dir)
{
  struct vu_handler * handler;
  int fd;
  int status;

  handler = vu_dir_dispatch (dir);
  fd = vu_dir_fd (dir);
  status = handler->vtable->closedir(errn, dir, handler->closure);
  if (status >= 0)
    {
      DIR ** dirp;

      dirp = (DIR **)ar_ref ((void **)&known_dirs, lim_use_must_malloc, fd, sizeof (DIR *));
      *dirp = 0;
      if (handler != &default_handler)
	{
	  if (handler->vtable->free_closure)
	    handler->vtable->free_closure (handler->closure);
	  handler->vtable = 0;
	  handler->closure = 0;
	}
      unreserv_pseudo (fd);
    }
  return status;
}


/*(c vu_fchdir)
 * int vu_fchdir (int * errn, int fd);
 * 
 * See the manual page for `fchdir'.
 */
int
vu_fchdir (int * errn, int fd)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fchdir(errn, fd, handler->closure);
}


/*(c vu_fchmod)
 * int vu_fchmod (int * errn, int fd, int mode);
 * 
 * See the manual page for `fchmod'.
 */
int
vu_fchmod (int * errn, int fd, int mode)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fchmod (errn, fd, mode, handler->closure);
}


/*(c vu_fchown)
 * int vu_fchown (int * errn, int fd, int owner, int group);
 * 
 * See the manual page for `fchown'.
 */
int
vu_fchown (int * errn, int fd, int owner, int group)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fchown (errn, fd, owner, group, handler->closure);
}


/*(c vu_fstat)
 * int vu_fstat (int * errn, int fd, struct stat * buf);
 * 
 * See the manual page for `fstat'.
 */
int
vu_fstat (int * errn, int fd, struct stat * buf)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fstat(errn, fd, buf, handler->closure);
}


/*(c vu_fsync)
 * int vu_fsync (int * errn, int fd);
 * 
 * See the manual page for `fsync'.
 */
int
vu_fsync (int * errn, int fd)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fsync(errn, fd, handler->closure);
}


/*(c vu_ftruncate)
 * int vu_ftruncate (int * errn, int fd, off_t where);
 * 
 * See the manual page for `ftruncate'.
 */
int
vu_ftruncate (int * errn, int fd, off_t where)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->ftruncate(errn, fd, where, handler->closure);
}


/*(c vu_link)
 * int vu_link (int * errn, char * from, char * to);
 * 
 * See the manual page for `link'.
 */
int
vu_link (int * errn, char * from, char * to)
{
  struct vu_handler * handler;
  struct vu_handler * handler2;

  handler = vu_path_dispatch (from);
  handler2 = vu_path_dispatch (to);
  if (   (handler->vtable->link != handler2->vtable->link)
      || (handler->closure != handler2->closure))
    {
      *errn = EXDEV;
      return -1;
    }
  return handler->vtable->link(errn, from, to, handler->closure);
}


/*(c vu_lseek)
 * off_t vu_lseek (int * errn, int fd, off_t offset, int whence);
 * 
 * See the manual page for `lseek'.
 */
off_t
vu_lseek (int * errn, int fd, off_t offset, int whence)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->lseek(errn, fd, offset, whence, handler->closure);
}


/*(c vu_lstat)
 * int vu_lstat (int * errn, char * path, struct stat * buf);
 * 
 * See the manual page for `lstat'.
 */
int
vu_lstat (int * errn, char * path, struct stat * buf)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->lstat(errn, path, buf, handler->closure);
}


/*(c vu_mkdir)
 * int vu_mkdir (int * errn, char * path, int mode);
 * 
 * See the manual page for `mkdir'.
 */
int
vu_mkdir (int * errn, char * path, int mode)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->mkdir(errn, path, mode, handler->closure);
}


/*(c vu_open)
 * int vu_open (int * errn, char * path, int flags, int mode);
 * 
 * See the manual page for `open'.
 */
int
vu_open (int * errn, char * path, int flags, int mode)
{
  struct vu_handler * handler;
  int fd;

  handler = vu_path_dispatch (path);
  fd = handler->vtable->open(errn, path, flags, mode, handler->closure);
  if (fd >= 0)
    {
      if ((ar_size ((void *)fd_handlers, lim_use_must_malloc, sizeof (*fd_handlers)) <= fd) || !fd_handlers [fd].vtable)
	vu_set_fd_handler (fd, handler->vtable, handler->closure);
    }
    
  return fd;
}


/*(c vu_dir_fd)
 * int vu_dir_fd (DIR * dir);
 * 
 * Return the pseudo descriptor associated with DIR.
 */
int
vu_dir_fd (DIR * dir)
{
  int x;

  x = ar_size ((void *)known_dirs, lim_use_must_malloc, sizeof (*known_dirs)) - 1;

  while (x >= 0)
    {
      if (known_dirs[x] == dir)
	return x;
      --x;
    }

  panic ("attempt to find pseudo fd for unknown dir");
  return -1;
}


/*(c vu_opendir)
 * int vu_opendir (int * errn, DIR ** retv,  char * path);
 * 
 * See the manual page for `opendir'.
 */
int
vu_opendir (int * errn, DIR ** retv,  char * path)
{
  struct vu_handler * handler;
  int fd;
  int status;

  handler = vu_path_dispatch (path);
  status = handler->vtable->opendir(errn, retv, path, handler->closure);
  if (status >= 0)
    {
      fd = reserv_pseudo (errn, 0);
      if (fd < 0)
	panic ("unable to reserve pseudo fd in vu_opendir");
      {
	DIR ** dirp;
	  
	dirp = (DIR **)ar_ref ((void **)&known_dirs, lim_use_must_malloc, fd, sizeof (DIR *));
	*dirp = *retv;
	vu_set_fd_handler (fd, handler->vtable, handler->closure);
      }
    }
  return status;
}


/*(c vu_read)
 * ssize_t vu_read (int * errn, int fd, char * buf, size_t count);
 * 
 * See the manual page for `read'.
 */
ssize_t
vu_read (int * errn, int fd, char * buf, size_t count)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->read (errn, fd, buf, count, handler->closure);
}


/*(c vu_read_retry)
 * ssize_t vu_read_retry (int * errn, int fd, char * buf, size_t count);
 * 
 * Use `vu_read' to read from `fd'.  Read repeatedly (even if a read
 * returns early from EINTR or EAGAIN) until `count' characters are
 * read, or the end-of-file is reached.
 *
 * Return the number of characters read or -1 on error.
 */
ssize_t
vu_read_retry (int * errn, int fd, char * buf, size_t count)
{
  ssize_t amt_read;

  amt_read = 0;
  while (1)
    {
      ssize_t amt;
      amt = vu_read (errn, fd, buf, count);
      if ((amt < 0) && (*errn != EAGAIN) && (*errn != EINTR))
	return amt;
      amt_read += amt;
      if (!amt || (amt == count))
	return amt_read;
      count -= amt;
      buf += amt;
    }
}


/*(c vu_readdir)
 * int vu_readdir (int * errn,
 *		   struct alloc_limits * limits,
 *		   char ** file_ret,
 *		   DIR * dir);
 * 
 * See the manual page for `readdir'.
 * 
 * Note that in `vu', the file name is returned in `*file_ret'
 * and is dynamically allocated using `limits'.  It is up to the
 * caller to free the file name.
 */
int
vu_readdir (int * errn,
	    struct alloc_limits * limits,
	    char ** file_ret,
	    DIR * dir)
{
  struct vu_handler * handler;

  handler = vu_dir_dispatch (dir);
  return handler->vtable->readdir(errn, limits, file_ret, dir, handler->closure);
}


/*(c vu_readlink)
 * int vu_readlink (int * errn, char * path, char * buf, int bufsize);
 * 
 * See the manual page for `readlink'.
 */
int
vu_readlink (int * errn, char * path, char * buf, int bufsize)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->readlink(errn, path, buf, bufsize, handler->closure);
}


/*(c vu_rename)
 * int vu_rename (int * errn, char * from, char * to);
 * 
 * See the manual page for `rename'.
 */
int
vu_rename (int * errn, char * from, char * to)
{
  struct vu_handler * handler;
  struct vu_handler * handler2;

  handler = vu_path_dispatch (from);
  handler2 = vu_path_dispatch (to);
  if (   (handler->vtable->rename != handler2->vtable->rename)
      || (handler->closure != handler2->closure))
    {
      *errn = EXDEV;
      return -1;
    }
  return handler->vtable->rename(errn, from, to, handler->closure);
}


/*(c vu_rmdir)
 * int vu_rmdir (int * errn, char * path);
 * 
 * See the manual page for `rmdir'.
 */
int
vu_rmdir (int * errn, char * path)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->rmdir(errn, path, handler->closure);
}


/*(c vu_stat)
 * int vu_stat (int * errn, char * path, struct stat * buf);
 * 
 * See the manual page for `stat'.
 */
int
vu_stat (int * errn, char * path, struct stat * buf)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->stat(errn, path, buf, handler->closure);
}


/*(c vu_symlink)
 * int vu_symlink (int * errn, char * from, char * to);
 * 
 * See the manual page for `symlink'.
 */
int
vu_symlink (int * errn, char * from, char * to)
{
  struct vu_handler * handler;
  struct vu_handler * handler2;

  handler = vu_path_dispatch (from);
  handler2 = vu_path_dispatch (to);
  if (   (handler->vtable->symlink != handler2->vtable->symlink)
      || (handler->closure != handler2->closure))
    {
      *errn = EXDEV;
      return -1;
    }
  return handler->vtable->symlink(errn, from, to, handler->closure);
}



/*(c vu_truncate)
 * int vu_truncate (int * errn, char * path, off_t where);
 * 
 * See the manual page for `truncate'.
 */
int
vu_truncate (int * errn, char * path, off_t where)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->truncate(errn, path, where, handler->closure);
}


/*(c vu_unlink)
 * int vu_unlink (int * errn, char * path);
 * 
 * See the manual page for `unlink'.
 */
int
vu_unlink (int * errn, char * path)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->unlink(errn, path, handler->closure);
}


/*(c vu_utime)
 * int vu_utime (int * errn, char * path, struct utimbuf *times);
 * 
 * See the manual page for `utime'.
 */
int
vu_utime (int * errn, char * path, struct utimbuf * times)
{
  struct vu_handler * handler;

  handler = vu_path_dispatch (path);
  return handler->vtable->utime (errn, path, times, handler->closure);
}


/*(c vu_write)
 * ssize_t vu_write (int * errn, int fd, char * buf, size_t count);
 * 
 * See the manual page for `write'.
 */
ssize_t
vu_write (int * errn, int fd, char * buf, size_t count)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->write(errn, fd, buf, count, handler->closure);
}


/*(c vu_write_retry)
 * ssize_t vu_write_retry (int * errn, int fd, char * buf, size_t count);
 * 
 * Use `vu_write' to write to `fd'.  Write repeatedly (even if a write
 * returns early from EINTR or EAGAIN) until `count' characters are
 * written.
 * 
 * Return the number of characters written or -1 on error.
 */
ssize_t
vu_write_retry (int * errn, int fd, char * buf, size_t count)
{
  size_t orig_count;

  orig_count = count;
  while (1)
    {
      ssize_t amt;
      
      amt = vu_write (errn, fd, buf, count);
      if ((amt < 0) && (*errn != EAGAIN) && (*errn != EINTR))
	return amt;
      if (amt == count)
	return orig_count;
      count -= amt;
      buf += amt;
    }
}


/*(c vu_fcntl)
 * int vu_fcntl (int * errn, int fd, int cmd, long arg);
 * 
 * See the manual page for `fcntl'.
 */
int
vu_fcntl (int * errn, int fd, int cmd, long arg)
{
  struct vu_handler * handler;

  handler = vu_fd_dispatch (fd);
  return handler->vtable->fcntl(errn, fd, cmd, arg, handler->closure);
}


/*(c vu_dup)
 * int vu_dup (int * errn, int fd);
 * 
 * See the manual page for `dup'.
 */
int
vu_dup (int * errn, int fd)
{
  struct vu_handler * handler;
  int new_fd;

  handler = vu_fd_dispatch (fd);
  new_fd = handler->vtable->dup(errn, fd, handler->closure);
  
  return new_fd;
}


/*(c vu_dup2)
 * int vu_dup2 (int * errn, int fd, int newfd);
 * 
 * See the manual page for `dup2'.
 */
int
vu_dup2 (int * errn, int fd, int newfd)
{
  struct vu_handler * handler;
  int new_fd;

  handler = vu_fd_dispatch (fd);
  new_fd = handler->vtable->dup2(errn, fd, newfd, handler->closure);
  return new_fd;
}

