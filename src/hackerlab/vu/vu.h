/* vu.h - virtual unix file-system interface decls
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__VU_H
#define INCLUDE__VU__VU_H



#include "hackerlab/os/sys/stat.h"
#include "hackerlab/os/fcntl.h"
#include "hackerlab/os/unistd.h"
#include "hackerlab/os/stdlib.h"
#include "hackerlab/os/sys/time.h"
#include "hackerlab/os/utime.h"
#include "hackerlab/os/dirent.h"
#include "hackerlab/rx-posix/regexps.h"



/*c #define VU_MAP_FS_NAMES(c ,prefix, middle, suffix, punct)
 *
 * expand to many calls for the form:
 *
 *   VU_FS_NAME##c (fn, prefix, middle, suffix, ret, proto) punct
 *
 * where:
 *
 * `fn' is the root name of a virtual system call (e.g. `access' or
 * `open')
 *
 * `ret' is the return type of that system call (e.g. `int')
 *
 * `proto' is the argument list prototype of implementations of that 
 * system call (e.g. `(int * errn, char * path, int mode, void * closure)')
 *
 * By passing a suitable value for `c', this expansion can call another
 * macro (e.g. VU_FS_NAME_A) to construct structure initializers, 
 * structure field declarations, etc.
 *
 * To generate a list of fill-in-the-blank templates for VU functions, 
 * run `vu-prototypes your_prefix_'.
 */
#define VU_MAP_FS_NAMES(c ,prefix, middle, suffix, punct) \
 VU_FS_NAME##c (make_closure, prefix, middle, suffix, \
		    void *, (void * closure)) punct \
 VU_FS_NAME##c (free_closure, prefix, middle, suffix, \
		    void, (void * closure)) punct \
 VU_FS_NAME##c (access, prefix, middle, suffix, \
		    int, (int * errn, char * path, int mode, void * closure)) punct \
 VU_FS_NAME##c (chdir, prefix, middle, suffix, \
		    int, (int * errn, char * path, void * closure)) punct \
 VU_FS_NAME##c (chmod, prefix, middle, suffix, \
		    int, (int * errn, char * path, int mode, void * closure)) punct \
 VU_FS_NAME##c (chown, prefix, middle, suffix, \
		    int, (int * errn, char * path, int owner, int group, void * closure)) punct \
 VU_FS_NAME##c (chroot, prefix, middle, suffix, \
		    int, (int * errn, char * path, void * closure)) punct \
 VU_FS_NAME##c (close, prefix, middle, suffix, \
		    int, (int * errn, int fd, void * closure)) punct \
 VU_FS_NAME##c (closedir, prefix, middle, suffix, \
		    int, (int * errn, DIR * dir, void * closure)) punct \
 VU_FS_NAME##c (fchdir, prefix, middle, suffix, \
		    int, (int * errn, int fd, void * closure)) punct \
 VU_FS_NAME##c (fchmod, prefix, middle, suffix, \
		    int, (int * errn, int fd, int mode, void * closure)) punct \
 VU_FS_NAME##c (fchown, prefix, middle, suffix, \
		    int, (int * errn, int fd, int owner, int group, void * closure)) punct \
 VU_FS_NAME##c (fstat, prefix, middle, suffix, \
		    int, (int * errn, int fd, struct stat * buf, void * closure)) punct \
 VU_FS_NAME##c (fsync, prefix, middle, suffix, \
		    int, (int * errn, int fd, void * closure)) punct \
 VU_FS_NAME##c (ftruncate, prefix, middle, suffix, \
		    int, (int * errn, int fd, off_t where, void * closure)) punct \
 VU_FS_NAME##c (link, prefix, middle, suffix, \
		    int, (int * errn, char * from, char * to, void * closure)) punct \
 VU_FS_NAME##c (lseek, prefix, middle, suffix, \
		    off_t, (int * errn, int fd, off_t offset, int whence, void * closure)) punct \
 VU_FS_NAME##c (lstat, prefix, middle, suffix, \
		    int, (int * errn, char * path, struct stat * buf, void * closure)) punct \
 VU_FS_NAME##c (mkdir, prefix, middle, suffix, \
		    int, (int * errn, char * path, int mode, void * closure)) punct \
 VU_FS_NAME##c (open, prefix, middle, suffix, \
		    int, (int * errn, char * path, int flags, int mode, void * closure)) punct \
 VU_FS_NAME##c (opendir, prefix, middle, suffix, \
		    int, (int * errn, DIR ** retv,  char * path, void * closure)) punct \
 VU_FS_NAME##c (read, prefix, middle, suffix, \
		    ssize_t, \
		    (int * errn, int fd, char * buf, size_t count, void * closure)) punct \
 VU_FS_NAME##c (readdir, prefix, middle, suffix, \
		    int, (int * errn, struct alloc_limits * lims, char ** file, DIR * dir, void * closure)) punct \
 VU_FS_NAME##c (readlink, prefix, middle, suffix, \
		    int, (int * errn, char * path, char * buf, int bufsize, void * closure)) punct \
 VU_FS_NAME##c (rename, prefix, middle, suffix, \
		    int, (int * errn, char * from, char * to, void * closure)) punct \
 VU_FS_NAME##c (rmdir, prefix, middle, suffix, \
		    int, (int * errn, char * path, void * closure)) punct \
 VU_FS_NAME##c (stat, prefix, middle, suffix, \
		    int, (int * errn, char * path, struct stat * buf, void * closure)) punct \
 VU_FS_NAME##c (symlink, prefix, middle, suffix, \
		    int, (int * errn, char * from, char * to, void * closure)) punct \
 VU_FS_NAME##c (truncate, prefix, middle, suffix, \
		    int, (int * errn, char * path, off_t where, void * closure)) punct \
 VU_FS_NAME##c (unlink, prefix, middle, suffix, \
		    int, (int * errn, char * path, void * closure)) punct \
 VU_FS_NAME##c (utime, prefix, middle, suffix, \
		    int, (int * errn, char * path, struct utimbuf * times, void * closure)) punct \
 VU_FS_NAME##c (write, prefix, middle, suffix, \
		    ssize_t, \
		    (int * errn, int fd, char * buf, size_t count, void * closure)) punct \
 VU_FS_NAME##c (fcntl, prefix, middle, suffix, \
		    int, (int * errn, int fd, int cmd, long arg, void * closure)) punct \
 VU_FS_NAME##c (dup, prefix, middle, suffix, \
		    int, (int * errn, int fd, void * closure)) punct \
 VU_FS_NAME##c (dup2, prefix, middle, suffix, \
		    int, (int * errn, int fd, int newfd, void * closure)) punct \
 VU_FS_NAME##c (move_state, prefix, middle, suffix, \
		    int, (int * errn, int fd, int newfd, void * closure))

/* vu virtual function types.
 *
 * This expands to a series of typedefs such as:
 *
 * 	typedef int (*vu_access_fn) (int * errn, char * path, int mode);
 */
#define VU_FS_NAME_TYPEDEF(name, prefix, middle, suffix, ret, proto) typedef ret (*vu_##name##_fn) proto
VU_MAP_FS_NAMES(_TYPEDEF, , , , ;);


/*c struct vu_fs_discipline;
 * 
 * This type is a virtual function table whose fields point to
 * functions that implement the unix file-system discipline with VU
 * calling conventions.  For each file-system function or system call,
 * there is a corresponding VU function and VU function type that
 * matches that function, except for the addition of one or two
 * parameters.  For example:
 *
 *	// unix:
 *	//
 *	int open (const char * pathname, int flags, mode_t mode);
 *
 *	// vu function:
 *	//
 *	int vu_open (int * errn, char * pathname, int flags, int mode);
 * 
 * 	// vu function type:
 *	//
 *	typedef int (*vu_open_fn) (int *, char *, int, int, void * closure);
 * 
 * For each such function type, this structure has a corresponding
 * field:
 *
 *	// field of struct vu_fs_discipline:
 *	//
 *	vu_open_fn open;
 * 
 * The order of the fields is unspecified.  To staticly initialize a
 * structure of this type, use the macro VU_FS_DISCIPLINE_INITIALIZERS
 * and name your functions in a systematic way.  For example, if
 * your VU virtual function implementations have names like:
 *
 *	vu_sys_open
 *	vu_sys_close
 *	...
 *
 * You can initialize a `struct vu_fs_discipline' this way:
 *
 *	struct vu_fs_discipline vu_system_fs_vtable
 *	  = { VU_FS_DISCIPLINE_INITIALIZERS (vu_sys_) };
 *
 */
#define VU_FS_NAME_STRUCT_FIELD(name, prefix, middle, suffix, ret, proto) prefix##name##middle name##suffix
struct vu_fs_discipline
{
  VU_MAP_FS_NAMES (_STRUCT_FIELD,  vu_,_fn , , ;);
};


#define VU_FS_NAME_INITIALIZER(name, prefix, middle, suffix, ret, proto)	prefix##name,
#define VU_FS_DISCIPLINE_INITIALIZERS(PREFIX) 					VU_MAP_FS_NAMES (_INITIALIZER, PREFIX, , , )

/*c struct vu_handler;
 * 
 * For each VU namespace handler and file-descriptor handler, there
 * is a `struct vu_handler' that indicates the `vtable' and `closure'
 * to use for that portion of the file-system.
 */
struct vu_handler
{
  struct vu_fs_discipline * vtable;
  void * closure;
};


/****************************************************************
 * Internal definitions
 * 
 */

struct _vu_namespace_handler
{
  regex_t * preg;
  int eflags;
  struct vu_handler handler;
  t_uchar * name;
  t_uchar ** doc;
};


extern struct _vu_namespace_handler * _vu_fs_handlers;
extern struct _vu_namespace_handler * _vu_optional_fs_handlers;


/* automatically generated __STDC__ prototypes */
extern void vu_push_name_handler (t_uchar * name,
				  t_uchar ** doc,
				  regex_t * preg,
				  int eflags,
				  struct vu_fs_discipline * vtable,
				  void * closure,
				  int is_optional);
extern int vu_enable_optional_name_handler (t_uchar * name);
extern void vu_set_fd_handler (int fd,
			       struct vu_fs_discipline * vtable,
			       void * closure);
extern void vu_reinit_after_unexec (void);
extern int vu_move_state (int * errn, int fd, int newfd);
extern struct vu_handler * vu_path_dispatch (char * path);
extern struct vu_handler * vu_fd_dispatch (int fd);
extern struct vu_handler * vu_dir_dispatch (DIR * dir);
extern int vu_access (int * errn, char * path, int mode);
extern int vu_chdir (int * errn, char * path);
extern int vu_chmod (int * errn, char * path, int mode);
extern int vu_chown (int * errn, char * path, int owner, int group);
extern int vu_chroot (int * errn, char * path);
extern int vu_close (int * errn, int fd);
extern int vu_closedir (int * errn, DIR * dir);
extern int vu_fchdir (int * errn, int fd);
extern int vu_fchmod (int * errn, int fd, int mode);
extern int vu_fchown (int * errn, int fd, int owner, int group);
extern int vu_fstat (int * errn, int fd, struct stat * buf);
extern int vu_fsync (int * errn, int fd);
extern int vu_ftruncate (int * errn, int fd, off_t where);
extern int vu_link (int * errn, char * from, char * to);
extern off_t vu_lseek (int * errn, int fd, off_t offset, int whence);
extern int vu_lstat (int * errn, char * path, struct stat * buf);
extern int vu_mkdir (int * errn, char * path, int mode);
extern int vu_open (int * errn, char * path, int flags, int mode);
extern int vu_dir_fd (DIR * dir);
extern int vu_opendir (int * errn, DIR ** retv,  char * path);
extern ssize_t vu_read (int * errn, int fd, char * buf, size_t count);
extern ssize_t vu_read_retry (int * errn, int fd, char * buf, size_t count);
extern int vu_readdir (int * errn,
		       struct alloc_limits * limits,
		       char ** file_ret,
		       DIR * dir);
extern int vu_readlink (int * errn, char * path, char * buf, int bufsize);
extern int vu_rename (int * errn, char * from, char * to);
extern int vu_rmdir (int * errn, char * path);
extern int vu_stat (int * errn, char * path, struct stat * buf);
extern int vu_symlink (int * errn, char * from, char * to);
extern int vu_truncate (int * errn, char * path, off_t where);
extern int vu_unlink (int * errn, char * path);
extern int vu_utime (int * errn, char * path, struct utimbuf * times);
extern ssize_t vu_write (int * errn, int fd, char * buf, size_t count);
extern ssize_t vu_write_retry (int * errn, int fd, char * buf, size_t count);
extern int vu_fcntl (int * errn, int fd, int cmd, long arg);
extern int vu_dup (int * errn, int fd);
extern int vu_dup2 (int * errn, int fd, int newfd);
#endif /* INCLUDE__VU__VU_H */
