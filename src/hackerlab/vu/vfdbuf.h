/* vfdbuf.h - VU buffered I/O decls
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU__VFDBUF_H
#define INCLUDE__VU__VFDBUF_H


#include "hackerlab/vu/vu.h"


extern struct vu_fs_discipline vfdbuf_vtable;

enum vfdbuf_buffer_flags
{
  vfdbuf_add_zero_byte = 1,
  vfdbuf_auto_shift = 2
};


/* automatically generated __STDC__ prototypes */
extern void vfdbuf_flush_all(void);
extern int vfdbuf_buffer_fd (int * errn, int fd, long bufsize, int flags, int buffer_flags);
extern int vfdbuf_set_buffer (int * errn,
			      int fd,
			      t_uchar * buffer,
			      long bufsize,
			      long read_write_pos,
			      long buffered,
			      int add_zero_byte,
			      void (*free_buffer)(t_uchar * buf, void * closure),
			      void * free_buffer_closure);
extern int vfdbuf_unbuffer_fd (int * errn, int fd);
extern int vfdbuf_is_buffered (int fd);
extern int vfdbuf_set_dont_flush (int * errn, int fd, int setting);
extern int vfdbuf_is_dont_flush (int * errn, int fd);
extern int vfdbuf_set_auto_shift (int * errn, int fd, int setting);
extern int vfdbuf_is_auto_shift (int * errn, int fd);
extern int vfdbuf_getbuf (int * errn,
			  t_uchar ** buffer,
			  long * bufsize,
			  t_uchar ** read_write_position,
			  long * buffered,
			  int * has_zero_byte,
			  int fd);
extern int vfdbuf_takebuf (int * errn,
			   t_uchar ** bufbase,
			   long * bufsize,
			   t_uchar ** read_write_pos,
			   long * buffered,
			   int * has_zero_byte,
			   int fd,
			   void (*free_buffer)(t_uchar * buf, void * closure),
			   void * free_buffer_closure);
extern int vfdbuf_shift (int * errn, int fd, long amt);
extern int vfdbuf_advance (int * errn, int fd, long amt);
extern int vfdbuf_flush (int * errn, int fd);
extern int vfdbuf_return (int * errn, int fd, t_uchar * str, long len);
extern ssize_t vfdbuf_more (int * errn,
			    t_uchar ** buffer,
			    long * bufsize,
			    t_uchar ** read_write_pos,
			    long * buffered,
			    int * has_zero_byte,
			    int fd,
			    long opt_amt);
extern int vfdbuf_is_eof (int * errn, int fd);
extern int vfdbuf_clear_eof (int * errn, int fd);
extern int vfdbuf_access (int * errn, char * path, int mode, void * closure);
extern int vfdbuf_chdir (int * errn, char * path, void * closure);
extern int vfdbuf_chmod (int * errn, char * path, int mode, void * closure);
extern int vfdbuf_chown (int * errn, char * path, int owner, int group, void * closure);
extern int vfdbuf_chroot (int * errn, char * path, void * closure);
extern int vfdbuf_closedir (int * errn, DIR * dir, void * closure);
extern int vfdbuf_fchdir (int * errn, int fd, void * closure);
extern int vfdbuf_fchmod (int * errn, int fd, int mode, void * closure);
extern int vfdbuf_fchown (int * errn, int fd, int owner, int group, void * closure);
extern int vfdbuf_fstat (int * errn, int fd, struct stat * buf, void * closure);
extern int vfdbuf_link (int * errn, char * from, char * to, void * closure);
extern int vfdbuf_lstat (int * errn, char * path, struct stat * buf, void * closure);
extern int vfdbuf_mkdir (int * errn, char * path, int mode, void * closure);
extern int vfdbuf_open (int * errn, char * path, int flags, int mode, void * closure);
extern int vfdbuf_opendir (int * errn, DIR ** retv,  char * path, void * closure);
extern int vfdbuf_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure);
extern int vfdbuf_readlink (int * errn, char * path, char * buf, int bufsize, void * closure);
extern int vfdbuf_rename (int * errn, char * from, char * to, void * closure);
extern int vfdbuf_rmdir (int * errn, char * path, void * closure);
extern int vfdbuf_stat  (int * errn, char * path, struct stat * buf, void * closure);
extern int vfdbuf_symlink (int * errn, char * from, char * to, void * closure);
extern int vfdbuf_truncate (int * errn, char * path, off_t where, void * closure);
extern int vfdbuf_unlink (int * errn, char * path, void * closure);
extern int vfdbuf_utime (int * errn, char * path, struct utimbuf * times, void * closure);
extern int vfdbuf_fcntl (int * errn, int fd, int cmd, long arg, void * closure);
extern int vfdbuf_dup (int * errn, int fd, void * closure);
extern int vfdbuf_dup2 (int * errn, int fd, int newfd, void * closure);
extern int vfdbuf_move_state (int * errn, int fd, int newfd, void * closure);
extern int vfdbuf_close (int * errn, int fd, void * closure);
extern int vfdbuf_fsync (int * errn, int fd, void * closure);
extern int vfdbuf_ftruncate (int * errn, int fd, off_t where, void * closure);
extern ssize_t vfdbuf_read (int * errn, int fd, char * buf, size_t count, void * closure);
extern ssize_t vfdbuf_write (int * errn, int fd, char * buf, size_t count, void * closure);
extern off_t vfdbuf_lseek (int * errn, int fd, off_t offset, int whence, void * closure);
#endif  /* INCLUDE__VU__VFDBUF_H */
