/* tag: Tom Lord Tue Dec  4 14:41:40 2001 (safe-vfdbuf.h)
 */
/* safe-vfdbuf.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__VU__SAFE_VFDBUF_H
#define INCLUDE__VU__SAFE_VFDBUF_H


#include "hackerlab/vu/vfdbuf.h"


/* automatically generated __STDC__ prototypes */
extern void safe_buffer_fd (int fd, long bufsize, int flags, int buffer_flags);
extern void safe_set_buffer (int fd,
			     t_uchar * buffer,
			     long bufsize,
			     long read_write_pos,
			     long buffered,
			     int add_zero_byte,
			     void (*free_buffer)(t_uchar * buf, void * closure),
			     void * free_buffer_closure);
extern void safe_set_dont_flush (int fd, int setting);
extern void safe_unbuffer (int fd);
extern int safe_is_dont_flush (int fd);
extern void safe_set_auto_shift (int fd, int setting);
extern int safe_is_auto_shift (int fd);
extern void safe_getbuf (t_uchar ** buffer,
			 long * bufsize,
			 t_uchar ** read_write_position,
			 long * buffered,
			 int * has_zero_byte,
			 int fd);
extern void safe_takebuf (t_uchar ** bufbase,
			  long * bufsize,
			  t_uchar ** read_write_pos,
			  long * buffered,
			  int * has_zero_byte,
			  int fd,
			  void (*free_buffer)(t_uchar * buf, void * closure),
			  void * free_buffer_closure);
extern void safe_shift (int fd, long amt);
extern void safe_advance (int fd, long amt);
extern void safe_flush (int fd);
extern void safe_return (int fd, t_uchar * str, long len);
extern ssize_t safe_more (t_uchar ** buffer,
			  long * bufsize,
			  t_uchar ** read_write_pos,
			  long * buffered,
			  int * has_zero_byte,
			  int fd,
			  long opt_amt);
extern int safe_is_eof (int fd);
extern void safe_clear_eof (int fd);
#endif  /* INCLUDE__VU__SAFE_VFDBUF_H */
