/* reserv.c - reserved and pseudo file descriptors
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/os/limits.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/vu-bad-arg.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/reserv.h"

/************************************************************************
 *(h0 "Reserved File Descriptors and Pseudo-Descriptors"
 *    :include ("vu/reserv.h"))
 *
 * The functions in this section allow programs to allocate file
 * descriptors and pseudo-descriptors.  A pseudo-descriptor is an
 * integer outside of the range of descriptors used by the kernel
 * (`0..(OPEN_MAX - 1)').  Pseudo-descriptors are densely packed
 * (allocated sequentially beginning with `OPEN_MAX') and are suitable
 * for use with the function `vu_set_fd_handler' (see xref:"A Virtual
 * Unix File-System Interface").
 */



/* A dynamically sized array of available pseudo-descriptors.
 */
static int * free_reserved_pseudo  = 0;

/* The lowest-numbered pseudo-descriptor (== OPEN_MAX).
 */
static int first_pseudo;


/* The lowest-numbered pseudo-descriptor that has never been allocated.
 */
static int next_pseudo;


/* VU functions
 */

static void * reserve_pseudo_make_closure (void * closure);
static void reserve_pseudo_free_closure (void * closure);
static int reserve_pseudo_close (int * errn, int fd, void * closure);

/* #define reserve_pseudo_make_closure	(vu_make_closure_fn)vu_bad_arg */
/* #define reserve_pseudo_free_closure	(vu_free_closure_fn)vu_bad_arg */
#define reserve_pseudo_access		(vu_access_fn)vu_bad_arg
#define reserve_pseudo_chdir		(vu_chdir_fn)vu_bad_arg
#define reserve_pseudo_chmod		(vu_chmod_fn)vu_bad_arg
#define reserve_pseudo_chown		(vu_chown_fn)vu_bad_arg
#define reserve_pseudo_chroot		(vu_chroot_fn)vu_bad_arg
/* #define reserve_pseudo_close		(vu_close_fn)vu_bad_arg */
#define reserve_pseudo_closedir		(vu_closedir_fn)vu_bad_arg
#define reserve_pseudo_fchdir		(vu_fchdir_fn)vu_bad_arg
#define reserve_pseudo_fchmod		(vu_fchmod_fn)vu_bad_arg
#define reserve_pseudo_fchown		(vu_fchown_fn)vu_bad_arg
#define reserve_pseudo_fstat		(vu_fstat_fn)vu_bad_arg
#define reserve_pseudo_fsync		(vu_fsync_fn)vu_bad_arg
#define reserve_pseudo_ftruncate	(vu_ftruncate_fn)vu_bad_arg
#define reserve_pseudo_link		(vu_link_fn)vu_bad_arg
#define reserve_pseudo_lseek		(vu_lseek_fn)vu_bad_arg
#define reserve_pseudo_lstat		(vu_lstat_fn)vu_bad_arg
#define reserve_pseudo_mkdir		(vu_mkdir_fn)vu_bad_arg
#define reserve_pseudo_open		(vu_open_fn)vu_bad_arg
#define reserve_pseudo_opendir		(vu_opendir_fn)vu_bad_arg
#define reserve_pseudo_read		(vu_read_fn)vu_bad_arg
#define reserve_pseudo_readdir		(vu_readdir_fn)vu_bad_arg
#define reserve_pseudo_readlink		(vu_readlink_fn)vu_bad_arg
#define reserve_pseudo_rename		(vu_rename_fn)vu_bad_arg
#define reserve_pseudo_rmdir		(vu_rmdir_fn)vu_bad_arg
#define reserve_pseudo_stat		(vu_stat_fn)vu_bad_arg
#define reserve_pseudo_symlink		(vu_symlink_fn)vu_bad_arg
#define reserve_pseudo_truncate		(vu_truncate_fn)vu_bad_arg
#define reserve_pseudo_unlink		(vu_unlink_fn)vu_bad_arg
#define reserve_pseudo_utime		(vu_utime_fn)vu_bad_arg
#define reserve_pseudo_write		(vu_write_fn)vu_bad_arg
#define reserve_pseudo_fcntl		(vu_fcntl_fn)vu_bad_arg
#define reserve_pseudo_dup		(vu_dup_fn)vu_bad_arg
#define reserve_pseudo_dup2		(vu_dup2_fn)vu_bad_arg
#define reserve_pseudo_move_state	(vu_dup2_fn)vu_bad_arg

static struct vu_fs_discipline pseudo_fd_functions \
  = { VU_FS_DISCIPLINE_INITIALIZERS (reserve_pseudo_) };


/*(c reserv)
 * int reserv (int * errn, int flags);
 * 
 * Allocate a file descriptor by opening `"/dev/null"' by using
 * `vu_open' in the manner descibed by `flags' which may be one of
 * `O_RDONLY', `O_RDWR' etc.
 * 
 * (See xref:"vu_open".)
 */
int
reserv (int * errn, int flags)
{
  return vu_open (errn, "/dev/null", flags, 0);
}




static void
init_reserv ()
{
  static int initialized = 0;
  
  if (!initialized)
    {
      /* Is there really a guarantee that descriptors
       * are densly packed, starting at 0?  If not, 
       * this is bogus.
       */
      next_pseudo = sysconf (_SC_OPEN_MAX);
      if (next_pseudo < 0)
	panic ("I/O error calling sysconf (_SC_OPEN_MAX);\n");
/*
      next_pseudo = getdtablesize ();
      if (next_pseudo < 0)
	next_pseudo = OPEN_MAX;
*/
      first_pseudo = next_pseudo;
      initialized = 1;
    }
}

/*(c reserv_pseudo)
 * int reserv_pseudo (int * errn, int flags);
 * 
 * Reserve a pseudo file descriptor suitable for use with
 * `vu_set_fd_handler'.  A pseudo file descriptor can be used with the
 * vu file system functions and is guaranteed not to be the same as
 * any real file descriptor.  (see xref:"A Virtual Unix File-System
 * Interface")
 */
int
reserv_pseudo (int * errn, int flags)
{
  int answer;

  init_reserv ();

  if (ar_size ((void *)free_reserved_pseudo, lim_use_must_malloc, sizeof (*free_reserved_pseudo)))
    answer = *(int *)ar_pop ((void **)&free_reserved_pseudo, lim_use_must_malloc, sizeof (int));
  else
    answer = ++next_pseudo;

  vu_set_fd_handler (answer, &pseudo_fd_functions, 0);
  return answer;
}


/*(c reserv_pseudo_ge_n)
 * int reserv_pseudo_ge_n (int * errn, int n, int flags);
 * 
 * Reserve a pseudo file descriptor greater than or equal to `n'
 * suitable for use with `vu_set_fd_handler'.  A pseudo file
 * descriptor can be used with the vu file system functions and is
 * guaranteed not to be the same as any real file descriptor.  (see
 * xref:"A Virtual Unix File-System Interface")
 *
 * This function is useful for implementing `vu_fcntl' with a `cmd'
 * argument `F_DUPFD'.
 */
int
reserv_pseudo_ge_n (int * errn, int n, int flags)
{
  int s;
  int answer;
  
  init_reserv ();

  if (n < first_pseudo)
    n = first_pseudo;

  answer = -1;
  s = ar_size ((void *)free_reserved_pseudo, lim_use_must_malloc, sizeof (*free_reserved_pseudo));
  if (s)
    {
      int x;
      for (x = 0; x < s; ++x)
	if (   (free_reserved_pseudo[x] >= n)
	    && (free_reserved_pseudo[x] < free_reserved_pseudo[s - 1]))
	  {
	    int tmp;
	    tmp = free_reserved_pseudo[x];
	    free_reserved_pseudo[x] = free_reserved_pseudo[s - 1];
	    free_reserved_pseudo[s - 1] = tmp;
	  }
    }
  
  if (!s || (free_reserved_pseudo[s - 1] < n))
    {
      while (next_pseudo < n)
	{
	  *(int *)ar_push ((void **)&free_reserved_pseudo, lim_use_must_malloc, sizeof (int)) = next_pseudo;
	  ++next_pseudo;
	}
    }
  return reserv_pseudo (errn, flags);
}


/* static void * reserve_pseudo_make_closure (void * closure);
 * 
 * Do nothing. This is the `make_closure' function in the vtable
 * `pseudo_fd_functions'.
 */
static void * 
reserve_pseudo_make_closure (void * closure)
{
  return closure;
}


/* static void reserve_pseudo_free_closure (void * closure);
 * 
 * Do nothing. This is the `free_closure' function in the vtable
 * `pseudo_fd_functions'.
 */
static void
reserve_pseudo_free_closure (void * closure)
{
}

/* static int reserve_pseudo_close (int * errn, int fd, void * closure);
 * 
 * Close a pseudo-file descriptor.  This is the `close' function in
 * the vtable `pseudo_fd_functions'.
 */
static int
reserve_pseudo_close (int * errn, int fd, void * closure)
{
  *(int *)ar_push ((void **)&free_reserved_pseudo, lim_use_must_malloc, sizeof (int)) = fd;
  return 0;
}


/*(c unreserv_pseudo)
 * void unreserv_pseudo (int fd);
 * 
 * Release a pseudo-descriptor previously allocated by
 * `reserved_pseudo'.
 */
void
unreserv_pseudo (int fd)
{
  *(int *)ar_push ((void **)&free_reserved_pseudo, lim_use_must_malloc, sizeof (int)) = fd;
}
