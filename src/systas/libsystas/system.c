/* system.c - unix system functions
 *
 ****************************************************************
 * Copyright (C) 1995,1996, 1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/wait.h>
#include <signal.h>
#include <grp.h>
#include <sys/utsname.h>
#include <dirent.h>

#ifdef FD_SET

#define SELECT_TYPE fd_set
#define SELECT_SET_SIZE FD_SETSIZE

#else /* no FD_SET */

/* Define the macros to access a single-int bitmap of descriptors.  */
#define SELECT_SET_SIZE 32
#define SELECT_TYPE int
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)

#endif /* no FD_SET */

extern char ** environ;

#include "hackerlab/vu/vu.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/async.h"

/* SCM_SYSCALL retries system calls that have been interrupted (EINTR) */
#define SCM_SYSCALL(line) do{errno = 0;line;}while(errno==EINTR)



/************************************************************************
 *(h0 "System Calls and C Library Functions")
 * 
 * The procedures in this section are a more or less direct interface
 * to many of the most important unix system calls and C library
 * functions defined by popular unix standards.  Not included in this
 * section are the file-system functions. (See xref:"File System and 
 * Descriptor Procedures".)
 *
 * Functions whose name begins with "%" report system errors by
 * returning a symbol (the name of the error, as defined in
 * "errno.h").  Other kinds of error, such as passing a parameter of
 * the wrong type, cause exceptions.  This documentation refers to all
 * such functions as "system calls", regardless of whether they are
 * actually implemented by system calls on the host platform.
 *
 * The return value of a system call can be checked for error
 * conditions using `errno?'.  The error number can be recovered using
 * `errno->integer' and an error message, provided by the host
 * platform, can be dervided using `strerror'.
 * 
 * The procedure `%%' is useful for calling a system call and
 * converting error returns into exceptions.  In addition, `%%' will
 * retry system calls that are interrupted by a signal.  (`%e' can be
 * used to convert errors to exceptions without retrying system calls
 * and `%i' can be used to retry system calls without converting
 * errors to exceptions).
 *
 * Some of these procedures return a structure where the corresponding
 * C function would return a structure. The module `(unix structures)'
 * provides functions which can read and write values from these 
 * vectors by name instead of index number. (*Note: Unix Structures.)
 */

/************************************************************************
 * CPP Constants
 * 
 * scm_cpp_constants
 */

SCM_SYMBOL (s_execarg, "exec arg");
SCM_SYMBOL (s_signal, "signal");
SCM_SYMBOL (s_waitpid_option, "waitpid-option");
SCM_SYMBOL (s_access_mode, "access-mode");
SCM_SYMBOL (s_errno_value, "errno-value");
SCM_SYMBOL (s_unrecognized_errno, "unrecognized-errno");
SCM_SYMBOL (s_exited, "exited"); 
SCM_SYMBOL (s_stopped, "stopped"); 
SCM_SYMBOL (s_unrecognized_status, "unrecognized-status"); 
SCM_SYMBOL (s_signaled, "signaled");
SCM_SYMBOL (s_EPERM, "EPERM");
SCM_SYMBOL (s_ENOENT, "ENOENT");
SCM_SYMBOL (s_ESRCH, "ESRCH");
SCM_SYMBOL (s_EINTR, "EINTR");
SCM_SYMBOL (s_EIO, "EIO");
SCM_SYMBOL (s_ENXIO, "ENXIO");
SCM_SYMBOL (s_E2BIG, "E2BIG");
SCM_SYMBOL (s_ENOEXEC, "ENOEXEC");
SCM_SYMBOL (s_EBADF, "EBADF");
SCM_SYMBOL (s_ECHILD, "ECHILD");
SCM_SYMBOL (s_EAGAIN, "EAGAIN");
SCM_SYMBOL (s_ENOMEM, "ENOMEM");
SCM_SYMBOL (s_EACCES, "EACCES");
SCM_SYMBOL (s_EFAULT, "EFAULT");
SCM_SYMBOL (s_ENOTBLK, "ENOTBLK");
SCM_SYMBOL (s_EBUSY, "EBUSY");
SCM_SYMBOL (s_EEXIST, "EEXIST");
SCM_SYMBOL (s_EXDEV, "EXDEV");
SCM_SYMBOL (s_ENODEV, "ENODEV");
SCM_SYMBOL (s_ENOTDIR, "ENOTDIR");
SCM_SYMBOL (s_EISDIR, "EISDIR");
SCM_SYMBOL (s_EINVAL, "EINVAL");
SCM_SYMBOL (s_ENFILE, "ENFILE");
SCM_SYMBOL (s_EMFILE, "EMFILE");
SCM_SYMBOL (s_ENOTTY, "ENOTTY");
SCM_SYMBOL (s_ETXTBSY, "ETXTBSY");
SCM_SYMBOL (s_EFBIG, "EFBIG");
SCM_SYMBOL (s_ENOSPC, "ENOSPC");
SCM_SYMBOL (s_ESPIPE, "ESPIPE");
SCM_SYMBOL (s_EROFS, "EROFS");
SCM_SYMBOL (s_EMLINK, "EMLINK");
SCM_SYMBOL (s_EPIPE, "EPIPE");
SCM_SYMBOL (s_EDOM, "EDOM");
SCM_SYMBOL (s_ERANGE, "ERANGE");
SCM_SYMBOL (s_EDEADLK, "EDEADLK");
SCM_SYMBOL (s_ENAMETOOLONG, "ENAMETOOLONG");
SCM_SYMBOL (s_ENOLCK, "ENOLCK");
SCM_SYMBOL (s_ENOSYS, "ENOSYS");
SCM_SYMBOL (s_ENOTEMPTY, "ENOTEMPTY");
SCM_SYMBOL (s_ELOOP, "ELOOP");
SCM_SYMBOL (s_EWOULDBLOCK, "EWOULDBLOCK");
SCM_SYMBOL (s_ENOMSG, "ENOMSG");
SCM_SYMBOL (s_EIDRM, "EIDRM");
SCM_SYMBOL (s_ECHRNG, "ECHRNG");
SCM_SYMBOL (s_EL2NSYNC, "EL2NSYNC");
SCM_SYMBOL (s_EL3HLT, "EL3HLT");
SCM_SYMBOL (s_EL3RST, "EL3RST");
SCM_SYMBOL (s_ELNRNG, "ELNRNG");
SCM_SYMBOL (s_EUNATCH, "EUNATCH");
SCM_SYMBOL (s_ENOCSI, "ENOCSI");
SCM_SYMBOL (s_EL2HLT, "EL2HLT");
SCM_SYMBOL (s_EBADE, "EBADE");
SCM_SYMBOL (s_EBADR, "EBADR");
SCM_SYMBOL (s_EXFULL, "EXFULL");
SCM_SYMBOL (s_ENOANO, "ENOANO");
SCM_SYMBOL (s_EBADRQC, "EBADRQC");
SCM_SYMBOL (s_EBADSLT, "EBADSLT");
SCM_SYMBOL (s_EDEADLOCK, "EDEADLOCK");
SCM_SYMBOL (s_EBFONT, "EBFONT");
SCM_SYMBOL (s_ENOSTR, "ENOSTR");
SCM_SYMBOL (s_ENODATA, "ENODATA");
SCM_SYMBOL (s_ETIME, "ETIME");
SCM_SYMBOL (s_ENOSR, "ENOSR");
SCM_SYMBOL (s_ENONET, "ENONET");
SCM_SYMBOL (s_ENOPKG, "ENOPKG");
SCM_SYMBOL (s_EREMOTE, "EREMOTE");
SCM_SYMBOL (s_ENOLINK, "ENOLINK");
SCM_SYMBOL (s_EADV, "EADV");
SCM_SYMBOL (s_ESRMNT, "ESRMNT");
SCM_SYMBOL (s_ECOMM, "ECOMM");
SCM_SYMBOL (s_EPROTO, "EPROTO");
SCM_SYMBOL (s_EMULTIHOP, "EMULTIHOP");
SCM_SYMBOL (s_EDOTDOT, "EDOTDOT");
SCM_SYMBOL (s_EBADMSG, "EBADMSG");
SCM_SYMBOL (s_EOVERFLOW, "EOVERFLOW");
SCM_SYMBOL (s_ENOTUNIQ, "ENOTUNIQ");
SCM_SYMBOL (s_EBADFD, "EBADFD");
SCM_SYMBOL (s_EREMCHG, "EREMCHG");
SCM_SYMBOL (s_ELIBACC, "ELIBACC");
SCM_SYMBOL (s_ELIBBAD, "ELIBBAD");
SCM_SYMBOL (s_ELIBSCN, "ELIBSCN");
SCM_SYMBOL (s_ELIBMAX, "ELIBMAX");
SCM_SYMBOL (s_ELIBEXEC, "ELIBEXEC");
SCM_SYMBOL (s_EILSEQ, "EILSEQ");
SCM_SYMBOL (s_ERESTART, "ERESTART");
SCM_SYMBOL (s_ESTRPIPE, "ESTRPIPE");
SCM_SYMBOL (s_EUSERS, "EUSERS");
SCM_SYMBOL (s_ENOTSOCK, "ENOTSOCK");
SCM_SYMBOL (s_EDESTADDRREQ, "EDESTADDRREQ");
SCM_SYMBOL (s_EMSGSIZE, "EMSGSIZE");
SCM_SYMBOL (s_EPROTOTYPE, "EPROTOTYPE");
SCM_SYMBOL (s_ENOPROTOOPT, "ENOPROTOOPT");
SCM_SYMBOL (s_EPROTONOSUPPORT, "EPROTONOSUPPORT");
SCM_SYMBOL (s_ESOCKTNOSUPPORT, "ESOCKTNOSUPPORT");
SCM_SYMBOL (s_EOPNOTSUPP, "EOPNOTSUPP");
SCM_SYMBOL (s_EPFNOSUPPORT, "EPFNOSUPPORT");
SCM_SYMBOL (s_EAFNOSUPPORT, "EAFNOSUPPORT");
SCM_SYMBOL (s_EADDRINUSE, "EADDRINUSE");
SCM_SYMBOL (s_EADDRNOTAVAIL, "EADDRNOTAVAIL");
SCM_SYMBOL (s_ENETDOWN, "ENETDOWN");
SCM_SYMBOL (s_ENETUNREACH, "ENETUNREACH");
SCM_SYMBOL (s_ENETRESET, "ENETRESET");
SCM_SYMBOL (s_ECONNABORTED, "ECONNABORTED");
SCM_SYMBOL (s_ECONNRESET, "ECONNRESET");
SCM_SYMBOL (s_ENOBUFS, "ENOBUFS");
SCM_SYMBOL (s_EISCONN, "EISCONN");
SCM_SYMBOL (s_ENOTCONN, "ENOTCONN");
SCM_SYMBOL (s_ESHUTDOWN, "ESHUTDOWN");
SCM_SYMBOL (s_ETOOMANYREFS, "ETOOMANYREFS");
SCM_SYMBOL (s_ETIMEDOUT, "ETIMEDOUT");
SCM_SYMBOL (s_ECONNREFUSED, "ECONNREFUSED");
SCM_SYMBOL (s_EHOSTDOWN, "EHOSTDOWN");
SCM_SYMBOL (s_EHOSTUNREACH, "EHOSTUNREACH");
SCM_SYMBOL (s_EALREADY, "EALREADY");
SCM_SYMBOL (s_EINPROGRESS, "EINPROGRESS");
SCM_SYMBOL (s_ESTALE, "ESTALE");
SCM_SYMBOL (s_EUCLEAN, "EUCLEAN");
SCM_SYMBOL (s_ENOTNAM, "ENOTNAM");
SCM_SYMBOL (s_ENAVAIL, "ENAVAIL");
SCM_SYMBOL (s_EISNAM, "EISNAM");
SCM_SYMBOL (s_EREMOTEIO, "EREMOTEIO");
SCM_SYMBOL (s_EDQUOT, "EDQUOT");

SCM_KEYWORD (kw_pid, "pid");
SCM_KEYWORD (kw_state, "state");
SCM_KEYWORD (kw_exit_status, "exit-status");
SCM_KEYWORD (kw_term_signal, "term-signal");
SCM_KEYWORD (kw_stop_signal, "stop-signal");

SCM_KEYWORD (kw_name, "name");
SCM_KEYWORD (kw_passwd, "passwd");
SCM_KEYWORD (kw_uid, "uid");
SCM_KEYWORD (kw_gid, "gid");
SCM_KEYWORD (kw_gecos, "gecos");
SCM_KEYWORD (kw_dir, "dir");
SCM_KEYWORD (kw_shell, "shell");

SCM_KEYWORD (kw_sysname, "sysname");
SCM_KEYWORD (kw_nodename, "nodename");
SCM_KEYWORD (kw_release, "release");
SCM_KEYWORD (kw_version, "version");
SCM_KEYWORD (kw_machine, "machine");

SCM_KEYWORD (kw_mem, "mem");



/* scm_makerrno
 * 
 * Construct a new errno object for a given error number.
 */
SCM
scm_makerrno (int x)
{
  SCM_INTS_NESTED;

  switch (x)
    {
    default:
      return s_unrecognized_errno;
#ifdef EPERM
    case EPERM:
      return s_EPERM;
#endif
#ifdef ENOENT
    case ENOENT:
      return s_ENOENT;
#endif
#ifdef ESRCH
    case ESRCH:
      return s_ESRCH;
#endif
#ifdef EINTR
    case EINTR:
      return s_EINTR;
#endif
#ifdef EIO
    case EIO:
      return s_EIO;
#endif
#ifdef ENXIO
    case ENXIO:
      return s_ENXIO;
#endif
#ifdef E2BIG
    case E2BIG:
      return s_E2BIG;
#endif
#ifdef ENOEXEC
    case ENOEXEC:
      return s_ENOEXEC;
#endif
#ifdef EBADF
    case EBADF:
      return s_EBADF;
#endif
#ifdef ECHILD
    case ECHILD:
      return s_ECHILD;
#endif
#ifdef ENOMEM
    case ENOMEM:
      return s_ENOMEM;
#endif
#ifdef EACCES
    case EACCES:
      return s_EACCES;
#endif
#ifdef EFAULT
    case EFAULT:
      return s_EFAULT;
#endif
#ifdef ENOTBLK
    case ENOTBLK:
      return s_ENOTBLK;
#endif
#ifdef EBUSY
    case EBUSY:
      return s_EBUSY;
#endif
#ifdef EEXIST
    case EEXIST:
      return s_EEXIST;
#endif
#ifdef EXDEV
    case EXDEV:
      return s_EXDEV;
#endif
#ifdef ENODEV
    case ENODEV:
      return s_ENODEV;
#endif
#ifdef ENOTDIR
    case ENOTDIR:
      return s_ENOTDIR;
#endif
#ifdef EISDIR
    case EISDIR:
      return s_EISDIR;
#endif
#ifdef EINVAL
    case EINVAL:
      return s_EINVAL;
#endif
#ifdef ENFILE
    case ENFILE:
      return s_ENFILE;
#endif
#ifdef EMFILE
    case EMFILE:
      return s_EMFILE;
#endif
#ifdef ENOTTY
    case ENOTTY:
      return s_ENOTTY;
#endif
#ifdef ETXTBSY
    case ETXTBSY:
      return s_ETXTBSY;
#endif
#ifdef EFBIG
    case EFBIG:
      return s_EFBIG;
#endif
#ifdef ENOSPC
    case ENOSPC:
      return s_ENOSPC;
#endif
#ifdef ESPIPE
    case ESPIPE:
      return s_ESPIPE;
#endif
#ifdef EROFS
    case EROFS:
      return s_EROFS;
#endif
#ifdef EMLINK
    case EMLINK:
      return s_EMLINK;
#endif
#ifdef EPIPE
    case EPIPE:
      return s_EPIPE;
#endif
#ifdef EDOM
    case EDOM:
      return s_EDOM;
#endif
#ifdef ERANGE
    case ERANGE:
      return s_ERANGE;
#endif
#ifdef EDEADLK
    case EDEADLK:
      return s_EDEADLK;
#endif
#ifdef ENAMETOOLONG
    case ENAMETOOLONG:
      return s_ENAMETOOLONG;
#endif
#ifdef ENOLCK
    case ENOLCK:
      return s_ENOLCK;
#endif
#ifdef ENOSYS
    case ENOSYS:
      return s_ENOSYS;
#endif
#ifdef ENOTEMPTY
    case ENOTEMPTY:
      return s_ENOTEMPTY;
#endif
#ifdef ELOOP
    case ELOOP:
      return s_ELOOP;
#endif
#ifdef EAGAIN
    case EAGAIN:
      return s_EAGAIN;
#endif
#ifdef EWOULDBLOCK
#ifndef EAGAIN
    case EWOULDBLOCK:
      return s_EWOULDBLOCK;
#else /* #ifdef EAGAIN */
#if EAGAIN != EWOULDBLOCK
    case EWOULDBLOCK:
      return s_EWOULDBLOCK;
#endif /* EAGAIN != EWOULDBLOCK */
#endif /* #ifdef EAGAIN */
#endif /* #ifdef EWOULDBLOCK */
#ifdef ENOMSG
    case ENOMSG:
      return s_ENOMSG;
#endif
#ifdef EIDRM
    case EIDRM:
      return s_EIDRM;
#endif
#ifdef ECHRNG
    case ECHRNG:
      return s_ECHRNG;
#endif
#ifdef EL2NSYNC
    case EL2NSYNC:
      return s_EL2NSYNC;
#endif
#ifdef EL3HLT
    case EL3HLT:
      return s_EL3HLT;
#endif
#ifdef EL3RST
    case EL3RST:
      return s_EL3RST;
#endif
#ifdef ELNRNG
    case ELNRNG:
      return s_ELNRNG;
#endif
#ifdef EUNATCH
    case EUNATCH:
      return s_EUNATCH;
#endif
#ifdef ENOCSI
    case ENOCSI:
      return s_ENOCSI;
#endif
#ifdef EL2HLT
    case EL2HLT:
      return s_EL2HLT;
#endif
#ifdef EBADE
    case EBADE:
      return s_EBADE;
#endif
#ifdef EBADR
    case EBADR:
      return s_EBADR;
#endif
#ifdef EXFULL
    case EXFULL:
      return s_EXFULL;
#endif
#ifdef ENOANO
    case ENOANO:
      return s_ENOANO;
#endif
#ifdef EBADRQC
    case EBADRQC:
      return s_EBADRQC;
#endif
#ifdef EBADSLT
    case EBADSLT:
      return s_EBADSLT;
#endif
#if defined(EDEADLOCK) && (EDEADLOCK != EDEADLK)
    case EDEADLOCK:
      return s_EDEADLOCK;
#endif
#ifdef EBFONT
    case EBFONT:
      return s_EBFONT;
#endif
#ifdef ENOSTR
    case ENOSTR:
      return s_ENOSTR;
#endif
#ifdef ENODATA
    case ENODATA:
      return s_ENODATA;
#endif
#ifdef ETIME
    case ETIME:
      return s_ETIME;
#endif
#ifdef ENOSR
    case ENOSR:
      return s_ENOSR;
#endif
#ifdef ENONET
    case ENONET:
      return s_ENONET;
#endif
#ifdef ENOPKG
    case ENOPKG:
      return s_ENOPKG;
#endif
#ifdef EREMOTE
    case EREMOTE:
      return s_EREMOTE;
#endif
#ifdef ENOLINK
    case ENOLINK:
      return s_ENOLINK;
#endif
#ifdef EADV
    case EADV:
      return s_EADV;
#endif
#ifdef ESRMNT
    case ESRMNT:
      return s_ESRMNT;
#endif
#ifdef ECOMM
    case ECOMM:
      return s_ECOMM;
#endif
#ifdef EPROTO
    case EPROTO:
      return s_EPROTO;
#endif
#ifdef EMULTIHOP
    case EMULTIHOP:
      return s_EMULTIHOP;
#endif
#ifdef EDOTDOT
    case EDOTDOT:
      return s_EDOTDOT;
#endif
#ifdef EBADMSG
    case EBADMSG:
      return s_EBADMSG;
#endif
#ifdef EOVERFLOW
    case EOVERFLOW:
      return s_EOVERFLOW;
#endif
#ifdef ENOTUNIQ
    case ENOTUNIQ:
      return s_ENOTUNIQ;
#endif
#ifdef EBADFD
    case EBADFD:
      return s_EBADFD;
#endif
#ifdef EREMCHG
    case EREMCHG:
      return s_EREMCHG;
#endif
#ifdef ELIBACC
    case ELIBACC:
      return s_ELIBACC;
#endif
#ifdef ELIBBAD
    case ELIBBAD:
      return s_ELIBBAD;
#endif
#ifdef ELIBSCN
    case ELIBSCN:
      return s_ELIBSCN;
#endif
#ifdef ELIBMAX
    case ELIBMAX:
      return s_ELIBMAX;
#endif
#ifdef ELIBEXEC
    case ELIBEXEC:
      return s_ELIBEXEC;
#endif
#ifdef EILSEQ
    case EILSEQ:
      return s_EILSEQ;
#endif
#ifdef ERESTART
    case ERESTART:
      return s_ERESTART;
#endif
#ifdef ESTRPIPE
    case ESTRPIPE:
      return s_ESTRPIPE;
#endif
#ifdef EUSERS
    case EUSERS:
      return s_EUSERS;
#endif
#ifdef ENOTSOCK
    case ENOTSOCK:
      return s_ENOTSOCK;
#endif
#ifdef EDESTADDRREQ
    case EDESTADDRREQ:
      return s_EDESTADDRREQ;
#endif
#ifdef EMSGSIZE
    case EMSGSIZE:
      return s_EMSGSIZE;
#endif
#ifdef EPROTOTYPE
    case EPROTOTYPE:
      return s_EPROTOTYPE;
#endif
#ifdef ENOPROTOOPT
    case ENOPROTOOPT:
      return s_ENOPROTOOPT;
#endif
#ifdef EPROTONOSUPPORT
    case EPROTONOSUPPORT:
      return s_EPROTONOSUPPORT;
#endif
#ifdef ESOCKTNOSUPPORT
    case ESOCKTNOSUPPORT:
      return s_ESOCKTNOSUPPORT;
#endif
#ifdef EOPNOTSUPP
    case EOPNOTSUPP:
      return s_EOPNOTSUPP;
#endif
#ifdef EPFNOSUPPORT
    case EPFNOSUPPORT:
      return s_EPFNOSUPPORT;
#endif
#ifdef EAFNOSUPPORT
    case EAFNOSUPPORT:
      return s_EAFNOSUPPORT;
#endif
#ifdef EADDRINUSE
    case EADDRINUSE:
      return s_EADDRINUSE;
#endif
#ifdef EADDRNOTAVAIL
    case EADDRNOTAVAIL:
      return s_EADDRNOTAVAIL;
#endif
#ifdef ENETDOWN
    case ENETDOWN:
      return s_ENETDOWN;
#endif
#ifdef ENETUNREACH
    case ENETUNREACH:
      return s_ENETUNREACH;
#endif
#ifdef ENETRESET
    case ENETRESET:
      return s_ENETRESET;
#endif
#ifdef ECONNABORTED
    case ECONNABORTED:
      return s_ECONNABORTED;
#endif
#ifdef ECONNRESET
    case ECONNRESET:
      return s_ECONNRESET;
#endif
#ifdef ENOBUFS
    case ENOBUFS:
      return s_ENOBUFS;
#endif
#ifdef EISCONN
    case EISCONN:
      return s_EISCONN;
#endif
#ifdef ENOTCONN
    case ENOTCONN:
      return s_ENOTCONN;
#endif
#ifdef ESHUTDOWN
    case ESHUTDOWN:
      return s_ESHUTDOWN;
#endif
#ifdef ETOOMANYREFS
    case ETOOMANYREFS:
      return s_ETOOMANYREFS;
#endif
#ifdef ETIMEDOUT
    case ETIMEDOUT:
      return s_ETIMEDOUT;
#endif
#ifdef ECONNREFUSED
    case ECONNREFUSED:
      return s_ECONNREFUSED;
#endif
#ifdef EHOSTDOWN
    case EHOSTDOWN:
      return s_EHOSTDOWN;
#endif
#ifdef EHOSTUNREACH
    case EHOSTUNREACH:
      return s_EHOSTUNREACH;
#endif
#ifdef EALREADY
    case EALREADY:
      return s_EALREADY;
#endif
#ifdef EINPROGRESS
    case EINPROGRESS:
      return s_EINPROGRESS;
#endif
#ifdef ESTALE
    case ESTALE:
      return s_ESTALE;
#endif
#ifdef EUCLEAN
    case EUCLEAN:
      return s_EUCLEAN;
#endif
#ifdef ENOTNAM
    case ENOTNAM:
      return s_ENOTNAM;
#endif
#ifdef ENAVAIL
    case ENAVAIL:
      return s_ENAVAIL;
#endif
#ifdef EISNAM
    case EISNAM:
      return s_EISNAM;
#endif
#ifdef EREMOTEIO
    case EREMOTEIO:
      return s_EREMOTEIO;
#endif
#ifdef EDQUOT
    case EDQUOT:
      return s_EDQUOT;
#endif ifdef
    }
}


/*(c errno?)
 * (errno? obj)
 * 
 * Return #t if `obj' is an errno object, #f otherwise.
 */
SCM_PROC (s_errno_p, "errno?", 1, 0, 0, scm_errno_p);
SCM
scm_errno_p (SCM x)
{
  SCM_INTS_UNKNOWN;
  SCM value;

  value = scm_hashq_ref (scm_cpp_constants, x, SCM_BOOL_F);
  return ((   !SCM_IS_IMMEDIATE (value)
	   && SCM_CONSP (value)
	   && (s_errno_value == SCM_CAR (value))
	   && SCM_INUMP (SCM_CDR (value)))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c errno->integer)
 * (errno->integer errno)
 * 
 * Return the error number associated with an errno object.
 */
SCM_PROC (s_errno_to_integer, "errno->integer", 1, 0, 0, scm_errno_to_integer);
SCM
scm_errno_to_integer (SCM x)
{
  SCM_INTS_ENABLED;
  return scm_integer_cpp_constant (s_errno_value, x, scm_arg1, s_errno_to_integer);
}

/*(c %e)
;; %e fn . args
;; %signal-errors fn . args
;; 
;; Apply `fn' to `args'.  If `fn' returns an errno object,
;; throw a exception whose type is the name of the answer.
 */


/*(c %i)
 * %i fn . args
 * %retry-interrupted-calls fn . args
 * 
 * Apply `fn' to `args'.  If `fn' returns an errno object,
 * and the indicated error is `EINTR', retry the call to "fn".
 */

/*(c %%)
 * %% fn . args
 * %high-level-system-call fn . args
 * 
 * Apply `fn' to `args'.  If `fn' returns an errno object,
 * and the indicated error is `EINTR', retry the call to "fn".
 * 
 * If `fn' returns an errno object for some other error,
 * throw an exception whose type is the name of the error
 */

/*(c errno-exceptions-return)
 * errno-exceptions-return thunk
 * 
 * Invoke `thunk' in the context of `catch'.  If an errno exception
 * is caught, return the errno object.  If any other exception is 
 * caught, rethrow that exception.
 */



/*(c strerror)
 * (strerror error)
 * 
 * Return the system-defined error message for a given 
 * error number.  `error' must be an integer or an errno object.
 */
SCM_PROC(s_strerror, "strerror", 1, 0, 0, scm_strerror);
SCM 
scm_strerror (SCM arg)
{
  SCM_INTS_ENABLED;
  int e;
  char * it;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (arg), arg, scm_arg1, s_strerror);

  e = SCM_INUM (scm_errno_to_integer (arg));
  
  SCM_DEFER_INTS;
  it = strerror (e);
  answer = scm_makfromstr0 (it);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %getgroups)
 * (%getgroups)
 * 
 * Return the process group access list (a vector of integer id numbers).
 * See the manual page "getgroups".
 */
SCM_PROC (s_sys_getgroups, "%getgroups", 0, 0, 0, scm_sys_getgroups);
SCM
scm_sys_getgroups(void)
{
  SCM_INTS_ENABLED;
  SCM grps;
  SCM ans;
  int ngroups;

  SCM_DEFER_INTS;
  ngroups = getgroups (0, NULL);
  SCM_ALLOW_INTS;
  if (!ngroups)
    return SCM_BOOL_F;
  SCM_NEWCELL(grps);
  {
    gid_t *groups;
    int val;

    SCM_DEFER_INTS;
    groups = (gid_t *)scm_must_malloc(ngroups * sizeof(gid_t));
    val = getgroups(ngroups, groups);
    if (val < 0)
      {
	scm_must_free((char *)groups);
	ans = scm_makerrno (errno);
      }
    else
      {
	SCM_CDR (grps) = (SCM)groups;	/* set up grps as a GC protect */
	SCM_SET_LENGTH(grps, 0L + ngroups * sizeof(gid_t), scm_tc7_string);
	ans = scm_makvector (ngroups, SCM_UNDEFINED, 0, SCM_BOOL_F);
	while (--ngroups >= 0) SCM_VECTOR_ELTS(ans)[ngroups] = SCM_MAKINUM(groups[ngroups]);
	SCM_CDR (grps) = (SCM)groups;	/* to make sure grps stays around. */
	/* GC will free "groups" */
      }
    SCM_ALLOW_INTS;
    return ans;
  }
}



/* scm_pwent2scm
 * 
 * Convert a password entry to a keyword argument list.
 */
static SCM
scm_pwent2scm (struct passwd *entry)
{
  SCM_INTS_DISABLED;
  SCM result;

  
  result = scm_listify (kw_name, scm_makfromstr0 (entry->pw_name),
			kw_passwd, scm_makfromstr0 (entry->pw_passwd),
			kw_uid, scm_ulong2num ((unsigned long) entry->pw_uid),
			kw_gid, scm_ulong2num ((unsigned long) entry->pw_gid),
			kw_gecos, scm_makfromstr0 (entry->pw_gecos),
			kw_dir, (!entry->pw_dir
				 ? scm_makfromstr0 ("")
				 : scm_makfromstr0 (entry->pw_dir)),
			kw_shell, (!entry->pw_shell
				   ? scm_makfromstr0 ("")
				   : scm_makfromstr0 (entry->pw_shell)),
			SCM_UNDEFINED);
  return result;
}


/*(c getpwent)
 * (getpwent)
 * 
 * Return the next entry from the password file as a keyword argument
 * list or #f.
 *
 * See the manual page "getpwent".
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getpwent, "getpwent", 0, 0, 0, scm_getpwent);
SCM 
scm_getpwent (void)
{
  SCM_INTS_ENABLED;
  SCM result;
  struct passwd *entry;

  SCM_DEFER_INTS;
  entry = getpwent ();
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_pwent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}


/*(c setpwent)
 * (setpwent)
 * 
 * Rewind the file pointer for the password file.  
 * See the manual page "setpwent".
 */
SCM_PROC (s_setpwent, "setpwent", 0, 0, 0, scm_setpwent);
SCM
scm_setpwent (void)
{
  SCM ans;
  SCM_DEFER_INTS;
  setpwent ();
  ans = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c endpwent)
 * (endpwent)
 * 
 * Close the password file.
 * See the manual page "endpwent".
 */
SCM_PROC (s_endpwent, "endpwent", 0, 0, 0, scm_endpwent);
SCM
scm_endpwent (void)
{
  SCM ans;
  SCM_DEFER_INTS;
  endpwent ();
  ans = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return ans;
}


/*(c getpwuid)
 * (getpwuid user-id)
 * 
 * Return the password file entry for a given user id.
 * See the manual page "getpwuid".
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getpwuid, "getpwuid", 1, 0, 0, scm_getpwuid);
SCM 
scm_getpwuid (SCM user)
{
  SCM_INTS_ENABLED;
  struct passwd *entry;
  SCM result;

  SCM_ASSERT (SCM_INUMP (user), user, scm_arg1, s_getpwuid);
  SCM_DEFER_INTS;
  entry = getpwuid (SCM_INUM (user));
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_pwent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}

/*(c getpwnam)
 * (getpwnam user-name)
 * 
 * Return the password file entry for a given user name.
 * See the manual page "getpwnam".
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getpwnam, "getpwnam", 1, 0, 0, scm_getpwnam);
SCM 
scm_getpwnam (SCM user)
{
  SCM_INTS_ENABLED;
  struct passwd *entry;
  SCM result;

  SCM_ASSERT (scm_is_ro_string (user), user, scm_arg1, s_getpwnam);
  if (scm_is_ro_substr (user))
    user = scm_makfromstr (SCM_RO_CHARS (user), SCM_RO_LENGTH (user));

  SCM_DEFER_INTS;
  entry = getpwnam (SCM_RO_CHARS (user));
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_pwent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}


/* scm_grent2scm
 * 
 * Convert a groups-file entry to keyword-argument list.
 */
static SCM
scm_grent2scm (struct group *entry)
{
  SCM_INTS_DISABLED;
  SCM result;

  result = scm_listify (kw_name, scm_makfromstr0 (entry->gr_name),
			kw_passwd, scm_makfromstr0 (entry->gr_passwd),
			kw_gid, scm_ulong2num ((unsigned long) entry->gr_gid),
			kw_mem, scm_argv2scm (-1, (t_uchar **)entry->gr_mem),
			SCM_UNDEFINED);
  return result;
}


/*(c getgrent)
 * (getgrent)
 * 
 * Return the next entry from the groups file as a keyword-argument
 * list.
 * 
 * See the manual page "getgrent".
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getgrent, "getgrent", 0, 0, 0, scm_getgrent);
SCM 
scm_getgrent (void)
{
  SCM_INTS_ENABLED;
  struct group *entry;
  SCM result;

  SCM_DEFER_INTS;
  entry = getgrent ();
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_grent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}


/*(c getgrgid)
 * (getgrgid group-id)
 * 
 * Return the groups file entry for a given group id.
 * See the manual page "getgrgid".
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getgrgid, "getgrgid", 1, 0, 0, scm_getgrgid);
SCM 
scm_getgrgid (SCM name)
{
  SCM_INTS_ENABLED;
  struct group *entry;
  SCM result;

  SCM_ASSERT (SCM_INUMP (name), name, scm_arg1, s_getgrgid);

  SCM_DEFER_INTS;
  entry = getgrgid (SCM_INUM (name));
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_grent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}


/*(c getgrnam)
 * (getgrnam group-name)
 * 
 * Return the groups file entry for a given group name.
 * See the manual page "getgrnam".  Return #f if no group
 * has the given name.
 *
 * This function returns a keyword argument list.
 */
SCM_PROC (s_getgrnam, "getgrnam", 1, 0, 0, scm_getgrnam);
SCM 
scm_getgrnam (SCM name)
{
  SCM_INTS_ENABLED;
  struct group *entry;
  SCM result;

  SCM_ASSERT (scm_is_ro_string (name), name, scm_arg1, s_getgrnam);
  if (scm_is_ro_substr (name))
    name = scm_makfromstr (SCM_RO_CHARS (name), SCM_RO_LENGTH (name));

  SCM_DEFER_INTS;
  entry = getgrnam (SCM_RO_CHARS (name));
  if (!entry)
    result = SCM_BOOL_F;
  else
    result = scm_grent2scm (entry);
  SCM_ALLOW_INTS;
  return result;
}


/*(c setgrent)
 * (setgrent)
 * 
 * Rewind the file pointer for the groups file.  
 * See the manual page "setgrent".
 */
SCM_PROC (s_setgrent, "setgrent", 0, 0, 0, scm_setgrent);
SCM 
scm_setgrent (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  setgrent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


/*(c endgrent)
 * (endgrent)
 * 
 * Close the groups file.
 * See the manual page "endgrent".
 */
SCM_PROC (s_endgrent, "endgrent", 0, 0, 0, scm_endgrent);
SCM 
scm_endgrent (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  endgrent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


/*(c signal-name->integer)
 * (signal-name->integer signal)
 * 
 * Return the signal number of `signal'.  
 *
 * `signal' may already be a signal number, or it can be a symbol whose name
 * is the traditional name in C for a signal (e.g. 'SIGINT)
 */
SCM_PROC (s_signal_name_to_integer, "signal-name->integer", 1, 0, 0, scm_signal_name_to_integer);
SCM
scm_signal_name_to_integer (SCM name)
{
  return SCM_MAKINUM (scm_integer_cpp_constant (s_signal, name, scm_arg1, s_signal_name_to_integer));
}


/*(c %kill)
 * (%kill process-id signal)
 * 
 * Send the indicated signal to the indicated process.
 * `process-id' and `signal' must be integers.
 *
 * See the manual page "kill".
 */
SCM_PROC (s_sys_kill, "%kill", 2, 0, 0, scm_sys_kill);
SCM 
scm_sys_kill (SCM pid, SCM sig)
{
  SCM_INTS_ENABLED;
  int signal_number;
  int i;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (pid), pid, scm_arg1, s_sys_kill);
  signal_number = scm_integer_cpp_constant (s_signal, sig, scm_arg2, s_signal);
  SCM_DEFER_INTS;
  i = kill ((int) SCM_INUM (pid), signal_number);
  answer = i ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %killpg)
 * (%killpg process-group-id signal)
 * 
 * Send the indicated signal to the indicated process group.
 * `process-group-id' and `signal' must be integers.
 *
 * See the manual page "killpg".
 */
SCM_PROC (s_sys_killpg, "%killpg", 2, 0, 0, scm_sys_killpg);
SCM 
scm_sys_killpg (SCM pgid, SCM sig)
{
  SCM_INTS_ENABLED;
  int signal_number;
  int i;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (pgid), pgid, scm_arg1, s_sys_killpg);
  signal_number = scm_integer_cpp_constant (s_signal, sig, scm_arg2, s_signal);
  SCM_DEFER_INTS;
  i = killpg ((int) SCM_INUM (pgid), signal_number);
  answer = i ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c wait-options->integer)
 * (wait-options->integer options)
 * 
 * Return an integer which is the logical or of one or more options to the `%wait' system call.
 * `options' may be an integer (which is returned), a symbol, or a list of symbols and integers.
 * Accepted symbols share names with CPP constants (e.g. `WNOHANG').
 */
SCM_PROC (s_wait_options_to_integer, "wait-options->integer", 1, 0, 0, scm_wait_options_to_integer);
SCM
scm_wait_options_to_integer (SCM options)
{
  return scm_integer_logior_cpp_constants (s_waitpid_option, options, scm_arg1, s_waitpid_option);
}


/*(c %waitpid)
 * (%waitpid process-id options)
 * 
 * Wait for the indicated process to exit and return its status.
 * Both arguments must be integers. [!!! out of date]
 *
 * See the manual page "waitpid".
 */
SCM_PROC (s_sys_waitpid, "%waitpid", 1, 1, 0, scm_sys_waitpid);
SCM 
scm_sys_waitpid (SCM pid, SCM options)
{
  SCM_INTS_ENABLED;
  int i;
  int status;
  int ioptions;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (pid), pid, scm_arg1, s_sys_waitpid);
  if (SCM_UNBNDP (options))
    ioptions = 0;
  else
    ioptions = scm_integer_logior_cpp_constants (s_waitpid_option, options, scm_arg2, s_sys_waitpid);

  SCM_DEFER_INTS;
  i = waitpid (SCM_INUM (pid), &status, ioptions);
  if (i == -1)
    answer = scm_makerrno (errno);
  else if (i == 0)
    answer = SCM_BOOL_F;
  else if (i == -1)
    answer = scm_makerrno (errno);
  else
    {
      SCM state_sym;
      SCM status_kw;
      SCM status_val;

      if (WIFEXITED (status))
	{
	  state_sym = s_exited;
	  status_kw = kw_exit_status;
	  status_val  = scm_ulong2num ((unsigned long)WEXITSTATUS (status));
	}
      else if (WIFSIGNALED (status))
	{
	  state_sym = s_signaled;
	  status_kw = kw_term_signal;
	  status_val  = scm_ulong2num ((unsigned long)WTERMSIG (status));
	}
      else
	{
	  state_sym = s_stopped;
	  status_kw = kw_stop_signal;
	  status_val  = scm_ulong2num ((unsigned long)WSTOPSIG (status));
	}

      answer =  scm_listify (kw_pid, SCM_MAKINUM (i),
			     kw_state, state_sym,
			     status_kw, status_val,
			     SCM_UNDEFINED);
    }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getpid)
 * (getpid)
 * 
 * Return the id of the current process.
 * See the manual page "getpid".
 */
SCM_PROC (s_getpid, "getpid", 0, 0, 0, scm_getpid);
SCM 
scm_getpid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM ((unsigned long) getpid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getppid)
 * (getppid)
 * 
 * Return the parent process id of the current proces.
 * See the manual page "getppid".
 */
SCM_PROC (s_getppid, "getppid", 0, 0, 0, scm_getppid);
SCM 
scm_getppid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (0L + getppid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getuid)
 * (getuid)
 * 
 * Return the user id of the current process.
 * See the manual page "getuid".
 */
SCM_PROC (s_getuid, "getuid", 0, 0, 0, scm_getuid);
SCM 
scm_getuid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (0L + getuid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getgid)
 * (getgid)
 * 
 * Return the group id of the current process.
 * See the manual page "getgid".
 */
SCM_PROC (s_getgid, "getgid", 0, 0, 0, scm_getgid);
SCM 
scm_getgid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (0L + getgid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c geteuid)
 * (geteuid)
 * 
 * Return the effective user id of the current process.
 * See the manual page "geteuid".
 */
SCM_PROC (s_geteuid, "geteuid", 0, 0, 0, scm_geteuid);
SCM 
scm_geteuid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (0L + geteuid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getegid)
 * (getegid)
 * 
 * Return the effective group id of the current process.
 * See the manual page "getegid".
 */
SCM_PROC (s_getegid, "getegid", 0, 0, 0, scm_getegid);
SCM 
scm_getegid (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (0L + getegid ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %setuid)
 * (%setuid id)
 * 
 * Set the effective user id of the current process.
 * See the manual page "setuid".
 */
SCM_PROC (s_sys_setuid, "%setuid", 1, 0, 0, scm_sys_setuid);
SCM 
scm_sys_setuid (SCM id)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (id), id, scm_arg1, s_sys_setuid);
  SCM_DEFER_INTS;
  answer = setuid (SCM_INUM (id)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %setgid)
 * (%setgid id)
 * 
 * Set the effective group id of the current process.
 * See the manual page "setgid".
 */
SCM_PROC (s_sys_setgid, "%setgid", 1, 0, 0, scm_sys_setgid);
SCM 
scm_sys_setgid (SCM id)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (id), id, scm_arg1, s_sys_setgid);
  SCM_DEFER_INTS;
  answer = setgid (SCM_INUM (id)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %seteuid)
 * (%seteuid id)
 * 
 * Set the effective user id of the current process.
 * See the manual page "setuid".
 */
SCM_PROC (s_sys_seteuid, "%seteuid", 1, 0, 0, scm_sys_seteuid);
SCM 
scm_sys_seteuid (SCM id)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (id), id, scm_arg1, s_sys_seteuid);
  SCM_DEFER_INTS;
  answer = seteuid (SCM_INUM (id)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %setegid)
 * (%setegid id)
 * 
 * Set the effective group id of the current process.
 * See the manual page "setegid".
 */
SCM_PROC (s_sys_setegid, "%setegid", 1, 0, 0, scm_sys_setegid);
SCM 
scm_sys_setegid (SCM id)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (id), id, scm_arg1, s_sys_setegid);
  SCM_DEFER_INTS;
  answer = setegid (SCM_INUM (id)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getpgrp)
 * (getpgrp)
 * 
 * Return the process group of the current process.
 * See the manual page "getpgrp".
 */
SCM_PROC (s_getpgrp, "getpgrp", 0, 0, 0, scm_getpgrp);
SCM 
scm_getpgrp (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_DEFER_INTS;
  answer = SCM_MAKINUM (getpgrp ());
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %setpgid)
 * (%setpgid process-id process-group-id)
 * 
 * Set the process group of the indicated process.
 * See the manual page "setpgid".
 */
SCM_PROC (s_setpgid, "%setpgid", 2, 0, 0, scm_sys_setpgid);
SCM 
scm_sys_setpgid (SCM pid, SCM pgid)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (pid), pid, scm_arg1, s_setpgid);
  SCM_ASSERT (SCM_INUMP (pgid), pgid, scm_arg2, s_setpgid);
  SCM_DEFER_INTS;
  answer = (setpgid (SCM_INUM (pid), SCM_INUM (pgid))
	    ? scm_makerrno (errno)
	    : SCM_BOOL_T);
  SCM_ALLOW_INTS;
  return answer;
}

#if 0
/* Not supported by BSDI */
/*s
 * %getpgid process-id
 * 
 * Return the process group id of the indicated process.
 * See the manual page "getpgid".
 */
SCM_PROC (s_sys_getpgid, "%getpgid", 1, 0, 0, scm_sys_getpgid);
SCM
scm_sys_getpgid (SCM pid)
{
  SCM_INTS_ENABLED;
  int ret;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (pid), pid, scm_arg1, s_sys_getpgid);
  SCM_DEFER_INTS;
  ret = getpgid (SCM_INUM (pid));
  answer = ((ret < 0) ? scm_makerrno (errno) : SCM_MAKINUM (ret));
  SCM_ALLOW_INTS;
  return answer;
}
#endif

/*(c %setsid)
 * (%setsid)
 * 
 * See the manual page "setsid".
 */
SCM_PROC (s_setsid, "%setsid", 0, 0, 0, scm_sys_setsid);
SCM 
scm_sys_setsid (void)
{
  SCM_INTS_ENABLED;
  pid_t sid;
  SCM answer;

  SCM_DEFER_INTS;
  sid = setsid ();
  answer = (sid == -1) ? scm_makerrno (errno) : SCM_MAKINUM (sid);
  SCM_ALLOW_INTS;
  return answer;
}



/*(c %exec)
 * (%exec filename args :optional environment)
 * 
 * Exec the indicated program, providing `args' as
 * command line arguments.  If `environment' is specified,
 * make that the environment of program.
 *
 * `args' must be a list of read-only strings -- other elements of 
 * the list are ignored.
 *
 * See the manual page "exec".
 */
SCM_PROC (s_sys_exec, "%exec", 2, 1, 0, scm_sys_exec);
SCM
scm_sys_exec (SCM filename, SCM args, SCM env)
{
  SCM_INTS_ENABLED;
  t_uchar **execargv;
  t_uchar **environment;

  SCM answer;

  SCM_ASSERT (scm_is_ro_string (filename), filename, scm_arg1, s_sys_exec);
  if (scm_is_ro_substr (filename))
    filename = scm_makfromstr (SCM_RO_CHARS (filename), SCM_RO_LENGTH (filename));

  SCM_DEFER_INTS;
  execargv = scm_scm2argv (0, args, scm_arg2, s_sys_exec);
  if (!execargv)
    answer = SCM_BOOL_F;	/* probably won't be used --
				 * there will be a deferred exception when we 
				 * reach SCM_ALLOW_INTS
				 */
  else
    {
      if (SCM_UNBNDP (env) || (env == SCM_BOOL_T))
	environment = (t_uchar **)environ;
      else
	{
	  environment = scm_scm2argv (0, env, scm_arg3, s_sys_exec);
	  if (!environment)
	    {
	      int x;
	      for (x = 0; execargv[x]; ++x)
		free (execargv[x]);
	      free (execargv);
	      answer = SCM_BOOL_F; /* also expecting a deferred exception */
	      goto done;
	    }
	}
      execve (SCM_RO_CHARS (filename), (char **)execargv, (char **)environment);
      answer = scm_makerrno (errno);
    }
 done:
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %fork)
 * (%fork)
 * 
 * Create a new (child) process.  In the parent process,
 * return the process id of the child.  In the child process,
 * return 0.
 * 
 * See the manual page "fork".
 */
SCM_PROC (s_sys_fork, "%fork", 0, 0, 0, scm_sys_fork);
SCM
scm_sys_fork(void)
{
  SCM_INTS_ENABLED;
  pid_t pid;
  SCM answer;

  SCM_DEFER_INTS;
  pid = fork ();
  if (pid == -1)
    answer = scm_makerrno (errno);
  else
    answer = SCM_MAKINUM (0L+pid);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %exit)
 * (%exit status)
 * 
 * Exit the current process with the indicated status.
 * See the manual page "exit".
 */
SCM_PROC (s_sys_exit, "%exit", 1, 0, 0, scm_sys_exit);
SCM
scm_sys_exit (SCM x)
{
  SCM_INTS_ENABLED;
  int ix;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (x), x, scm_arg1, s_sys_exit);
  ix = SCM_INUM (x);
  SCM_DEFER_INTS;
  exit (ix);
  answer = scm_makerrno (errno);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %uname)
 * (%uname)
 * 
 * Return a vector of information about the host system.
 * See the manual page "uname".
 * 
 * This function returns a keyword-argument list.
 */
SCM_PROC (s_sys_uname, "%uname", 0, 0, 0, scm_sys_uname);
SCM 
scm_sys_uname (void)
{
  SCM_INTS_ENABLED;
  struct utsname buf;
  SCM answer;

  SCM_DEFER_INTS;
  if (uname (&buf))
    answer = scm_makerrno (errno);
  else
    {
      answer = scm_listify (kw_sysname, scm_makfromstr0 (buf.sysname),
			    kw_nodename, scm_makfromstr0 (buf.nodename),
			    kw_release, scm_makfromstr0 (buf.release),
			    kw_version, scm_makfromstr0 (buf.version),
			    kw_machine, scm_makfromstr0 (buf.machine),
			    SCM_UNDEFINED);
    }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c environ)
 * (environ (:optional new-environment))
 * 
 * Return the process environment variables as a list
 * of strings of the form:
 *
 *	("VAR=value" ...)
 *
 * If `new-environment' is provided, it should be a list
 * of the same form.  The process environment variables
 * are set to that list.
 *
 * See the manual page "environ".
 */
SCM_PROC (s_environ, "environ", 0, 1, 0, scm_environ);
SCM
scm_environ (SCM env)
{
  SCM_INTS_ENABLED;

  if (SCM_UNBNDP (env))
    {
      SCM answer;
      SCM_DEFER_INTS;
      answer = scm_argv2scm (-1, (t_uchar **)environ);
      SCM_ALLOW_INTS;
      return answer;
    }
  else
    {
      t_uchar ** new_environ;

      SCM_DEFER_INTS;
      new_environ = scm_scm2argv (0, env, scm_arg1, s_environ);

      {
	char **ep;
	static int first = 1;
	if (!first)
	  {
	    /* Free the old environment, except when called for the first
	     * time.
	     */
	    for (ep = environ; *ep != NULL; ep++)
	      free (*ep);
	    free ((char *) environ);
	  }
	first = 0;
      }
      environ = (char **)new_environ;
      SCM_ALLOW_INTS;
      return SCM_UNSPECIFIED;
    }
}


/*(c %access?)
 * (%access? path mode)
 * 
 * Return #t if the processes real user and group id have the
 * authority to access the file  `path' in the manner specified 
 * by `mode'.
 *
 * See the manual page "access".
 */
SCM_PROC (s_sys_access_p, "%access?", 2, 0, 0, scm_sys_access_p);
SCM 
scm_sys_access_p (SCM path, SCM show)
{
  SCM_INTS_ENABLED;
  int how;
  int rv;
  int errn;
  SCM answer;

  SCM_ASSERT (scm_is_ro_string (path), path, scm_arg1, s_sys_access_p);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));
  how = scm_integer_logior_cpp_constants (s_access_mode, show, scm_arg2, s_sys_access_p);
  SCM_DEFER_INTS;
  rv = vu_access (&errn, SCM_RO_CHARS (path), how);
  if (rv == -1)
    answer = scm_makerrno (errn);
  else
    answer = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c getenv)
 * (getenv variable-name)
 * 
 * Return the value of the indicated environment variable.
 * Return #f if the variable has no value.
 *
 * See the manual page "getenv".
 */
SCM_PROC (s_getenv, "getenv", 1, 0, 0, scm_getenv);
SCM
scm_getenv(SCM nam)
{
  SCM_INTS_ENABLED;
  char *val;
  SCM answer;

  SCM_ASSERT(scm_is_ro_string(nam), nam, scm_arg1, s_getenv);

  if (scm_is_ro_substr (nam))
    nam = scm_makfromstr (SCM_RO_CHARS (nam), SCM_RO_LENGTH (nam));

  SCM_DEFER_INTS;
  val = getenv(SCM_RO_CHARS(nam));
  if (!val)
    answer = SCM_BOOL_F;
  else
    answer = scm_makfromstr(val, (size_t)strlen(val));
  SCM_ALLOW_INTS;

  return answer;
}


/*(c %setenv)
 * (%setenv variable-name value overwrite?)
 * 
 * Add the indicated variable to the environment with the given 
 * value. If overwrite is not provided or is #f, and the variable 
 * is already defined, this procedure has no effect.
 * 
 * See the manual page "setenv".
 */
SCM_PROC (s_sys_setenv, "%setenv", 3, 0, 0, scm_sys_setenv);
SCM
scm_sys_setenv (SCM skey, SCM sval, SCM sover)
{
  SCM_INTS_ENABLED;
  int over;
  int answer;
  SCM sanswer;

  SCM_ASSERT(scm_is_ro_string(skey), skey, scm_arg1, s_sys_setenv);

  if (scm_is_ro_substr (skey))
    skey = scm_makfromstr (SCM_RO_CHARS (skey), SCM_RO_LENGTH (skey));

  SCM_ASSERT(scm_is_ro_string(sval), sval, scm_arg2, s_sys_setenv);

  if (scm_is_ro_substr (sval))
    sval = scm_makfromstr (SCM_RO_CHARS (sval), SCM_RO_LENGTH (sval));

  over = !SCM_UNBNDP (sover) && (SCM_BOOL_F != sover);

  SCM_DEFER_INTS;
  answer = setenv (SCM_RO_CHARS (skey), SCM_RO_CHARS (sval), over);
  sanswer = (answer < 0
	     ? scm_makerrno (ENOMEM)
	     : SCM_BOOL_T);
  SCM_ALLOW_INTS;
  return sanswer;
}


/*(c unsetenv)
 * (unsetenv variable-name)
 * 
 * Remove the indicated environment variable from the process'
 * environment variable settings.
 *
 * See the manual page "unsetenv".
 */
SCM_PROC (s_unsetenv, "unsetenv", 1, 0, 0, scm_unsetenv);
SCM
scm_unsetenv (SCM skey)
{
  SCM_INTS_ENABLED;
  SCM_ASSERT(scm_is_ro_string(skey), skey, scm_arg1, s_unsetenv);
  if (scm_is_ro_substr (skey))
    skey = scm_makfromstr (SCM_RO_CHARS (skey), SCM_RO_LENGTH (skey));

  SCM_DEFER_INTS;
  unsetenv (SCM_RO_CHARS (skey));
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


/*(c %putenv)
 * (%putenv string)
 * 
 * Modify the processes environment variables according
 * to `string'.
 *
 * See the manual page "putenv".
 */
SCM_PROC (s_sys_putenv, "%putenv", 1, 0, 0, scm_sys_putenv);
SCM
scm_sys_putenv (SCM str)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_sys_putenv);
  if (scm_is_ro_substr (str))
    str = scm_makfromstr (SCM_RO_CHARS (str), SCM_RO_LENGTH (str));
  SCM_DEFER_INTS;
  answer = putenv (SCM_RO_CHARS (str)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c ctime)
 * (ctime time)
 * 
 * Convert a time, represented in seconds since an epoch, to
 * a string such as:
 *
 *	"Thu Nov 24 18:22:48 1986\n"
 *
 * Fields in the string returned have constant length.
 *
 * See also `%time'.
 */
SCM_PROC (s_ctime, "ctime", 1, 0, 0, scm_ctime);
SCM
scm_ctime (SCM stime)
{
  long time;
  char * canswer;
  SCM answer;

  time = scm_num2long (stime, scm_arg1, s_ctime);
  SCM_DEFER_INTS;
  canswer = ctime (&time);
  answer = scm_makfromstr0 (canswer);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %nice)
 * (%nice increment)
 * 
 * Change the niceness of the current process by `increment'.
 * `increment' must be an integer.
 *
 * See the manual page "nice".
 */
SCM_PROC (s_sys_nice, "%nice", 1, 0, 0, scm_sys_nice);
SCM
scm_sys_nice(SCM incr)
{
  SCM_INTS_ENABLED;
  SCM answer;

  SCM_ASSERT(SCM_INUMP(incr), incr, scm_arg1, s_sys_nice);
  SCM_DEFER_INTS;
  answer = nice(SCM_INUM(incr)) ? scm_makerrno (errno) : SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c %sync)
 * (%sync)
 * 
 * Force pending file-system changes to disk.
 * 
 * See the manual page "fsync".
 */
SCM_PROC (s_sys_sync, "%sync", 0, 0, 0, scm_sys_sync);
SCM
scm_sys_sync(void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  sync();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


/*(c %truncate)
 * (%truncate path SCM_LENGTH)
 * 
 * Change the length of a file.
 * `path' must be a read-only string.
 * `length' must be an integer.
 *
 * See the manual page "truncate".
 */
SCM_PROC (s_sys_truncate, "%truncate", 2, 0, 0, scm_sys_truncate);
SCM
scm_sys_truncate (SCM path, SCM len)
{
  SCM_INTS_ENABLED;
  size_t s;
  int res;
  int errn;
  SCM v;

  SCM_ASSERT(scm_is_ro_string(path), path, scm_arg1, s_sys_truncate);
  if (scm_is_ro_substr (path))
    path = scm_makfromstr (SCM_RO_CHARS (path), SCM_RO_LENGTH (path));
  s = scm_num2ulong (len, scm_arg2, s_sys_truncate);
  SCM_DEFER_INTS;
  res = vu_truncate (&errn, SCM_RO_CHARS (path), s);
  if (res < 0)
    v = scm_makerrno (errn);
  else
    v = SCM_BOOL_T;
  SCM_ALLOW_INTS;
  return v;
}


/*(c alarm)
 * (alarm n)
 * 
 * Arrange for a SIGALRM interrupt `n' seconds into the future.
 *
 * See the manual page "alarm".
 */
SCM_PROC(s_alarm, "alarm", 1, 0, 0, scm_alarm);
SCM 
scm_alarm (SCM i)
{
  SCM_INTS_ENABLED;
  unsigned int j;

  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, scm_arg1, s_alarm);
  SCM_DEFER_INTS;
  SCM_SYSCALL (j = alarm (SCM_INUM (i)));
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (j);
}


/*(c pause)
 * (pause)
 * 
 * Make the current process sleep until a signal is received.
 *
 * See the manual page "pause".
 */
SCM_PROC(s_pause, "pause", 0, 0, 0, scm_pause);
SCM 
scm_pause (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  pause ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


/*(c sleep)
 * (sleep n)
 * 
 * Make the current process sleep until `n' seconds have
 * passed or a signal is received.
 * 
 * See the manual page "sleep".
 */
SCM_PROC(s_sleep, "sleep", 1, 0, 0, scm_sleep);
SCM 
scm_sleep (SCM i)
{
  SCM_INTS_ENABLED;
  unsigned int j;

  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, scm_arg1, s_sleep);
  SCM_DEFER_INTS;
  SCM_SYSCALL(j = sleep(SCM_INUM(i)););
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (j);
}


/****************************************************************
 * CPP constants
 */ 

void
scm_declare_integer_cpp_constant (SCM type, t_uchar * name, SCM value)
{
  scm_hashq_set_x (scm_cpp_constants, SCM_CAR (scm_intern_symhash ((name), SCM_UNDEFINED)), scm_cons (type, value));
}

int
scm_integer_cpp_constant (SCM type, SCM argument, SCM error_msg, SCM subr_name)
{
  SCM value;

  if (SCM_INUMP (argument))
    return SCM_INUM (argument);

  value = scm_hashq_ref (scm_cpp_constants, argument, SCM_BOOL_F);
  SCM_ASSERT (   !SCM_IS_IMMEDIATE (value)
	      && SCM_CONSP (value)
	      && (type == SCM_CAR (value))
	      && (SCM_INUMP (SCM_CDR (value))),
	      argument, error_msg, subr_name);
  return SCM_INUM (SCM_CDR (value));
}


int
scm_integer_logior_cpp_constants (SCM type, SCM argument, SCM error_msg, SCM subr_name)
{
  if ((SCM_UNDEFINED == argument) || (SCM_BOOL_F == argument))
    return 0;

  if (SCM_INUMP (argument))
    return SCM_INUM (argument);

  if (SCM_EOL == argument)
    return 0;

  if (SCM_IS_IMMEDIATE (argument) || !SCM_CONSP (argument))
    return scm_integer_cpp_constant (type, argument, error_msg, subr_name);
  else
    {
      int answer;

      answer = 0;
      while (!SCM_IS_IMMEDIATE (argument) && SCM_CONSP (argument))
	{
	  answer |= scm_integer_cpp_constant (type, SCM_CAR (argument), error_msg, subr_name);
	  argument = SCM_CDR (argument);
	}
      return answer;
    }
}


static void
init_cpp_constants ()
{
  /****************************************************************
   * access modes
   */
  scm_declare_integer_cpp_constant (s_access_mode, "R_OK", SCM_MAKINUM (R_OK));
  scm_declare_integer_cpp_constant (s_access_mode, "W_OK", SCM_MAKINUM (W_OK));
  scm_declare_integer_cpp_constant (s_access_mode, "X_OK", SCM_MAKINUM (X_OK));
  scm_declare_integer_cpp_constant (s_access_mode, "F_OK", SCM_MAKINUM (F_OK));


  /****************************************************************
   * waitpid options
   */
  scm_declare_integer_cpp_constant (s_waitpid_option, "WNOHANG", SCM_MAKINUM (WNOHANG));
  scm_declare_integer_cpp_constant (s_waitpid_option, "WUNTRACED", SCM_MAKINUM (WUNTRACED));


  /****************************************************************
   * signal names
   */
#ifdef SIGHUP
  scm_declare_integer_cpp_constant (s_signal, "SIGHUP", SCM_MAKINUM (SIGHUP));
#endif
#ifdef SIGCHLD
  scm_declare_integer_cpp_constant (s_signal, "SIGCHLD", SCM_MAKINUM (SIGCHLD));
#endif
#ifdef SIGINT
  scm_declare_integer_cpp_constant (s_signal, "SIGINT", SCM_MAKINUM (SIGINT));
#endif
#ifdef SIGQUIT
  scm_declare_integer_cpp_constant (s_signal, "SIGQUIT", SCM_MAKINUM (SIGQUIT));
#endif
#ifdef SIGILL
  scm_declare_integer_cpp_constant (s_signal, "SIGILL", SCM_MAKINUM (SIGILL));
#endif
#ifdef SIGTRAP
  scm_declare_integer_cpp_constant (s_signal, "SIGTRAP", SCM_MAKINUM (SIGTRAP));
#endif
#ifdef SIGABRT
  scm_declare_integer_cpp_constant (s_signal, "SIGABRT", SCM_MAKINUM (SIGABRT));
#endif
#ifdef SIGIOT
  scm_declare_integer_cpp_constant (s_signal, "SIGIOT", SCM_MAKINUM (SIGIOT));
#endif
#ifdef SIGBUS
  scm_declare_integer_cpp_constant (s_signal, "SIGBUS", SCM_MAKINUM (SIGBUS));
#endif
#ifdef SIGFPE
  scm_declare_integer_cpp_constant (s_signal, "SIGFPE", SCM_MAKINUM (SIGFPE));
#endif
#ifdef SIGKILL
  scm_declare_integer_cpp_constant (s_signal, "SIGKILL", SCM_MAKINUM (SIGKILL));
#endif
#ifdef SIGUSR1
  scm_declare_integer_cpp_constant (s_signal, "SIGUSR1", SCM_MAKINUM (SIGUSR1));
#endif
#ifdef SIGSEGV
  scm_declare_integer_cpp_constant (s_signal, "SIGSEGV", SCM_MAKINUM (SIGSEGV));
#endif
#ifdef SIGUSR2
  scm_declare_integer_cpp_constant (s_signal, "SIGUSR2", SCM_MAKINUM (SIGUSR2));
#endif
#ifdef SIGPIPE
  scm_declare_integer_cpp_constant (s_signal, "SIGPIPE", SCM_MAKINUM (SIGPIPE));
#endif
#ifdef SIGALRM
  scm_declare_integer_cpp_constant (s_signal, "SIGALRM", SCM_MAKINUM (SIGALRM));
#endif
#ifdef SIGTERM
  scm_declare_integer_cpp_constant (s_signal, "SIGTERM", SCM_MAKINUM (SIGTERM));
#endif
#ifdef SIGSTKFLT
  scm_declare_integer_cpp_constant (s_signal, "SIGSTKFLT", SCM_MAKINUM (SIGSTKFLT));
#endif
#ifdef SIGCONT
  scm_declare_integer_cpp_constant (s_signal, "SIGCONT", SCM_MAKINUM (SIGCONT));
#endif
#ifdef SIGSTOP
  scm_declare_integer_cpp_constant (s_signal, "SIGSTOP", SCM_MAKINUM (SIGSTOP));
#endif
#ifdef SIGTSTP
  scm_declare_integer_cpp_constant (s_signal, "SIGTSTP", SCM_MAKINUM (SIGTSTP));
#endif
#ifdef SIGTTIN
  scm_declare_integer_cpp_constant (s_signal, "SIGTTIN", SCM_MAKINUM (SIGTTIN));
#endif
#ifdef SIGTTOU
  scm_declare_integer_cpp_constant (s_signal, "SIGTTOU", SCM_MAKINUM (SIGTTOU));
#endif
#ifdef SIGIO
  scm_declare_integer_cpp_constant (s_signal, "SIGIO", SCM_MAKINUM (SIGIO));
#endif
#ifdef SIGPOLL
  scm_declare_integer_cpp_constant (s_signal, "SIGPOLL", SCM_MAKINUM (SIGPOLL));
#endif
#ifdef SIGURG
  scm_declare_integer_cpp_constant (s_signal, "SIGURG", SCM_MAKINUM (SIGURG));
#endif
#ifdef SIGXCPU
  scm_declare_integer_cpp_constant (s_signal, "SIGXCPU", SCM_MAKINUM (SIGXCPU));
#endif
#ifdef SIGXFSZ
  scm_declare_integer_cpp_constant (s_signal, "SIGXFSZ", SCM_MAKINUM (SIGXFSZ));
#endif
#ifdef SIGVTALRM
  scm_declare_integer_cpp_constant (s_signal, "SIGVTALRM", SCM_MAKINUM (SIGVTALRM));
#endif
#ifdef SIGPROF
  scm_declare_integer_cpp_constant (s_signal, "SIGPROF", SCM_MAKINUM (SIGPROF));
#endif
#ifdef SIGWINCH
  scm_declare_integer_cpp_constant (s_signal, "SIGWINCH", SCM_MAKINUM (SIGWINCH));
#endif
#ifdef SIGLOST
  scm_declare_integer_cpp_constant (s_signal, "SIGLOST", SCM_MAKINUM (SIGLOST));
#endif
#ifdef SIGPWR
  scm_declare_integer_cpp_constant (s_signal, "SIGPWR", SCM_MAKINUM (SIGPWR));
#endif


  /****************************************************************
   * errno values
   */
#ifdef EPERM
  scm_declare_integer_cpp_constant (s_errno_value, "EPERM", SCM_MAKINUM (EPERM));
#endif
#ifdef ENOENT
  scm_declare_integer_cpp_constant (s_errno_value, "ENOENT", SCM_MAKINUM (ENOENT));
#endif
#ifdef ESRCH
  scm_declare_integer_cpp_constant (s_errno_value, "ESRCH", SCM_MAKINUM (ESRCH));
#endif
#ifdef EINTR
  scm_declare_integer_cpp_constant (s_errno_value, "EINTR", SCM_MAKINUM (EINTR));
#endif
#ifdef EIO
  scm_declare_integer_cpp_constant (s_errno_value, "EIO", SCM_MAKINUM (EIO));
#endif
#ifdef ENXIO
  scm_declare_integer_cpp_constant (s_errno_value, "ENXIO", SCM_MAKINUM (ENXIO));
#endif
#ifdef E2BIG
  scm_declare_integer_cpp_constant (s_errno_value, "E2BIG", SCM_MAKINUM (E2BIG));
#endif
#ifdef ENOEXEC
  scm_declare_integer_cpp_constant (s_errno_value, "ENOEXEC", SCM_MAKINUM (ENOEXEC));
#endif
#ifdef EBADF
  scm_declare_integer_cpp_constant (s_errno_value, "EBADF", SCM_MAKINUM (EBADF));
#endif
#ifdef ECHILD
  scm_declare_integer_cpp_constant (s_errno_value, "ECHILD", SCM_MAKINUM (ECHILD));
#endif
#ifdef EAGAIN
  scm_declare_integer_cpp_constant (s_errno_value, "EAGAIN", SCM_MAKINUM (EAGAIN));
#endif
#ifdef ENOMEM
  scm_declare_integer_cpp_constant (s_errno_value, "ENOMEM", SCM_MAKINUM (ENOMEM));
#endif
#ifdef EACCES
  scm_declare_integer_cpp_constant (s_errno_value, "EACCES", SCM_MAKINUM (EACCES));
#endif
#ifdef EFAULT
  scm_declare_integer_cpp_constant (s_errno_value, "EFAULT", SCM_MAKINUM (EFAULT));
#endif
#ifdef ENOTBLK
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTBLK", SCM_MAKINUM (ENOTBLK));
#endif
#ifdef EBUSY
  scm_declare_integer_cpp_constant (s_errno_value, "EBUSY", SCM_MAKINUM (EBUSY));
#endif
#ifdef EEXIST
  scm_declare_integer_cpp_constant (s_errno_value, "EEXIST", SCM_MAKINUM (EEXIST));
#endif
#ifdef EXDEV
  scm_declare_integer_cpp_constant (s_errno_value, "EXDEV", SCM_MAKINUM (EXDEV));
#endif
#ifdef ENODEV
  scm_declare_integer_cpp_constant (s_errno_value, "ENODEV", SCM_MAKINUM (ENODEV));
#endif
#ifdef ENOTDIR
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTDIR", SCM_MAKINUM (ENOTDIR));
#endif
#ifdef EISDIR
  scm_declare_integer_cpp_constant (s_errno_value, "EISDIR", SCM_MAKINUM (EISDIR));
#endif
#ifdef EINVAL
  scm_declare_integer_cpp_constant (s_errno_value, "EINVAL", SCM_MAKINUM (EINVAL));
#endif
#ifdef ENFILE
  scm_declare_integer_cpp_constant (s_errno_value, "ENFILE", SCM_MAKINUM (ENFILE));
#endif
#ifdef EMFILE
  scm_declare_integer_cpp_constant (s_errno_value, "EMFILE", SCM_MAKINUM (EMFILE));
#endif
#ifdef ENOTTY
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTTY", SCM_MAKINUM (ENOTTY));
#endif
#ifdef ETXTBSY
  scm_declare_integer_cpp_constant (s_errno_value, "ETXTBSY", SCM_MAKINUM (ETXTBSY));
#endif
#ifdef EFBIG
  scm_declare_integer_cpp_constant (s_errno_value, "EFBIG", SCM_MAKINUM (EFBIG));
#endif
#ifdef ENOSPC
  scm_declare_integer_cpp_constant (s_errno_value, "ENOSPC", SCM_MAKINUM (ENOSPC));
#endif
#ifdef ESPIPE
  scm_declare_integer_cpp_constant (s_errno_value, "ESPIPE", SCM_MAKINUM (ESPIPE));
#endif
#ifdef EROFS
  scm_declare_integer_cpp_constant (s_errno_value, "EROFS", SCM_MAKINUM (EROFS));
#endif
#ifdef EMLINK
  scm_declare_integer_cpp_constant (s_errno_value, "EMLINK", SCM_MAKINUM (EMLINK));
#endif
#ifdef EPIPE
  scm_declare_integer_cpp_constant (s_errno_value, "EPIPE", SCM_MAKINUM (EPIPE));
#endif
#ifdef EDOM
  scm_declare_integer_cpp_constant (s_errno_value, "EDOM", SCM_MAKINUM (EDOM));
#endif
#ifdef ERANGE
  scm_declare_integer_cpp_constant (s_errno_value, "ERANGE", SCM_MAKINUM (ERANGE));
#endif
#ifdef EDEADLK
  scm_declare_integer_cpp_constant (s_errno_value, "EDEADLK", SCM_MAKINUM (EDEADLK));
#endif
#ifdef ENAMETOOLONG
  scm_declare_integer_cpp_constant (s_errno_value, "ENAMETOOLONG", SCM_MAKINUM (ENAMETOOLONG));
#endif
#ifdef ENOLCK
  scm_declare_integer_cpp_constant (s_errno_value, "ENOLCK", SCM_MAKINUM (ENOLCK));
#endif
#ifdef ENOSYS
  scm_declare_integer_cpp_constant (s_errno_value, "ENOSYS", SCM_MAKINUM (ENOSYS));
#endif
#ifdef ENOTEMPTY
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTEMPTY", SCM_MAKINUM (ENOTEMPTY));
#endif
#ifdef ELOOP
  scm_declare_integer_cpp_constant (s_errno_value, "ELOOP", SCM_MAKINUM (ELOOP));
#endif
#ifdef ENOMSG
  scm_declare_integer_cpp_constant (s_errno_value, "ENOMSG", SCM_MAKINUM (ENOMSG));
#endif
#ifdef EIDRM
  scm_declare_integer_cpp_constant (s_errno_value, "EIDRM", SCM_MAKINUM (EIDRM));
#endif
#ifdef ECHRNG
  scm_declare_integer_cpp_constant (s_errno_value, "ECHRNG", SCM_MAKINUM (ECHRNG));
#endif
#ifdef EL2NSYNC
  scm_declare_integer_cpp_constant (s_errno_value, "EL2NSYNC", SCM_MAKINUM (EL2NSYNC));
#endif
#ifdef EL3HLT
  scm_declare_integer_cpp_constant (s_errno_value, "EL3HLT", SCM_MAKINUM (EL3HLT));
#endif
#ifdef EL3RST
  scm_declare_integer_cpp_constant (s_errno_value, "EL3RST", SCM_MAKINUM (EL3RST));
#endif
#ifdef ELNRNG
  scm_declare_integer_cpp_constant (s_errno_value, "ELNRNG", SCM_MAKINUM (ELNRNG));
#endif
#ifdef EUNATCH
  scm_declare_integer_cpp_constant (s_errno_value, "EUNATCH", SCM_MAKINUM (EUNATCH));
#endif
#ifdef ENOCSI
  scm_declare_integer_cpp_constant (s_errno_value, "ENOCSI", SCM_MAKINUM (ENOCSI));
#endif
#ifdef EL2HLT
  scm_declare_integer_cpp_constant (s_errno_value, "EL2HLT", SCM_MAKINUM (EL2HLT));
#endif
#ifdef EBADE
  scm_declare_integer_cpp_constant (s_errno_value, "EBADE", SCM_MAKINUM (EBADE));
#endif
#ifdef EBADR
  scm_declare_integer_cpp_constant (s_errno_value, "EBADR", SCM_MAKINUM (EBADR));
#endif
#ifdef EXFULL
  scm_declare_integer_cpp_constant (s_errno_value, "EXFULL", SCM_MAKINUM (EXFULL));
#endif
#ifdef ENOANO
  scm_declare_integer_cpp_constant (s_errno_value, "ENOANO", SCM_MAKINUM (ENOANO));
#endif
#ifdef EBADRQC
  scm_declare_integer_cpp_constant (s_errno_value, "EBADRQC", SCM_MAKINUM (EBADRQC));
#endif
#ifdef EBADSLT
  scm_declare_integer_cpp_constant (s_errno_value, "EBADSLT", SCM_MAKINUM (EBADSLT));
#endif
#ifdef EDEADLOCK
  scm_declare_integer_cpp_constant (s_errno_value, "EDEADLOCK", SCM_MAKINUM (EDEADLOCK));
#endif
#ifdef EBFONT
  scm_declare_integer_cpp_constant (s_errno_value, "EBFONT", SCM_MAKINUM (EBFONT));
#endif
#ifdef ENOSTR
  scm_declare_integer_cpp_constant (s_errno_value, "ENOSTR", SCM_MAKINUM (ENOSTR));
#endif
#ifdef ENODATA
  scm_declare_integer_cpp_constant (s_errno_value, "ENODATA", SCM_MAKINUM (ENODATA));
#endif
#ifdef ETIME
  scm_declare_integer_cpp_constant (s_errno_value, "ETIME", SCM_MAKINUM (ETIME));
#endif
#ifdef ENOSR
  scm_declare_integer_cpp_constant (s_errno_value, "ENOSR", SCM_MAKINUM (ENOSR));
#endif
#ifdef ENONET
  scm_declare_integer_cpp_constant (s_errno_value, "ENONET", SCM_MAKINUM (ENONET));
#endif
#ifdef ENOPKG
  scm_declare_integer_cpp_constant (s_errno_value, "ENOPKG", SCM_MAKINUM (ENOPKG));
#endif
#ifdef EREMOTE
  scm_declare_integer_cpp_constant (s_errno_value, "EREMOTE", SCM_MAKINUM (EREMOTE));
#endif
#ifdef ENOLINK
  scm_declare_integer_cpp_constant (s_errno_value, "ENOLINK", SCM_MAKINUM (ENOLINK));
#endif
#ifdef EADV
  scm_declare_integer_cpp_constant (s_errno_value, "EADV", SCM_MAKINUM (EADV));
#endif
#ifdef ESRMNT
  scm_declare_integer_cpp_constant (s_errno_value, "ESRMNT", SCM_MAKINUM (ESRMNT));
#endif
#ifdef ECOMM
  scm_declare_integer_cpp_constant (s_errno_value, "ECOMM", SCM_MAKINUM (ECOMM));
#endif
#ifdef EPROTO
  scm_declare_integer_cpp_constant (s_errno_value, "EPROTO", SCM_MAKINUM (EPROTO));
#endif
#ifdef EMULTIHOP
  scm_declare_integer_cpp_constant (s_errno_value, "EMULTIHOP", SCM_MAKINUM (EMULTIHOP));
#endif
#ifdef EDOTDOT
  scm_declare_integer_cpp_constant (s_errno_value, "EDOTDOT", SCM_MAKINUM (EDOTDOT));
#endif
#ifdef EBADMSG
  scm_declare_integer_cpp_constant (s_errno_value, "EBADMSG", SCM_MAKINUM (EBADMSG));
#endif
#ifdef EOVERFLOW
  scm_declare_integer_cpp_constant (s_errno_value, "EOVERFLOW", SCM_MAKINUM (EOVERFLOW));
#endif
#ifdef ENOTUNIQ
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTUNIQ", SCM_MAKINUM (ENOTUNIQ));
#endif
#ifdef EBADFD
  scm_declare_integer_cpp_constant (s_errno_value, "EBADFD", SCM_MAKINUM (EBADFD));
#endif
#ifdef EREMCHG
  scm_declare_integer_cpp_constant (s_errno_value, "EREMCHG", SCM_MAKINUM (EREMCHG));
#endif
#ifdef ELIBACC
  scm_declare_integer_cpp_constant (s_errno_value, "ELIBACC", SCM_MAKINUM (ELIBACC));
#endif
#ifdef ELIBBAD
  scm_declare_integer_cpp_constant (s_errno_value, "ELIBBAD", SCM_MAKINUM (ELIBBAD));
#endif
#ifdef ELIBSCN
  scm_declare_integer_cpp_constant (s_errno_value, "ELIBSCN", SCM_MAKINUM (ELIBSCN));
#endif
#ifdef ELIBMAX
  scm_declare_integer_cpp_constant (s_errno_value, "ELIBMAX", SCM_MAKINUM (ELIBMAX));
#endif
#ifdef ELIBEXEC
  scm_declare_integer_cpp_constant (s_errno_value, "ELIBEXEC", SCM_MAKINUM (ELIBEXEC));
#endif
#ifdef EILSEQ
  scm_declare_integer_cpp_constant (s_errno_value, "EILSEQ", SCM_MAKINUM (EILSEQ));
#endif
#ifdef ERESTART
  scm_declare_integer_cpp_constant (s_errno_value, "ERESTART", SCM_MAKINUM (ERESTART));
#endif
#ifdef ESTRPIPE
  scm_declare_integer_cpp_constant (s_errno_value, "ESTRPIPE", SCM_MAKINUM (ESTRPIPE));
#endif
#ifdef EUSERS
  scm_declare_integer_cpp_constant (s_errno_value, "EUSERS", SCM_MAKINUM (EUSERS));
#endif
#ifdef ENOTSOCK
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTSOCK", SCM_MAKINUM (ENOTSOCK));
#endif
#ifdef EDESTADDRREQ
  scm_declare_integer_cpp_constant (s_errno_value, "EDESTADDRREQ", SCM_MAKINUM (EDESTADDRREQ));
#endif
#ifdef EMSGSIZE
  scm_declare_integer_cpp_constant (s_errno_value, "EMSGSIZE", SCM_MAKINUM (EMSGSIZE));
#endif
#ifdef EPROTOTYPE
  scm_declare_integer_cpp_constant (s_errno_value, "EPROTOTYPE", SCM_MAKINUM (EPROTOTYPE));
#endif
#ifdef ENOPROTOOPT
  scm_declare_integer_cpp_constant (s_errno_value, "ENOPROTOOPT", SCM_MAKINUM (ENOPROTOOPT));
#endif
#ifdef EPROTONOSUPPORT
  scm_declare_integer_cpp_constant (s_errno_value, "EPROTONOSUPPORT", SCM_MAKINUM (EPROTONOSUPPORT));
#endif
#ifdef ESOCKTNOSUPPORT
  scm_declare_integer_cpp_constant (s_errno_value, "ESOCKTNOSUPPORT", SCM_MAKINUM (ESOCKTNOSUPPORT));
#endif
#ifdef EOPNOTSUPP
  scm_declare_integer_cpp_constant (s_errno_value, "EOPNOTSUPP", SCM_MAKINUM (EOPNOTSUPP));
#endif
#ifdef EPFNOSUPPORT
  scm_declare_integer_cpp_constant (s_errno_value, "EPFNOSUPPORT", SCM_MAKINUM (EPFNOSUPPORT));
#endif
#ifdef EAFNOSUPPORT
  scm_declare_integer_cpp_constant (s_errno_value, "EAFNOSUPPORT", SCM_MAKINUM (EAFNOSUPPORT));
#endif
#ifdef EADDRINUSE
  scm_declare_integer_cpp_constant (s_errno_value, "EADDRINUSE", SCM_MAKINUM (EADDRINUSE));
#endif
#ifdef EADDRNOTAVAIL
  scm_declare_integer_cpp_constant (s_errno_value, "EADDRNOTAVAIL", SCM_MAKINUM (EADDRNOTAVAIL));
#endif
#ifdef ENETDOWN
  scm_declare_integer_cpp_constant (s_errno_value, "ENETDOWN", SCM_MAKINUM (ENETDOWN));
#endif
#ifdef ENETUNREACH
  scm_declare_integer_cpp_constant (s_errno_value, "ENETUNREACH", SCM_MAKINUM (ENETUNREACH));
#endif
#ifdef ENETRESET
  scm_declare_integer_cpp_constant (s_errno_value, "ENETRESET", SCM_MAKINUM (ENETRESET));
#endif
#ifdef ECONNABORTED
  scm_declare_integer_cpp_constant (s_errno_value, "ECONNABORTED", SCM_MAKINUM (ECONNABORTED));
#endif
#ifdef ECONNRESET
  scm_declare_integer_cpp_constant (s_errno_value, "ECONNRESET", SCM_MAKINUM (ECONNRESET));
#endif
#ifdef ENOBUFS
  scm_declare_integer_cpp_constant (s_errno_value, "ENOBUFS", SCM_MAKINUM (ENOBUFS));
#endif
#ifdef EISCONN
  scm_declare_integer_cpp_constant (s_errno_value, "EISCONN", SCM_MAKINUM (EISCONN));
#endif
#ifdef ENOTCONN
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTCONN", SCM_MAKINUM (ENOTCONN));
#endif
#ifdef ESHUTDOWN
  scm_declare_integer_cpp_constant (s_errno_value, "ESHUTDOWN", SCM_MAKINUM (ESHUTDOWN));
#endif
#ifdef ETOOMANYREFS
  scm_declare_integer_cpp_constant (s_errno_value, "ETOOMANYREFS", SCM_MAKINUM (ETOOMANYREFS));
#endif
#ifdef ETIMEDOUT
  scm_declare_integer_cpp_constant (s_errno_value, "ETIMEDOUT", SCM_MAKINUM (ETIMEDOUT));
#endif
#ifdef ECONNREFUSED
  scm_declare_integer_cpp_constant (s_errno_value, "ECONNREFUSED", SCM_MAKINUM (ECONNREFUSED));
#endif
#ifdef EHOSTDOWN
  scm_declare_integer_cpp_constant (s_errno_value, "EHOSTDOWN", SCM_MAKINUM (EHOSTDOWN));
#endif
#ifdef EHOSTUNREACH
  scm_declare_integer_cpp_constant (s_errno_value, "EHOSTUNREACH", SCM_MAKINUM (EHOSTUNREACH));
#endif
#ifdef EALREADY
  scm_declare_integer_cpp_constant (s_errno_value, "EALREADY", SCM_MAKINUM (EALREADY));
#endif
#ifdef EINPROGRESS
  scm_declare_integer_cpp_constant (s_errno_value, "EINPROGRESS", SCM_MAKINUM (EINPROGRESS));
#endif
#ifdef ESTALE
  scm_declare_integer_cpp_constant (s_errno_value, "ESTALE", SCM_MAKINUM (ESTALE));
#endif
#ifdef EUCLEAN
  scm_declare_integer_cpp_constant (s_errno_value, "EUCLEAN", SCM_MAKINUM (EUCLEAN));
#endif
#ifdef ENOTNAM
  scm_declare_integer_cpp_constant (s_errno_value, "ENOTNAM", SCM_MAKINUM (ENOTNAM));
#endif
#ifdef ENAVAIL
  scm_declare_integer_cpp_constant (s_errno_value, "ENAVAIL", SCM_MAKINUM (ENAVAIL));
#endif
#ifdef EISNAM
  scm_declare_integer_cpp_constant (s_errno_value, "EISNAM", SCM_MAKINUM (EISNAM));
#endif
#ifdef EREMOTEIO
  scm_declare_integer_cpp_constant (s_errno_value, "EREMOTEIO", SCM_MAKINUM (EREMOTEIO));
#endif
#ifdef EDQUOT
  scm_declare_integer_cpp_constant (s_errno_value, "EDQUOT", SCM_MAKINUM (EDQUOT));
#endif
}



void 
scm_init_system (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/system.x"

  init_cpp_constants ();
}



/************************************************************************
 *(h1 "Rationale -- Unix System Call")
 *
 * Another implementation (SCSH) converts all system call errors to 
 * exceptions.  That seems wrong: an error from a system call is
 * not necessarily an exception  -- it may be a useful return value.
 * Therefore, we use the convention that system call errors return
 * a symbol, but make it trivial (via the procedure `%%') to convert
 * error returns to exceptions.
 *
 */

