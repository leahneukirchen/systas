/* fdsocket.c -socket functions operating on file descriptors
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998,1999 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include "hackerlab/mem/mem.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/filesys.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/socket.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/fdsocket.h"


/************************************************************************
 *(h0 "Networking Procedures")
 * 
 * 
 * These procedures need to be documented in a future releae:
 *
 * 
 *  %socket %socketpair %getsockopt %setsockopt %shutdown %connect %bind
 *  %listen %accept %getsockname %getpeername %recv %send %recvfrom
 *  %sendto
 * 
 *  inet-aton inet-ntoa inet-netof inet-lnaof inet-makeaddr gethostent
 *  gethostbyname gethostbyaddr getnetent getnetbyname getnetbyaddr
 *  getprotoent getprotobyname protobynumber getservent getservbyname
 *  getservbyport sethostent endhostent setnetent endnetent setservent
 *  endservent setprotoent endprotoent gethostname
 * 
 */




SCM_SYMBOL (s_unrecognized_protocol, "unrecognized protocol");
SCM_SYMBOL (s_path_too_long_for_socket_address, "path too long for socket address");
SCM_SYMBOL (s_unrecognized_send_recv_flag, "unrecognized send/recv flag");
SCM_SYMBOL (s_address_buffer, "address buffer");

SCM_SYMBOL (s_INADDR_ANY, "INADDR_ANY");
SCM_SYMBOL (s_INADDR_BROADCAST, "INADDR_BROADCAST");
#if 0
/* Not supported by BSDI */
SCM_SYMBOL (s_INADDR_LOOPBACK, "INADDR_LOOPBACK");
#endif

/* socket types.  */
#ifdef SOCK_STREAM
SCM_SYMBOL (scm_SOCK_STREAM, "SOCK_STREAM");
#endif
#ifdef SOCK_DGRAM
SCM_SYMBOL (scm_SOCK_DGRAM, "SOCK_DGRAM");
#endif

/* setsockopt level.  */
#ifdef SOL_SOCKET
SCM_SYMBOL (scm_SOL_SOCKET, "SOL_SOCKET");
#endif
#ifdef SOL_IP
SCM_SYMBOL (scm_SOL_IP, "SOL_IP");
#endif
#ifdef SOL_TCP
SCM_SYMBOL (scm_SOL_TCP, "SOL_TCP");
#endif
#ifdef SOL_UDP
SCM_SYMBOL (scm_SOL_UDP, "SOL_UDP");
#endif

/* setsockopt names.  */
#ifdef SO_DEBUG
SCM_SYMBOL (scm_SO_DEBUG, "SO_DEBUG");
#endif
#ifdef SO_REUSEADDR
SCM_SYMBOL (scm_SO_REUSEADDR, "SO_REUSEADDR");
#endif
#ifdef SO_STYLE
SCM_SYMBOL (scm_SO_STYLE, "SO_STYLE");
#endif
#ifdef SO_TYPE
SCM_SYMBOL (scm_SO_TYPE, "SO_TYPE");
#endif
#ifdef SO_ERROR
SCM_SYMBOL (scm_SO_ERROR, "SO_ERROR");
#endif
#ifdef SO_DONTROUTE
SCM_SYMBOL (scm_SO_DONTROUTE, "SO_DONTROUTE");
#endif
#ifdef SO_BROADCAST
SCM_SYMBOL (scm_SO_BROADCAST, "SO_BROADCAST");
#endif
#ifdef SO_SNDBUF
SCM_SYMBOL (scm_SO_SNDBUF, "SO_SNDBUF");
#endif
#ifdef SO_RCVBUF
SCM_SYMBOL (scm_SO_RCVBUF, "SO_RCVBUF");
#endif
#ifdef SO_KEEPALIVE
SCM_SYMBOL (scm_SO_KEEPALIVE, "SO_KEEPALIVE");
#endif
#ifdef SO_OOBINLINE
SCM_SYMBOL (scm_SO_OOBINLINE, "SO_OOBINLINE");
#endif
#ifdef SO_NO_CHECK
SCM_SYMBOL (scm_SO_NO_CHECK, "SO_NO_CHECK");
#endif
#ifdef SO_PRIORITY
SCM_SYMBOL (scm_SO_PRIORITY, "SO_PRIORITY");
#endif
#ifdef SO_LINGER
SCM_SYMBOL (scm_SO_LINGER, "SO_LINGER");
#endif

/* recv/send options.  */
SCM_SYMBOL (scm_MSG_OOB, "MSG_OOB");
SCM_SYMBOL (scm_MSG_PEEK, "MSG_PEEK");
SCM_SYMBOL (scm_MSG_DONTROUTE, "MSG_DONTROUTE");



int
scm_scm2socktype (SCM type, SCM why, SCM caller)
{
  SCM_INTS_ENABLED;
#ifdef SOCK_STREAM
  if (scm_SOCK_STREAM == type)
    return SOCK_STREAM;
#endif
#ifdef SOCK_DGRAM
  if (scm_SOCK_DGRAM == type)
    return SOCK_DGRAM;
#endif
  SCM_ASSERT (SCM_INUMP (type), type, why, caller);
  return SCM_INUM (type);
}


int
scm_scm2protocol (SCM proto, SCM why, SCM caller)
{
  SCM_INTS_ENABLED;
  struct protoent *entry;
  int ans;

  if (SCM_INUMP (proto))
    return SCM_INUM (proto);

  SCM_ASSERT (scm_is_ro_string (proto), proto, why, caller);
  SCM_DEFER_INTS;
  entry = getprotobyname (SCM_RO_CHARS (proto));
  if (!entry)
    ans = -1;
  else
    ans =  entry->p_proto;
  SCM_ALLOW_INTS;

  SCM_ASSERT (ans > 0, proto, s_unrecognized_protocol, caller);
  return ans;
}

SCM_PROC (s_sys_socket, "%socket", 3, 0, 0, scm_sys_socket);
SCM 
scm_sys_socket (SCM family, SCM style, SCM proto)
{
  SCM_INTS_ENABLED;
  int fd;
  int fam;
  int type;
  int protocol;
  SCM answer;

  fam = scm_scm2addrtype (family, scm_arg1, s_sys_socket);
  type = scm_scm2socktype (style, scm_arg2, s_sys_socket);
  protocol = scm_scm2protocol (proto, scm_arg3, s_sys_socket);

  SCM_DEFER_INTS;
  fd = socket (fam, type, protocol);
  if (fd == -1)
    answer = scm_makerrno (errno);
  else
    answer = scm_makefd (fd, scm_fd_is_open | scm_close_fd_on_gc);
  SCM_ALLOW_INTS;
  return answer;
}

SCM_PROC (s_sys_socketpair, "%socketpair", 3, 0, 0, scm_sys_socketpair);
SCM 
scm_sys_socketpair (SCM family, SCM style, SCM proto)
{
  SCM_INTS_ENABLED;
  int rv;
  int fam;
  int type;
  int protocol;
  int fd[2];
  SCM answer;

  fam = scm_scm2addrtype (family, scm_arg1, s_sys_socketpair);
  type = scm_scm2socktype (style, scm_arg2, s_sys_socketpair);
  protocol = scm_scm2protocol (proto, scm_arg3, s_sys_socketpair);

  SCM_DEFER_INTS;
  rv = socketpair (fam, type, protocol, fd);
  if (rv == -1)
    answer = scm_makerrno (errno);
  else
    {
      SCM a;
      SCM b;
      a = scm_makefd (fd[0], scm_fd_is_open | scm_close_fd_on_gc);
      b = scm_makefd (fd[1], scm_fd_is_open | scm_close_fd_on_gc);
      answer = scm_cons (a, b);
    }
  SCM_ALLOW_INTS;
  return answer;
}


int
scm_scm2sol (SCM sol, SCM why, SCM caller)
{
#ifdef SOL_SOCKET
  if (sol == scm_SOL_SOCKET)
    return SOL_SOCKET;
#endif
#ifdef SOL_IP
  if (sol == scm_SOL_IP)
    return SOL_IP;
#endif
#ifdef SOL_TCP
  if (sol == scm_SOL_TCP)
    return SOL_TCP;
#endif
#ifdef SOL_UDP
  if (sol == scm_SOL_UDP)
    return SOL_UDP;
#endif
  SCM_ASSERT (SCM_INUMP (sol), sol, why, caller);
  return SCM_INUM (sol);
}

int
scm_scm2so (SCM so, SCM why, SCM caller)
{
#ifdef SO_DEBUG
  if (so == scm_SO_DEBUG)
    return SO_DEBUG;
#endif
#ifdef SO_REUSEADDR
  if (so == scm_SO_REUSEADDR)
    return SO_REUSEADDR;
#endif
#ifdef SO_STYLE
  if (so == scm_SO_STYLE)
    return SO_STYLE;
#endif
#ifdef SO_TYPE
  if (so == scm_SO_TYPE)
    return SO_TYPE;
#endif
#ifdef SO_ERROR
  if (so == scm_SO_ERROR)
    return SO_ERROR;
#endif
#ifdef SO_DONTROUTE
  if (so == scm_SO_DONTROUTE)
    return SO_DONTROUTE;
#endif
#ifdef SO_BROADCAST
  if (so == scm_SO_BROADCAST)
    return SO_BROADCAST;
#endif
#ifdef SO_SNDBUF
  if (so == scm_SO_SNDBUF)
    return SO_SNDBUF;
#endif
#ifdef SO_RCVBUF
  if (so == scm_SO_RCVBUF)
    return SO_RCVBUF;
#endif
#ifdef SO_KEEPALIVE
  if (so == scm_SO_KEEPALIVE)
    return SO_KEEPALIVE;
#endif
#ifdef SO_OOBINLINE
  if (so == scm_SO_OOBINLINE)
    return SO_OOBINLINE;
#endif
#ifdef SO_NO_CHECK
  if (so == scm_SO_NO_CHECK)
    return SO_NO_CHECK;
#endif
#ifdef SO_PRIORITY
  if (so == scm_SO_PRIORITY)
    return SO_PRIORITY;
#endif
#ifdef SO_LINGER
  if (so == scm_SO_LINGER)
    return SO_LINGER;
#endif
  SCM_ASSERT (SCM_INUMP (so), so, why, caller);
  return SCM_INUM (so);
}


SCM_PROC (s_sys_getsockopt, "%getsockopt", 3, 0, 0, scm_sys_getsockopt);
SCM
scm_sys_getsockopt (SCM sfd, SCM level, SCM optname)
{
  SCM_INTS_ENABLED;
  int rv;
  int fd;
  int optlen;
  char optval[sizeof (struct linger)];
  int ilevel;
  int ioptname;
  SCM ans;

  optlen = sizeof (struct linger);
  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_getsockopt);

  ilevel = scm_scm2sol (level, scm_arg2, s_sys_getsockopt);
  ioptname = scm_scm2so (optname, scm_arg3, s_sys_getsockopt);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  rv = getsockopt (fd, ilevel, ioptname, (void *) optval, &optlen);

  if (rv == -1)
    ans = scm_makerrno (errno);

#ifdef SO_LINGER
  else if (ilevel == SOL_SOCKET && ioptname == SO_LINGER)
    {
      struct linger *ling;
      ling = (struct linger *) optval;
      ans = scm_cons (SCM_MAKINUM (ling->l_onoff),
		      SCM_MAKINUM (ling->l_linger));
    }
#endif
#ifdef SO_SNDBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_SNDBUF)
    {
      long * bufsize;
      bufsize = (long *) optval;
      ans = SCM_MAKINUM (*bufsize);
    }
#endif
#ifdef SO_RCVBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_RCVBUF)
    {
      long * bufsize;
      bufsize = (long *) optval;
      ans = SCM_MAKINUM (*bufsize);
    }
#endif
  else
    ans = SCM_MAKINUM (*(int *) optval);

  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_sys_setsockopt, "%setsockopt", 4, 0, 0, scm_sys_setsockopt);
SCM
scm_sys_setsockopt (SCM sfd, SCM level, SCM optname, SCM value)
{
  SCM_INTS_ENABLED;

  int rv;
  int fd;
  long optlen;
  char optval[sizeof (struct linger)]; /* Biggest option :-(  */
  int ilevel;
  int ioptname;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_setsockopt);

  ilevel = scm_scm2sol (level, scm_arg2, s_sys_setsockopt);
  ioptname = scm_scm2so (optname, scm_arg3, s_sys_setsockopt);

  if (0);
#ifdef SO_LINGER
  else if (ilevel == SOL_SOCKET && ioptname == SO_LINGER)
    {
      struct linger ling;
      if (SCM_INUMP (value))
	{
	  ling.l_onoff = 1;
	  ling.l_linger = SCM_INUM (SCM_CDR (value));
	}
      else
	{
	  SCM_ASSERT (value == SCM_BOOL_F, value, scm_arg4, s_sys_setsockopt);
	  ling.l_onoff = 0;
	}
      optlen = sizeof (struct linger);
      SCM_DEFER_INTS;
      mem_move (optval, (void *) &ling, optlen);
      SCM_ALLOW_INTS;
    }
#endif
#ifdef SO_SNDBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_SNDBUF)
    {
      SCM_ASSERT (SCM_INUMP (value), value, scm_arg4, s_sys_setsockopt);
      optlen = sizeof (long);
      (*(long *) optval) = (long) SCM_INUM (value);
    }
#endif
#ifdef SO_RCVBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_RCVBUF)
    {
      SCM_ASSERT (SCM_INUMP (value), value, scm_arg4, s_sys_setsockopt);
      optlen = sizeof (long);
      (*(long *) optval) = (long) SCM_INUM (value);
    }
#endif
  else
    {
      /* Most options just take an int.  */
      optlen = sizeof (int);
      SCM_ASSERT (SCM_INUMP (value), value, scm_arg4, s_sys_setsockopt);
      (*(int *) optval) = (int) SCM_INUM (value);
    }
  {
    SCM ans;
    SCM_DEFER_INTS;

    if (SCM_INUMP (sfd))
      fd = SCM_INUM (sfd);
    else
      fd = SCM_FD (sfd);

    rv = setsockopt (fd, ilevel, ioptname, (void *) optval, optlen);
    ans = ((rv == -1) ? scm_makerrno (errno) : SCM_BOOL_T);
    SCM_ALLOW_INTS;
    return ans;
  }
}

SCM_PROC (s_sys_shutdown, "%shutdown", 2, 0, 0, scm_sys_shutdown);
SCM 
scm_sys_shutdown (SCM sfd, SCM how)
{
  SCM_INTS_ENABLED;

  int rv;
  int fd;
  SCM ans;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_shutdown);
  SCM_ASSERT (SCM_INUMP (how) && 0 <= SCM_INUM (how) && 2 >= SCM_INUM (how),
	      how, scm_arg2, s_sys_shutdown);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);

  rv = shutdown (fd, SCM_INUM (how));

  if (rv == -1)
    ans = scm_makerrno (errno);
  else
    ans = SCM_BOOL_T;

  SCM_ALLOW_INTS;

  return ans;
}


#define addr_buffer_size \
  (sizeof (struct sockaddr_un) > sizeof (struct sockaddr_in) \
   ? sizeof (struct sockaddr_un) \
   : sizeof (struct sockaddr_in))

/* convert fam/address/args into a sockaddr of the appropriate type.
 * args is modified by removing the arguments actually used.
 */
static struct sockaddr *
scm_fill_sockaddr (char * buff,
		   int fam,
		   SCM address,
		   SCM *args,
		   int which_arg,
		   SCM proc,
		   int *size)
{
  SCM_INTS_ENABLED;

  switch (fam)
    {
    default:
    case AF_INET:
      {
	SCM isport;
	struct sockaddr_in *soka;

	soka = (struct sockaddr_in *)buff;
	soka->sin_family = AF_INET;
	if (address == s_INADDR_ANY)
	  soka->sin_addr.s_addr = INADDR_ANY;
#if 0
	/* Not supported by BSDI */
	else if (address == s_INADDR_LOOPBACK)
	  soka->sin_addr.s_addr = INADDR_LOOPBACK;
#endif
	else if (address == s_INADDR_BROADCAST)
	  soka->sin_addr.s_addr = INADDR_BROADCAST;
	else
	  soka->sin_addr.s_addr = htonl (scm_num2ulong (address, scm_arg1 - 1 + which_arg, proc));
	SCM_ASSERT (!SCM_IS_IMMEDIATE (*args) && SCM_CONSP (*args), *args, which_arg, proc);
	isport = SCM_CAR (*args);
	*args = SCM_CDR (*args);
	SCM_ASSERT (SCM_INUMP (isport), isport, which_arg + 1, proc);
	soka->sin_port = htons (SCM_INUM (isport));
	*size = sizeof (struct sockaddr_in);
	return (struct sockaddr *) soka;
      }
    case AF_UNIX:
      {
	struct sockaddr_un *soka;

	soka = (struct sockaddr_un *)buff;
	soka->sun_family = AF_UNIX;
	SCM_ASSERT (scm_is_ro_string (address), address,
		    which_arg, proc);
	SCM_ASSERT (SCM_RO_LENGTH (address) < sizeof (soka->sun_path), address,
		    s_path_too_long_for_socket_address, proc);
	SCM_DEFER_INTS;
	mem_move (soka->sun_path, SCM_RO_CHARS (address), SCM_RO_LENGTH (address));
	soka->sun_path[SCM_RO_LENGTH (address)] = 0;
	SCM_ALLOW_INTS;
	*size = sizeof (struct sockaddr_un);
	return (struct sockaddr *) soka;
      }
    }
}

  
SCM_PROC (s_sys_connect, "%connect", 3, 0, 1, scm_sys_connect);
SCM 
scm_sys_connect (SCM sockfd, SCM family, SCM address, SCM args)
{
  SCM_INTS_ENABLED;
  int fam;
  int rv;
  int fd;
  char addr_buffer[addr_buffer_size];
  struct sockaddr *soka;
  int size;
  SCM ans;

  fam = scm_scm2addrtype (family, scm_arg2, s_sys_connect);
  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_connect);
  soka = scm_fill_sockaddr (addr_buffer, fam, address, &args, 3, s_sys_connect, &size);
  SCM_ASSERT ((SCM_EOL == args), args, scm_wna, s_sys_connect);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);
  rv = connect (fd, soka, size);
  ans = ((rv == -1) ? scm_makerrno (errno) : SCM_BOOL_T);
  SCM_ALLOW_INTS;

  return ans;
}


SCM_PROC (s_sys_bind, "%bind", 3, 0, 1, scm_sys_bind);
SCM 
scm_sys_bind (SCM sockfd, SCM family, SCM address, SCM args)
{
  SCM_INTS_ENABLED;
  int fam;
  int rv;
  int fd;
  char addr_buffer[addr_buffer_size];
  struct sockaddr *soka;
  int size;
  SCM ans;

  fam = scm_scm2addrtype (family, scm_arg2, s_sys_bind);
  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_bind);
  soka = scm_fill_sockaddr (addr_buffer, fam, address, &args, 3, s_sys_bind, &size);
  SCM_ASSERT ((SCM_EOL == args), args, scm_wna, s_sys_bind);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd); 
  rv = bind (fd, soka, size);
  ans = ((rv == -1) ? scm_makerrno (errno) : SCM_BOOL_T);
  SCM_ALLOW_INTS;

  return ans; 
}


SCM_PROC (s_sys_listen, "%listen", 2, 0, 0, scm_sys_listen);
SCM 
scm_sys_listen (SCM sfd, SCM backlog)
{
  SCM_INTS_ENABLED;
  int rv;
  int fd;
  SCM ans;

  SCM_ASSERT (SCM_INUMP (sfd) || (!SCM_IS_IMMEDIATE (sfd) && SCM_FDP (sfd)), sfd, scm_arg1, s_sys_listen);
  SCM_ASSERT (SCM_INUMP (backlog), backlog, scm_arg2, s_sys_listen);

  SCM_DEFER_INTS;
  if (SCM_INUMP (sfd))
    fd = SCM_INUM (sfd);
  else
    fd = SCM_FD (sfd);
  rv = listen (fd, SCM_INUM (backlog));
  ans = ((rv == -1) ? scm_makerrno (errno) : SCM_BOOL_T);
  SCM_ALLOW_INTS;

  return ans;
}


/* Put the components of a sockaddr into a new SCM vector.  */
static SCM
scm_addr_vector (struct sockaddr *address)
{
  SCM_INTS_DISABLED;
  short int fam;
  SCM result;
  SCM *ve;

  fam = address->sa_family;

  if (fam == AF_UNIX)
    {
      struct sockaddr_un *nad;
      nad = (struct sockaddr_un *) address;
      result = scm_makvector (2, SCM_UNSPECIFIED, 0, SCM_BOOL_F);
      ve = SCM_VECTOR_ELTS (result);
      ve[0] = scm_addrtype2scm (fam);
      ve[1] = scm_makfromstr (nad->sun_path,
			      (long) strlen (nad->sun_path));
    }
  else if (fam == AF_INET)
    {
      struct sockaddr_in *nad;
      nad = (struct sockaddr_in *) address;
      result = scm_makvector (3, SCM_UNSPECIFIED, 0, SCM_BOOL_F);
      ve = SCM_VECTOR_ELTS (result);
      ve[0] = scm_addrtype2scm (fam);
      ve[1] = scm_ulong2num (ntohl (nad->sin_addr.s_addr));
      ve[2] = scm_ulong2num ((unsigned long) ntohs (nad->sin_port));
    }
  else
    result = SCM_BOOL_F;

  return result;
}

SCM_PROC (s_sys_accept, "%accept", 1, 0, 0, scm_sys_accept);
SCM 
scm_sys_accept (SCM sockfd)
{
  SCM_INTS_ENABLED;

  char addr_buffer[addr_buffer_size];
  int oldfd;
  int newfd;
  int addrlen;
  SCM address;
  SCM sfd;
  SCM ans;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_accept);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    oldfd = SCM_INUM (sockfd);
  else
    oldfd = SCM_FD (sockfd);

  addrlen = addr_buffer_size;

  SCM_ALLOW_INTS;
  newfd = accept (oldfd, (struct sockaddr *) addr_buffer, &addrlen);
  SCM_DEFER_INTS;

  if (newfd == -1)
    ans = scm_makerrno (errno);
  else
    {
      sfd = scm_makefd (newfd, scm_fd_is_open | scm_close_fd_on_gc);
      address = (addrlen
		 ? scm_addr_vector ((struct sockaddr *) addr_buffer)
		 : SCM_BOOL_F);
      ans = scm_listify (sfd, address, SCM_UNDEFINED);
    }
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_sys_getsockname, "%getsockname", 1, 0, 0, scm_sys_getsockname);
SCM 
scm_sys_getsockname (SCM sockfd)
{
  SCM_INTS_ENABLED;

  char addr_buffer[addr_buffer_size];
  int addrlen;
  int rv;
  int fd;
  SCM result;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_getsockname);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  addrlen = addr_buffer_size;

  rv = getsockname (fd, (struct sockaddr *) addr_buffer, &addrlen);
  if (rv == -1)
    result = scm_makerrno (errno);
  else
    result = (addrlen
	      ? scm_addr_vector ((struct sockaddr *) addr_buffer)
	      : SCM_BOOL_F);

  SCM_ALLOW_INTS;

  return result;
}

SCM_PROC (s_sys_getpeername, "%getpeername", 1, 0, 0, scm_sys_getpeername);
SCM 
scm_sys_getpeername (SCM sockfd)
{
  SCM_INTS_ENABLED;

  char addr_buffer[addr_buffer_size];
  int rv;
  int fd;
  int addrlen;
  SCM result;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_getpeername);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  addrlen = addr_buffer_size;

  rv = getpeername (fd, (struct sockaddr *) addr_buffer, &addrlen);
  if (rv == -1)
    result = scm_makerrno (errno);
  else
    result = (addrlen
	      ? scm_addr_vector ((struct sockaddr *) addr_buffer)
	      : SCM_BOOL_F);

  SCM_ALLOW_INTS;

  return result;
}


int
scm_scm2sendrecv_flags (SCM flags, SCM why, SCM caller)
{
  int ans;
  ans = 0;

  while (flags != SCM_EOL)
    {
      SCM_ASSERT (!SCM_IS_IMMEDIATE (flags) && SCM_CONSP (flags), flags, why, caller);
      SCM_ASSERT (scm_is_symbol (SCM_CAR (flags)),
		  flags, why, caller);
      if (SCM_CAR (flags) == scm_MSG_OOB)
	ans |= MSG_OOB;
      else if (SCM_CAR (flags) == scm_MSG_PEEK)
	ans |= MSG_PEEK;
      else if (SCM_CAR (flags) == scm_MSG_DONTROUTE)
	ans |= MSG_DONTROUTE;
      else
	SCM_ASSERT (0, SCM_CAR (flags), s_unrecognized_send_recv_flag, caller);
      flags = SCM_CDR (flags);
    }
  return ans;
}

SCM_PROC (s_sys_recv, "%recv", 2, 1, 0, scm_sys_recv);
SCM
scm_sys_recv (SCM sockfd, SCM buff, SCM flags)
{
  SCM_INTS_ENABLED;

  int rv;
  int fd;
  int flg;
  char *p;
  int size;
  SCM result;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_recv);
  SCM_ASSERT (scm_is_string (buff), buff, scm_arg2, s_sys_recv);

  size = SCM_STRING_LENGTH (buff);
  p = SCM_STRING_CHARS (buff);

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_scm2sendrecv_flags (flags, scm_arg3, s_sys_recv);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  rv = recv (fd, p, size, flg);
  if (rv == -1)
    result = scm_makerrno (errno);
  else
    result = SCM_MAKINUM (rv);

  SCM_ALLOW_INTS;

  return result;
}

SCM_PROC (s_sys_send, "%send", 2, 1, 0, scm_sys_send);
SCM
scm_sys_send (SCM sockfd, SCM message, SCM flags)
{
  SCM_INTS_ENABLED;
  int rv;
  int fd;
  int flg;
  SCM answer;


  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_send);
  SCM_ASSERT (scm_is_ro_string (message), message, scm_arg2, s_sys_send);

  
  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_scm2sendrecv_flags (flags, scm_arg3, s_sys_send);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  rv = send (fd, SCM_RO_CHARS (message), SCM_RO_LENGTH (message), flg);
  answer = ((rv == -1) ? scm_makerrno (errno) : SCM_MAKINUM (rv));

  SCM_ALLOW_INTS;

  return answer;
}

SCM_PROC (s_sys_recvfrom, "%recvfrom", 2, 1, 0, scm_sys_recvfrom);
SCM
scm_sys_recvfrom (SCM sockfd, SCM buff, SCM flags)
{
  SCM_INTS_ENABLED;
  char addr_buffer[addr_buffer_size];
  int addrlen;
  int fd;
  int size;
  char *p;
  int flg;
  int rv;
  SCM address;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_recvfrom);
  SCM_ASSERT (scm_is_string (buff), buff, scm_arg2, s_sys_recvfrom);

  size = SCM_STRING_LENGTH (buff);
  p = SCM_STRING_CHARS (buff);

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_scm2sendrecv_flags (flags, scm_arg3, s_sys_recvfrom);

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  addrlen = addr_buffer_size;

  rv = recvfrom (fd, p, size, flg, (struct sockaddr *) addr_buffer, &addrlen);
  if (rv == -1)
    answer = scm_makerrno (errno);
  else
    {
      address = (addrlen
		 ? scm_addr_vector ((struct sockaddr *) addr_buffer)
		 : SCM_BOOL_F);
      answer = scm_listify (SCM_MAKINUM (rv), address, SCM_UNDEFINED);
    }
  SCM_ALLOW_INTS;
  return answer;
}

SCM_PROC (s_sys_sendto, "%sendto", 4, 0, 1, scm_sys_sendto);
SCM
scm_sys_sendto (SCM sockfd, SCM message, SCM family, SCM address, SCM args_and_flags)
{
  SCM_INTS_ENABLED;
  int fam;
  char addr_buffer[addr_buffer_size];
  int rv;
  int fd;
  int flg;
  struct sockaddr *soka;
  int size;
  SCM answer;

  SCM_ASSERT (SCM_INUMP (sockfd) || (!SCM_IS_IMMEDIATE (sockfd) && SCM_FDP (sockfd)), sockfd, scm_arg1, s_sys_sendto);
  SCM_ASSERT (scm_is_ro_string (message), message, scm_arg2, s_sys_sendto);

  fam = scm_scm2addrtype (family, scm_arg3, s_sys_sendto);


  soka = scm_fill_sockaddr (addr_buffer, fam, address, &args_and_flags, 4, s_sys_sendto, &size);
  if (SCM_EOL == args_and_flags)
    flg = 0;
  else
    {
      SCM_ASSERT (!SCM_IS_IMMEDIATE (args_and_flags) && SCM_CONSP (args_and_flags),
		  args_and_flags, scm_arg5, s_sys_sendto);
      flg = scm_scm2sendrecv_flags (SCM_CAR (args_and_flags), scm_arg5, s_sys_sendto);
    }

  SCM_DEFER_INTS;

  if (SCM_INUMP (sockfd))
    fd = SCM_INUM (sockfd);
  else
    fd = SCM_FD (sockfd);

  rv = sendto (fd, SCM_RO_CHARS (message), SCM_RO_LENGTH (message), flg, soka, size);
  answer = (rv == -1) ? scm_makerrno (errno) : SCM_MAKINUM (rv);

  SCM_ALLOW_INTS;

  return answer;
}


void
scm_init_fdsocket (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/fdsocket.x"
}

