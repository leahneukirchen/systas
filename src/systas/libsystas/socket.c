/* socket.c - socket functions not operating on file descriptors
 *
 ****************************************************************
 * Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include "hackerlab/char/str.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/socket.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/socket.h"




SCM_SYMBOL (s_unknown_h_error, "unknown_h_error");
SCM_SYMBOL (s_NETDB_INTERNAL, "NETDB_INTERNAL");
SCM_SYMBOL (s_HOST_NOT_FOUND, "HOST_NOT_FOUND");
SCM_SYMBOL (s_TRY_AGAIN, "TRY_AGAIN");
SCM_SYMBOL (s_NO_RECOVERY, "NO_RECOVERY");
SCM_SYMBOL (s_NO_DATA, "NO_DATA");


#ifdef AF_UNSPEC
SCM_SYMBOL (scm_AF_UNSPEC, "AF_UNSPEC");
#endif
#ifdef AF_UNIX
SCM_SYMBOL (scm_AF_UNIX, "AF_UNIX");
#endif
#ifdef AF_INET
SCM_SYMBOL (scm_AF_INET, "AF_INET");
#endif

SCM_KEYWORD (kw_name, "name");
SCM_KEYWORD (kw_aliases, "aliases");
SCM_KEYWORD (kw_addrtype, "addrtype");
SCM_KEYWORD (kw_addr_list, "addr_list");
SCM_KEYWORD (kw_net, "net");
SCM_KEYWORD (kw_proto, "proto");
SCM_KEYWORD (kw_port, "port");



SCM_PROC (s_inet_aton, "inet-aton", 1, 0, 0, scm_inet_aton);
SCM 
scm_inet_aton (SCM address)
{
  SCM_INTS_ENABLED;

  struct in_addr soka;
  int rv;
  unsigned long l;

  SCM_ASSERT (scm_is_ro_string (address), address, scm_arg1, s_inet_aton);
  if (scm_is_ro_substr (address))
    address = scm_makfromstr (SCM_RO_CHARS (address), SCM_RO_LENGTH (address));
  SCM_DEFER_INTS;
  rv = inet_aton (SCM_RO_CHARS (address), &soka);
  if (rv)
    l = ntohl (soka.s_addr);
  SCM_ALLOW_INTS;
  return rv ? scm_ulong2num (l) : SCM_BOOL_F;
}


SCM_PROC (s_inet_ntoa, "inet-ntoa", 1, 0, 0, scm_inet_ntoa);
SCM 
scm_inet_ntoa (SCM inetid)
{
  SCM_INTS_ENABLED;

  struct in_addr addr;
  char *s;
  SCM answer;
  unsigned long l;

  l = scm_num2ulong (inetid, scm_arg1, s_inet_ntoa);
  SCM_DEFER_INTS;
  addr.s_addr = htonl (l);
  s = inet_ntoa (addr);
  answer = scm_makfromstr (s, strlen (s));
  SCM_ALLOW_INTS;
  return answer;
}

SCM_PROC (s_inet_netof, "inet-netof", 1, 0, 0, scm_inet_netof);
SCM 
scm_inet_netof (SCM address)
{
  SCM_INTS_ENABLED;

  struct in_addr addr;
  unsigned long l;

  l = scm_num2ulong (address, scm_arg1, s_inet_netof);
  SCM_DEFER_INTS;
  addr.s_addr = htonl (l);
  l = inet_netof (addr);
  SCM_ALLOW_INTS;
  return scm_ulong2num (l);
}


SCM_PROC (s_inet_lnaof, "inet-lnaof", 1, 0, 0, scm_inet_lnaof);
SCM 
scm_inet_lnaof (SCM address)
{
  SCM_INTS_ENABLED;

  struct in_addr addr;
  unsigned long l;

  l = scm_num2ulong (address, scm_arg1, s_inet_lnaof);
  SCM_DEFER_INTS;
  addr.s_addr = htonl (l);
  l = inet_lnaof (addr);
  SCM_ALLOW_INTS;
  return scm_ulong2num (l);
}


SCM_PROC (s_inet_makeaddr, "inet-makeaddr", 2, 0, 0, scm_inet_makeaddr);
SCM 
scm_inet_makeaddr (SCM net, SCM lna)
{
  SCM_INTS_ENABLED;

  struct in_addr addr;
  unsigned long netnum;
  unsigned long lnanum;
  unsigned long l;

  netnum = scm_num2ulong (net, scm_arg1, s_inet_makeaddr);
  lnanum = scm_num2ulong (lna, scm_arg2, s_inet_makeaddr);
  SCM_DEFER_INTS;
  addr = inet_makeaddr (netnum, lnanum);
  l = ntohl (addr.s_addr);
  SCM_ALLOW_INTS;
  return scm_ulong2num (l);
}

SCM
scm_addrtype2scm (int n)
{
  switch (n)
    {
#ifdef AF_UNIX
    case AF_UNIX:
      return scm_AF_UNIX;
#endif
#ifdef AF_INET
    case AF_INET:
      return scm_AF_INET;
#endif
#ifdef AF_UNSPEC
    case AF_UNSPEC:
      return scm_AF_UNSPEC;
#endif
    default:
      return SCM_MAKINUM (n);
    }
}

int
scm_scm2addrtype (SCM s, SCM pos, SCM s_caller)
{
#ifdef AF_UNIX
 if (s == scm_AF_UNIX)
   return AF_UNIX;
#endif
#ifdef AF_INET
 if (s == scm_AF_INET)
   return AF_INET;
#endif
#ifdef AF_UNSPEC
 if (s == scm_AF_UNSPEC)
   return AF_UNSPEC;
#endif
 SCM_ASSERT (SCM_INUMP (s), s, pos, s_caller);
 return SCM_INUM (s);
}

static SCM
scm_hostent2scm (struct hostent * entry)
{
  SCM_INTS_DISABLED;
  struct in_addr inad;
  SCM ans;
  SCM lst;
  char **argv;
  int i;

  if (!entry)
    return SCM_BOOL_F;

  if (sizeof (struct in_addr) != entry->h_length)
    lst = SCM_EOL;
  else
    {
      lst = SCM_EOL;
      i = 0;
      for (argv = entry->h_addr_list; argv[i]; i++);
      while (i--)
	{
	  inad = *(struct in_addr *) argv[i];
	  lst = scm_cons (scm_ulong2num (ntohl (inad.s_addr)), lst);
	}
    }
  
  ans = scm_listify (kw_name, scm_makfromstr ((t_uchar *)entry->h_name, (long) strlen (entry->h_name)),
		     kw_aliases, scm_argv2scm (-1, (t_uchar **)entry->h_aliases),
		     kw_addrtype, scm_addrtype2scm (entry->h_addrtype),
		     kw_addr_list, lst,
		     SCM_UNDEFINED);
  return ans;
}


SCM_PROC (s_gethostent, "gethostent", 0, 0, 0, scm_gethostent);
SCM 
scm_gethostent (void)
{
  SCM_INTS_ENABLED;

  struct hostent *entry;
  SCM answer;


  SCM_DEFER_INTS;
  entry = gethostent ();
  answer = scm_hostent2scm (entry);
  SCM_ALLOW_INTS;
  return answer;
}

static SCM
scm_hosterror (void)
{
  SCM_INTS_UNKNOWN;

  switch (h_errno)
    {
    default:
      return s_unknown_h_error;
    case NETDB_INTERNAL:
      return s_NETDB_INTERNAL;
    case HOST_NOT_FOUND:
      return s_HOST_NOT_FOUND;
    case TRY_AGAIN:
      return s_TRY_AGAIN;
    case NO_RECOVERY:
      return s_NO_RECOVERY;
    case NO_DATA:
      return s_NO_DATA;
    }
}

SCM_PROC (s_gethostbyname, "gethostbyname", 1, 0, 0, scm_gethostbyname);
SCM 
scm_gethostbyname (SCM name)
{
  SCM_INTS_ENABLED;

  struct hostent *entry;
  SCM ans;

  if (scm_is_ro_substr (name))
    name = scm_makfromstr (SCM_RO_CHARS (name), SCM_RO_LENGTH (name));

  SCM_ASSERT (scm_is_ro_string (name), name, scm_arg1, s_gethostbyname);
  SCM_DEFER_INTS;
  entry = gethostbyname (SCM_RO_CHARS (name));
  if (!entry)
    ans = scm_hosterror ();
  else
    ans = scm_hostent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_gethostbyaddr, "gethostbyaddr", 1, 0, 0, scm_gethostbyaddr);
SCM 
scm_gethostbyaddr (SCM name)
{
  SCM_INTS_ENABLED;

  struct in_addr inad;
  struct hostent *entry;
  SCM ans;
  unsigned long l;

  l = scm_num2ulong (name, scm_arg1, s_gethostbyaddr);
  SCM_DEFER_INTS;
  {
    inad.s_addr = htonl (l);
    entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
  }
  if (!entry)
    ans = scm_hosterror ();
  else
    ans = scm_hostent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

static SCM
scm_netent2scm (struct netent *entry)
{
  SCM_INTS_DISABLED;
  SCM ans;
  
  if (!entry)
    return SCM_BOOL_F;

  ans = scm_listify (kw_name, scm_makfromstr (entry->n_name, (long) strlen (entry->n_name)),
		     kw_aliases, scm_argv2scm (-1, (t_uchar **)entry->n_aliases),
		     kw_addrtype, scm_addrtype2scm (entry->n_addrtype),
		     kw_net, scm_ulong2num (entry->n_net),
		     SCM_UNDEFINED);
  return ans;
}

SCM_PROC (s_getnetent, "getnetent", 0, 0, 0, scm_getnetent);
SCM 
scm_getnetent (void)
{
  SCM_INTS_ENABLED;

  struct netent *entry;
  SCM ans;

  SCM_DEFER_INTS;
  entry = getnetent ();
  ans = scm_netent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_getnetbyname, "getnetbyname", 1, 0, 0, scm_getnetbyname);
SCM 
scm_getnetbyname (SCM name)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct netent *entry;

  if (scm_is_ro_substr (name))
    name = scm_makfromstr (SCM_RO_CHARS (name), SCM_RO_LENGTH (name));

  SCM_ASSERT (scm_is_ro_string (name), name, scm_arg1, s_getnetbyname);
  SCM_DEFER_INTS;
  entry = getnetbyname (SCM_RO_CHARS (name));
  ans = scm_netent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}



SCM_PROC (s_getnetbyaddr, "getnetbyaddr", 1, 0, 0, scm_getnetbyaddr);
SCM 
scm_getnetbyaddr (SCM name)
{
  SCM_INTS_ENABLED;

  unsigned long netnum;
  struct netent *entry;
  SCM ans;

  netnum = scm_num2ulong (name, scm_arg1, s_getnetbyaddr);
  SCM_DEFER_INTS;
  entry = getnetbyaddr (netnum, AF_INET);
  ans = scm_netent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

static SCM
scm_protoent2scm (struct protoent *entry)
{
  SCM_INTS_DISABLED;
  SCM ans;

  if (!entry)
    return SCM_BOOL_F;

  ans = scm_listify (kw_name, scm_makfromstr (entry->p_name, (long) strlen (entry->p_name)),
		     kw_aliases, scm_argv2scm (-1, (t_uchar **)entry->p_aliases),
		     kw_proto, SCM_MAKINUM (entry->p_proto),
		     SCM_UNDEFINED);
  
  return ans;  
}

SCM_PROC (s_getprotoent, "getprotoent", 0, 0, 0, scm_getprotoent);
SCM 
scm_getprotoent (void)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct protoent *entry;

  SCM_DEFER_INTS;
  entry = getprotoent ();
  ans = scm_protoent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_getprotobyname, "getprotobyname", 1, 0, 0, scm_getprotobyname);
SCM 
scm_getprotobyname (SCM name)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct protoent *entry;

  if (scm_is_ro_substr (name))
    name = scm_makfromstr (SCM_RO_CHARS (name), SCM_RO_LENGTH (name));

  SCM_ASSERT (scm_is_ro_string (name), name, scm_arg1, s_getprotobyname);
  SCM_DEFER_INTS;
  entry = getprotobyname (SCM_RO_CHARS (name));
  ans = scm_protoent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_protobynumber, "protobynumber", 1, 0, 0, scm_protobynumber);
SCM 
scm_protobynumber (SCM name)
{
  SCM_INTS_ENABLED;

  unsigned long protonum;
  struct protoent *entry;
  SCM ans;

  protonum = scm_num2ulong (name, scm_arg1, s_protobynumber);
  SCM_DEFER_INTS;
  entry = getprotobynumber ((int)protonum);
  ans = scm_protoent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}


static SCM
scm_servent2scm (struct servent *entry)
{
  SCM_INTS_DISABLED;
  SCM ans;

  if (!entry)
    return SCM_BOOL_F;

  ans = scm_listify (kw_name, scm_makfromstr (entry->s_name, (long) strlen (entry->s_name)),
		     kw_aliases, scm_argv2scm (-1, (t_uchar **)entry->s_aliases),
		     kw_port, SCM_MAKINUM (ntohs (entry->s_port)),
		     kw_proto, scm_makfromstr (entry->s_proto, (long) str_length (entry->s_proto)),
		     SCM_UNDEFINED);
  return ans;
}

SCM_PROC (s_getservent, "getservent", 0, 0, 0, scm_getservent);
SCM 
scm_getservent (void)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct servent *entry;

  SCM_DEFER_INTS;
  entry = getservent ();
  ans = scm_servent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}


SCM_PROC (s_getservbyname, "getservbyname", 2, 0, 0, scm_getservbyname);
SCM 
scm_getservbyname (SCM name, SCM proto)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct servent *entry;

  if (scm_is_ro_substr (name))
    name = scm_makfromstr (SCM_RO_CHARS (name), SCM_RO_LENGTH (name));

  SCM_ASSERT (scm_is_ro_string (name), name, scm_arg1, s_getservbyname);

  if (scm_is_ro_substr (proto))
    proto = scm_makfromstr (SCM_RO_CHARS (proto), SCM_RO_LENGTH (proto));

  SCM_ASSERT (scm_is_ro_string (proto), proto, scm_arg2, s_getservbyname);

  SCM_DEFER_INTS;
  entry = getservbyname (SCM_RO_CHARS (name), SCM_RO_CHARS (proto));
  ans = scm_servent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}


SCM_PROC (s_getservbyport, "getservbyport", 2, 0, 0, scm_getservbyport);
SCM 
scm_getservbyport (SCM port, SCM proto)
{
  SCM_INTS_ENABLED;

  SCM ans;
  struct servent *entry;

  SCM_ASSERT (SCM_INUMP (port), port, scm_arg1, s_getservbyport);

  if (scm_is_ro_substr (proto))
    proto = scm_makfromstr (SCM_RO_CHARS (proto), SCM_RO_LENGTH (proto));

  SCM_ASSERT (scm_is_ro_string (proto), proto, scm_arg2, s_getservbyport);

  SCM_DEFER_INTS;
  entry = getservbyport (SCM_INUM (port), SCM_RO_CHARS (proto));
  ans = scm_servent2scm (entry);
  SCM_ALLOW_INTS;
  return ans;
}


SCM_PROC (s_sethostent, "sethostent", 0, 1, 0, scm_sethostent);
SCM 
scm_sethostent (SCM arg)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  if (SCM_UNBNDP (arg))
    arg = SCM_BOOL_F;
  sethostent (SCM_BOOL_F != arg);
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_endhostent, "endhostent", 0, 0, 0, scm_endhost);
SCM 
scm_endhost (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  endhostent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_setnetent, "setnetent", 0, 1, 0, scm_setnetent);
SCM 
scm_setnetent (SCM arg)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  if (SCM_UNBNDP (arg))
    arg = SCM_BOOL_F;
  setnetent (SCM_BOOL_F != arg);
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_endnetent, "endnetent", 0, 0, 0, scm_endnet);
SCM 
scm_endnet (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  endnetent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_setservent, "setservent", 0, 1, 0, scm_setservent);
SCM 
scm_setservent (SCM arg)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  if (SCM_UNBNDP (arg))
    arg = SCM_BOOL_F;
  setservent (SCM_BOOL_F != arg);
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_endservent, "endservent", 0, 0, 0, scm_endserv);
SCM 
scm_endserv (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  endservent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_setprotoent, "setprotoent", 0, 1, 0, scm_setprotoent);
SCM 
scm_setprotoent (SCM arg)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  if (SCM_UNBNDP (arg))
    arg = SCM_BOOL_F;
  setprotoent (SCM_BOOL_F != arg);
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


SCM_PROC (s_endprotoent, "endprotoent", 0, 0, 0, scm_endproto);
SCM 
scm_endproto (void)
{
  SCM_INTS_ENABLED;

  SCM_DEFER_INTS;
  endprotoent ();
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}



#ifndef INITIAL_HOSTNAME_LENGTH
#define INITIAL_HOSTNAME_LENGTH 33
#endif

static char *
xgethostname (void)
{
  SCM_INTS_UNKNOWN;
  char *hostname;
  size_t size;
  size_t old_size;
  int err;

  size = INITIAL_HOSTNAME_LENGTH;
  hostname = (char *)malloc (size);
  if (!hostname)
    return 0;
  while (1)
    {
      hostname[size - 1] = '\0';
      err = gethostname (hostname, size);
      if (err == 0 && hostname[size - 1] == '\0')
	break;
      old_size = size;
      size *= 2;
      hostname = (char *)realloc (hostname, size);
      if (!hostname)
	return 0;
    }

  return hostname;
}

SCM_PROC (s_gethostname, "gethostname", 0, 0, 0, scm_gethostname);
SCM
scm_gethostname (void)
{
  SCM_INTS_ENABLED;

  char * it;
  SCM answer;

  SCM_DEFER_INTS;
  it = xgethostname ();
  if (!it)
    answer = scm_makerrno (errno);
  else
    {
      answer = scm_take_str0 (it);
    }
  SCM_ALLOW_INTS;
  return answer;
}





void 
scm_init_socket (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/socket.x"
}


