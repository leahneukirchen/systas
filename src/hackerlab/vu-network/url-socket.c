/* url-socket.c - File namespace extensions.
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/os/sys/param.h"
#include "hackerlab/os/sys/socket.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/mem/must-malloc.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/fs/file-names.h"
#include "hackerlab/vu/vu-sys.h"
#include "hackerlab/vu/vu-bad-arg.h"
#include "hackerlab/vu-network/url-socket.h"


/************************************************************************
 *(h0 "A VU Handler for the URL Socket Scheme")
 * 
 * These functions provide VU namespace handlers for file-names
 * that designate various kinds of sockets.  By using these handlers,
 * your programs can (almost) transparently handle filenames
 * like:
 * 
 *	inet://myserver.host.com:10000
 * 
 * or
 * 
 * 	unix:/tmp/.X11-unix/X0
 *
 * Two kinds of sockets are available: *server* sockets and *client*
 * sockets.
 * 
 * "server sockets" are created with `socket(2)', `bind(2)', and
 * `listen(2)'.  When `vu_open' is called, a call to `listen(2)' is
 * made and the client connection (if any) is returned.
 *
 * "client sockets" are created with `socket(2)', `bind(2)', and
 * `connect(2)'.
 * 
 * Sockets may be named in either the `unix' or `inet' domains.
 *
 * The syntax of these URLs is described in the documentation for
 * `url_socket_push_client_handler' and
 * `url_socket_push_socket_handler'.
 */


/* struct url_socket_params
 *
 * The closure type for every file name pattern than can open a socket
 * URL.  A pointer to one of these structures is used as the closure
 * when calling `vu_push_name_handler'.
 *
 * These closures are copied by must_malloc/mem_move.
 */
struct url_socket_params
{
  enum url_socket_type type;					/* What type of socket (server, client, both) is opened by this file name pattern? */
  enum url_socket_domains domains;				/* What domain (unix, inet, both) is opened by this file name pattern? */
  int server_flags;						/* How was the server opened? (O_NONBLOCK, etc.) */
  int default_port;						/* If the port isn't specified in the file name, this is the default. */
  int backlog;							/* For listen(2). */
  url_socket_server_callback server_callback;			/* Called back when a new server socket is created and bound. */
  url_socket_connect_callback connection_callback;		/* Called when a server socket receives a new connection. */
  url_socket_server_close_callback server_close_callback;	/* Called when a server socket is closed. */
  void * closure;						/* A closure for the two callbacks. */
};



/* url_socket_vtable
 *
 * System calls for functions that operate on descriptors, URL
 * handling for functions that operate on file names.
 */
static struct vu_fs_discipline url_socket_vtable;


/* url_server_socket_vtable
 *
 * Errors for most functions.  Server shut-down for `vu_close'.
 */
static struct vu_fs_discipline url_server_socket_vtable;


/* __STDC__ prototypes for static functions */
static regex_t * url_socket_client_regex (t_uchar ** name,
					  t_uchar *** doc,
					  enum url_socket_domains domains,
					  int default_port);
static regex_t * url_socket_server_regex (t_uchar ** name,
					  t_uchar *** doc,
					  enum url_socket_domains domains,
					  int may_be_client,
					  int default_port);
static regex_t * url_socket_only_server_regex (t_uchar ** name,
					       t_uchar *** doc,
					       enum url_socket_domains domains,
					       int default_port);
static int server_fd_for_addr (int * errn,
			       struct sockaddr * addr,
			       int addr_len,
			       char * path,
			       int flags,
			       int mode,
			       struct url_socket_params * params);
static struct server_memo * find_memo_for_server_fd (int fd);
static void remove_memo_for_server_fd (int fd);
static int decode_socket_url (int * errn,
			      struct sockaddr * addr,
			      int * addr_len,
			      struct url_socket_params * params,
			      int * be_a_server,
			      char * path,
			      int be_a_server_hint);
static void * url_socket_make_closure (void * closure);
static void url_socket_free_closure (void * closure);
static int url_socket_open (int * errn, char * path, int flags, int mode, void * closure);
static int url_socket_close (int * errn, int fd, void * closure);
static int url_socket_access (int * errn, char * path, int mode, void * closure);
static int url_socket_chdir (int * errn, char * path, void * closure);
static int url_socket_chmod (int * errn, char * path, int mode, void * closure);
static int url_socket_chown (int * errn, char * path, int owner, int group, void * closure);
static int url_socket_chroot (int * errn, char * path, void * closure);
static int url_socket_closedir (int * errn, DIR * dir, void * closure);
static int url_socket_fchdir (int * errn, int fd, void * closure);
static int url_socket_fchmod (int * errn, int fd, int mode, void * closure);
static int url_socket_fchown (int * errn, int fd, int owner, int group, void * closure);
static int url_socket_fstat (int * errn, int fd, struct stat * buf, void * closure);
static int url_socket_fsync (int * errn, int fd, void * closure);
static int url_socket_ftruncate (int * errn, int fd, off_t where, void * closure);
static int url_socket_link (int * errn, char * from, char * to, void * closure);
static off_t url_socket_lseek (int * errn, int fd, off_t offset, int whence, void * closure);
static int url_socket_lstat (int * errn, char * path, struct stat * buf, void * closure);
static int url_socket_mkdir (int * errn, char * path, int mode, void * closure);
static int url_socket_opendir (int * errn, DIR ** retv,  char * path, void * closure);
static ssize_t url_socket_read (int * errn, int fd, char * buf, size_t count, void * closure);
static int url_socket_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure);
static int url_socket_readlink (int * errn, char * path, char * buf, int bufsize, void * closure);
static int url_socket_rename (int * errn, char * from, char * to, void * closure);
static int url_socket_rmdir (int * errn, char * path, void * closure);
static int url_socket_stat (int * errn, char * path, struct stat * buf, void * closure);
static int url_socket_symlink (int * errn, char * from, char * to, void * closure);
static int url_socket_truncate (int * errn, char * path, off_t where, void * closure);
static int url_socket_unlink (int * errn, char * path, void * closure);
static int url_socket_utime (int * errn, char * path, struct utimbuf * times, void * closure);
static ssize_t url_socket_write (int * errn, int fd, char * buf, size_t count, void * closure);
static int url_socket_fcntl (int * errn, int fd, int cmd, long arg, void * closure);
static int url_socket_dup (int * errn, int fd, void * closure);
static int url_socket_dup2 (int * errn, int fd, int newfd, void * closure);
static int url_socket_move_state (int * errn, int fd, int newfd, void * closure);
static int url_server_socket_close (int * errn, int fd, void * closure);





/* static regex_t * url_socket_client_regex (t_uchar * name,
 *					     t_uchar *** doc,
 *					     enum url_socket_domains domains,
 *					     int default_port);
 *
 * Return a file name regexp and namespace option name for client
 * sockets in the indicated domains.
 *
 *	Domains:		Regexp:			Name:
 *	-----------------------------------------------------------
 *	unix			unix:			client/unix
 *	inet			inet://			client/inet
 *	both			unix:\|inet://		client/any
 */
static regex_t *
url_socket_client_regex (t_uchar ** name,
			 t_uchar *** doc,
			 enum url_socket_domains domains,
			 int default_port)
{
  switch (domains)
    {
    default:
      panic ("illegal domains specification to url_socket_push_client_handler");
    case url_socket_unix:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "unix:$PATH",
	    "Client connection to a unix-domain socket.",
	    "$PATH is ~-expanded.",
	    0
	  };


	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^unix:", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "client/unix";
	*doc = docstr;
	return &regex;
      }
    case url_socket_inet:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "inet:$HOST:$PORT",
	    "Client connection to an internet-domain",
	    "socket.",
	    0
	  };
	static t_uchar * docstr_opt_port[] =
	  {
	    "inet:$HOST[:$PORT]",
	    "Client connection to an internet-domain",
	    "socket.",
	    0
	  };

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^inet://", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "client/inet";
	*doc = ((default_port < 0) ? docstr : docstr_opt_port);
	return &regex;
      }
    case url_socket_inet_or_unix:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "unix:$PATH or inet:$HOST:$PORT",
	    "Client connection to a unix or",
	    "internet domain socket.  $PATH",
	    "is ~-expanded.",
	    0
	  };
	static t_uchar * docstr_opt_port[] =
	  {
	    "unix:$PATH or inet:$HOST[:$PORT]",
	    "Client connection to a unix or",
	    "internet domain socket.  $PATH",
	    "is ~-expanded.",
	    0
	  };
	
	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^[[:(unix:\\|inet://):]]", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "client/any";
	*doc = ((default_port < 0) ? docstr : docstr_opt_port);
	return &regex;
      }
    }
}


/* static regex_t * url_socket_server_regex (t_uchar ** name,
 *					     t_uchar *** doc,
 *					     enum url_socket_domains domains,
 *					     int may_be_client,
 *					     int default_port);
 *
 * Return a file name regexp and option argument name for server
 * sockets in the indicated domains.  Regexps that can also be used
 * for client sockets are included.
 *
 *	Domains:		Regexp:
 *	--------------------------------------
 *	unix			unix:\|unix-server:
 *	inet			inet://\|inet-server://
 *	both			unix:\|unix-server:\|inet://\|inet-server://
 *
 * See the comment for `url_socket_push_socket_handler' for the names
 * associated with each combination of `domains' and `may_be_client'.
 */
static regex_t *
url_socket_server_regex (t_uchar ** name,
			 t_uchar *** doc,
			 enum url_socket_domains domains,
			 int may_be_client,
			 int default_port)
{
  switch (domains)
    {
    default:
      panic ("illegal domains specification to url_socket_push_client_handler");
    case url_socket_unix:
      {
	static int initialized = 0;
	static regex_t regex;

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^[[:(unix:\\|unix-server:):]]", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	if (may_be_client)
	  {
	    static t_uchar * docstr[] =
	      {
		"unix:$PATH or unix-server:$PATH",
		"Be a server or client for a unix-domain",
		"socket. $PATH is ~-expanded.",
		0
	      };
	    *name = "socket/unix";
	    *doc = docstr;
	  }
	else
	  {
	    static t_uchar * docstr[] =
	      {
		"unix:$PATH or unix-server:$PATH",
		"Be a server for a unix-domain socket.",
		"$PATH is ~-expanded.",
		0
	      };
	    *name = "server/unix";
	    *doc = docstr;
	  }
	return &regex;
      }
    case url_socket_inet:
      {
	static int initialized = 0;
	static regex_t regex;

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^[[:(inet://\\|inet-server://):]]", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	if (may_be_client)
	  {
	    static t_uchar * docstr[] =
	      {
		"inet:$HOST:$PORT",
		"  or inet-server:$PATH:$PORT",
		"Be a server or client for an",
		"internet-domain socket.",
		0
	      };
	    static t_uchar * docstr_opt_port[] =
	      {
		"inet:$HOST[:$PORT]",
		"  or inet-server:$PATH[:$PORT]",
		"Be a server or client for an",
		"internet-domain socket.",
		0
	      };
	    *name = "socket/inet";
	    *doc = ((default_port < 0) ? docstr : docstr_opt_port);
	  }
	else
	  {
	    static t_uchar * docstr[] =
	      {
		"inet:$HOST:$PORT",
		"  or inet-server:$PATH:$PORT",
		"Be a server for an internet-domain",
		"socket.",
		0
	      };
	    static t_uchar * docstr_opt_port[] =
	      {
		"inet:$HOST[:$PORT]",
		"  or inet-server:$PATH[:$PORT]",
		"Be a server for an internet-domain",
		"socket.",
		0
	      };
	    *name = "server/inet";
	    *doc = ((default_port < 0) ? docstr : docstr_opt_port);
	  }
	return &regex;
      }
    case url_socket_inet_or_unix:
      {
	static int initialized = 0;
	static regex_t regex;
	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^[[:(unix:\\|unix-server:\\|inet://\\|inet-server://):]]", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	if (may_be_client)
	  {
	    static t_uchar * docstr[] =
	      {
		"unix:$PATH, unix-server:$PATH",
		"inet:$HOST:$PORT",
		"or inet-server:$HOST:$PORT",
		"Be a client or server for a unix or",
		"internet domain socket. $PATH is",
		"~-expanded.",
		0
	      };
	    static t_uchar * docstr_opt_port[] =
	      {
		"unix:$PATH, unix-server:$PATH",
		"inet:$HOST:[$PORT]",
		"or inet-server:$HOST:[$PORT]",
		"Be client or a server for a unix or internet",
		"domain socket. $PATH is ~-expanded.",
		0
	      };
	    *name = "socket/any";
	    *doc = ((default_port < 0) ? docstr : docstr_opt_port);
	  }
	else
	  {
	    static t_uchar * docstr[] =
	      {
		"unix:$PATH, unix-server:$PATH",
		"inet:$HOST:$PORT",
		"or inet-server:$HOST:$PORT",
		"Be a server for a unix or internet",
		"internet domain socket. $PATH is",
		"~-expanded.",
		0
	      };
	    static t_uchar * docstr_opt_port[] =
	      {
		"unix:$PATH, unix-server:$PATH",
		"inet:$HOST:[$PORT]",
		"or inet-server:$HOST:[$PORT]",
		"Be server for a unix or internet domain",
		"socket. $PATH is ~-expanded.",
		0
	      };
	    *name = "server/any";
	    *doc = ((default_port < 0) ? docstr : docstr_opt_port);
	  }
	return &regex;
      }
    }
}


/* static regex_t * url_socket_only_server_regex (t_uchar ** name,
 *						  t_uchar *** doc,
 *						  enum url_socket_domains domains,
 *						  int default_port);
 *
 * Return a file name regexp and option argument name for server
 * sockets in the indicated domains.  Regexps that can also be used
 * for client sockets are excluded.
 *
 *	Domains:		Regexp:
 *	--------------------------------------
 *	unix			unix-server:
 *	inet			inet-server://
 *	both			unix-server:\|inet-server://
 *
 * See the comment for `url_socket_push_socket_handler' for the names
 * associated with each domain.
 */
static regex_t *
url_socket_only_server_regex (t_uchar ** name,
			      t_uchar *** doc,
			      enum url_socket_domains domains,
			      int default_port)
{
  switch (domains)
    {
    default:
      panic ("illegal domains specification to url_socket_push_client_handler");
    case url_socket_unix:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "unix-server:$PATH",
	    "Be a server for a unix-domain socket.",
	    "$PATH is ~-expanded.",
	    0
	  };

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^unix-server:", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "socket/unix-server";
	*doc = docstr;
	return &regex;
      }
    case url_socket_inet:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "inet-server:$PATH:$PORT",
	    "Be a server for an internet-domain",
	    "socket.",
	    0
	  };
	static t_uchar * docstr_opt_port[] =
	  {
	    "inet-server:$PATH[:$PORT]",
	    "Be a server for an internet-domain",
	    "socket.",
	    0
	  };

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^inet-server://", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "socket/inet-server";
	*doc = ((default_port < 0) ? docstr : docstr_opt_port);
	return &regex;
      }
    case url_socket_inet_or_unix:
      {
	static int initialized = 0;
	static regex_t regex;
	static t_uchar * docstr[] =
	  {
	    "unix-server:$PATH",
	    "or inet-server:$HOST:$PORT",
	    "Be a server for a unix or internet",
	    "internet domain socket. $PATH is",
	    "~-expanded.",
	    0
	  };
	static t_uchar * docstr_opt_port[] =
	  {
	    "unix-server:$PATH",
	    "or inet-server:$HOST:[$PORT]",
	    "Be server for a unix or internet domain",
	    "socket. $PATH is ~-expanded.",
	    0
	  };

	if (!initialized)
	  {
	    if (0 > regcomp (&regex, "^[[:(unix-server:\\|inet-server://):]]", 0))
	      panic ("unable to compile socket url regexp");
	    initialized = 1;
	  }
	*name = "socket/any-server";
	*doc = ((default_port < 0) ? docstr : docstr_opt_port);
	return &regex;
      }
    }
}



/*(c url_socket_push_client_handler)
 * void url_socket_push_client_handler (enum url_socket_domains domains,
 *					int default_port,
 *					int is_optional);
 *
 * Push a VU namespace handler for client sockets.
 *
 *	Domains:			File Name:
 *	------------------------------------------------------------------
 *	url_socket_unix			unix:$PATH
 *	url_socket_inet			inet://$HOST[:$PORT]
 *	url_socket_inet_or_unix		unix:$PATH or inet://$HOST[:$PORT]
 *
 * Note that the `$PATH' of a unix domain socket is subject to "~" expansion
 * using `file_name_tilde_expand'.
 * 
 * (See xref:"file_name_tilde_expand".)
 *
 * `default_port' is used for internet-domain sockets.
 *
 * If `is_optional' is 0, the handler is immediately added to the VU namespace.
 * If `is_optional' is not 0, the handler is added to the list of optional
 * handlers and may be enabled by `vu_enable_optional_name_handler'.
 * 
 * (See xref:"vu_enable_optional_name_handler".)
 *
 *	
 *	Domains:			Optional Handler Name:
 *	------------------------------------------------------------------
 *	url_socket_unix			client/unix
 *	url_socket_inet			client/inet
 *	url_socket_inet_or_unix		client/any
 *
 */
void
url_socket_push_client_handler (enum url_socket_domains domains,
				int default_port,
				int is_optional)
{
  regex_t * regex;
  struct url_socket_params params;
  t_uchar * name;
  t_uchar ** doc;

  regex = url_socket_client_regex (&name, &doc, domains, default_port);
  params.type = url_socket_client;
  params.domains = domains;
  params.default_port = default_port;
  params.backlog = 0;
  params.server_callback = 0;
  params.connection_callback = 0;
  params.server_close_callback = 0;
  vu_push_name_handler (name, doc, regex, 0, &url_socket_vtable, (void *)&params, is_optional);
}


/*(c url_socket_push_socket_handler)
 * void url_socket_push_socket_handler 
 *	(enum url_socket_domains domains,
 *	 int server_flags,
 *	 int may_be_client,
 *       int only_server_url,
 *	 int default_port,
 *	 int backlog,
 *	 url_socket_server_callback server_callback,
 *	 url_socket_connect_callback connection_callback,
 *	 url_socket_server_close_callback server_close_callback,
 *	 void * closure,
 *	 is_optional);
 *
 * Push a VU namespace handler for server sockets.  After this call, a
 * call to `vu_open' with a suitable file name will work by creating a
 * socket.
 *
 * `server_flags' may be any bitwise combination of:
 *
 *		O_NONBLOCK	# Don't block waiting for connections.
 *
 * There are two kinds of file name patterns: those that can be used
 * to name both clients and servers, and those that can only be used
 * to name servers:
 *
 *	server only:			client or server:
 *	-------------------------------------------------
 *	unix-server:$PATH		unix:$PATH
 *	inet-server://$HOST[:$PORT]	inet://$HOST[:$PORT]
 *
 * By default, both kinds of URL are accepted and interpreted as names
 * of server sockets.  Note that:
 *
 *	`$PATH' of a unix domain socket is  subject to "~" expansion using 
 *	`file_name_tilde_expand'.
 *
 *	`$HOST' of an internet-domain socket may be `INADDR_LOOPBACK'.
 *	If the URL is the name of a server, `$HOST' may also be
 *	`INADDR_ANY'.  (See `inet(4)'.)
 *
 *	`$PORT' of an internet-domain server may be 0, meaning that the
 *	system should assign a port number which will be reported in 
 *	the server callback in the `server_addr' parameter (see below).
 *
 * If `may_be_client' is not 0, then accept both kinds of URLs, but
 * interpret "client or server" URLs as naming client sockets (your
 * process will open a connection to some other server).
 *
 * If `only_server_url' is not 0, then accept "server only" URLs, but
 * not "client or server" URLs.  In this case, `may_be_client' is
 * ignored.  This is especially useful in combination with
 * `url_socket_push_client_handler' and other calls to
 * `vu_push_name_handler'.
 *
 * `default_port' is used when creating an internet-domain socket for
 * which the port number was not specified in the file-name.
 *
 * `backlog' is used for the call to `listen(2)'.
 *
 * `server_callback' is called when a new server socket has been
 * created successfully.  It's arguments are explained below.
 *
 * `connect_callback' is called when a new connection has been
 * received.  It's arguments are explained below.
 *
 * `server_close_callback' is called when the server descriptor
 * is closed.  It's arguments are explained below.
 *
 * `closure' is an opaque value passed to the callback functions.
 *
 * If `is_optional' is 0, the handler is immediately added to the VU namespace.
 * If `is_optional' is non-0, the handler is added to the list of optional
 * handlers and may be enabled by `vu_enable_optional_name_handler'.
 *
 *   domain(s) only_server_url may_be_client         Optional Handler Name
 *   --------------------------------------------------------------------
 *   unix            0               0               server/unix
 *   inet            0               0               server/inet
 *   inet_or_unix    0               0               server/any
 *   unix            0               1               socket/unix
 *   inet            0               1               socket/inet
 *   inet_or_unix    0               1               socket/any
 *   unix            1               *               socket/unix-server
 *   inet            1               *               socket/inet-server
 *   inet_or_unix    1               *               socket/any-server
 *
 * 
 * \Calling Conventions for Callbacks/
 *
 *  typedef void (*url_socket_server_callback) (char * path,
 * 					        int server_flags,
 *					        int flags,
 *					        int mode,
 *					        int server_fd,
 *					        struct sockaddr * server_addr,
 *					        int server_addr_len,
 *					        void * closure);
 *
 *	`path' is the file name that caused the server socket to
 *	be created.
 *
 *	`server_flags' is 0 or a bit-wise combination of `O_NONBLOCK'.
 *
 *	`flags' is the `flags' parameter passed to `vu_open', if this
 *	server socket was created by a call to `vu_open', 0 otherwise.
 *
 *	`mode' is the `mode' parameter passed to `vu_open', if this
 *	server socket was created by a call to `vu_open', 0 otherwise.
 *
 *	`server_fd' is the descriptor number of the server socket.
 *
 *	`server_addr' and `server_addr_len' is the address passed to
 *	`bind'.
 *
 *	`closure' is the `closure' argument passed to
 *	`url_socket_push_socket_handler'.
 *
 *  typedef void (*url_socket_connect_callback) 
 *		(char * path,
 *		 int flags,
 *		 int mode,
 *		 int server_fd,
 *		 int connection_fd,
 *		 struct sockaddr * client_addr,
 *		 int client_addr_len,
 *		 void * closure);
 *
 *	`path' is the file name that caused the client connection to this
 *	server to be created.
 *
 *	`flags' is the `flags' parameter passed to `vu_open'.
 *
 *	`mode' is the `mode' parameter passed to `vu_open'.
 *
 *	`server_fd' is the descriptor number of the server socket.
 *
 *	`connection_fd' is the descriptor number of the client connection.
 *
 *	`client_addr' and `client_addr_len' is the address from `accept'.
 *
 *	`closure' is the `closure' argument passed to
 *	`url_socket_push_socket_handler'.
 *
 *  typedef void (*url_socket_server_close_callback) 
 *		(int server_fd,
 *		 struct sockaddr * server_addr,
 *		 int server_addr_len,
 *		 void * closure);
 *
 *	`server_fd' is the descriptor number of the server socket.
 *
 *	`server_addr' and `server_addr_len' is the address passed to
 *	`bind'.
 *
 *	`closure' is the `closure' argument passed to
 *	`url_socket_push_socket_handler'.
 *
 */
void
url_socket_push_server_handler (enum url_socket_domains domains,
				int server_flags,
				int may_be_client,
				int only_server_url,
				int default_port,
				int backlog,
				url_socket_server_callback server_callback,
				url_socket_connect_callback connection_callback,
				url_socket_server_close_callback server_close_callback,
				void * closure,
				int is_optional)
{
  regex_t * regex;
  struct url_socket_params params;
  t_uchar * name;
  t_uchar ** doc;

  if (only_server_url)
    regex = url_socket_only_server_regex (&name, &doc, domains, default_port);
  else
    regex = url_socket_server_regex (&name, &doc, domains, may_be_client, default_port);

  if (may_be_client)
    params.type = url_socket_server_or_client;
  else
    params.type = url_socket_server;

  params.domains = domains;
  params.server_flags = server_flags;
  params.default_port = default_port;
  params.backlog = backlog;
  params.server_callback = server_callback;
  params.connection_callback = connection_callback;
  params.server_close_callback = server_close_callback;
  params.closure = closure;
  vu_push_name_handler (name, doc, regex, 0, &url_socket_vtable, (void *)&params, is_optional);
}



/****************************************************************
 * Caching Server Sockets
 *
 * The system creates only one server socket per address.
 */

#ifndef SOCK_MAXADDRLEN
#undef MAX
#define MAX(A,B)  (((A)>=(B))?(A):(B))
#define SOCK_MAXADDRLEN	MAX(sizeof(struct sockaddr_un), sizeof (struct sockaddr_in))
#endif


struct server_memo
{
  char addr_data[SOCK_MAXADDRLEN]; 	/* The address of the server */
  int fd;				/* The descriptor of the server */
  int flags;				/* O_NONBLOCK */
  char * path;				/* If this is a unix domain socket, the file name to unlink */
};

static struct server_memo * addr_memo = 0;

#define IN_ADDR(A)	((struct sockaddr_in *)A)
#define IN_ADDR(A)	((struct sockaddr_in *)A)
#define UN_ADDR(A)	((struct sockaddr_un *)A)
#define SOCKADDR(A)	((struct sockaddr *)(A))
#define ADDR_MEMO(N)	SOCKADDR(addr_memo[(N)].addr_data)
#define IN_ADDR_MEMO(N)	IN_ADDR(addr_memo[(N)].addr_data)
#define UN_ADDR_MEMO(N)	UN_ADDR(addr_memo[(N)].addr_data)

/* static int server_fd_for_addr (int * errn,
 *		    		  struct sockaddr * addr,
 *		    		  int addr_len,
 *				  char * path,
 *		    		  int flags,
 *		    		  int mode,
 *		    		  struct url_socket_params * params);
 *
 * Return a server socket for a given address or -1.
 *
 * `errn' returns an errno code.
 * 
 * `addr' and `addr_len' is the address of the server (for `bind(2)').
 *
 * `path' is the `path' parameter for the server callback in `params'.
 *
 * `flags' is the `flags' parameter for the server callback in `params'.
 *
 * `mode' is the `mode' parameter for the server callback in `params'.
 *
 * `params' is the VU closure containing the server callback and other
 * parameters.
 */
static int
server_fd_for_addr (int * errn,
		    struct sockaddr * addr,
		    int addr_len,
		    char * path,
		    int flags,
		    int mode,
		    struct url_socket_params * params)
{
  int fd;
  int size;
  int x;
  int desired_flags;
  struct server_memo * memo;

  size = ar_size ((void *)addr_memo, lim_use_must_malloc, sizeof (*addr_memo));
  for (x = 0; x < size; ++x)
    {
      if (addr->sa_family == ADDR_MEMO(x)->sa_family)
	{
	  switch (addr->sa_family)
	    {
	    default:
	      panic ("corrupt address memo in url-socket.c");
	    case AF_INET:
	      if (   (IN_ADDR (addr)->sin_port == IN_ADDR_MEMO (x)->sin_port)
		  && !mem_cmp ((t_uchar *)&IN_ADDR (addr)->sin_addr, (t_uchar *)&IN_ADDR_MEMO (x)->sin_addr, sizeof (struct in_addr)))
		{
		  memo = &addr_memo [x];
		  fd = addr_memo[x].fd;
		  goto got_fd;
		}
	      break;
	    case AF_UNIX:
	      if (!str_cmp (UN_ADDR (addr)->sun_path, UN_ADDR_MEMO (x)->sun_path))
		{
		  memo = &addr_memo [x];
		  fd = addr_memo[x].fd;
		  goto got_fd;
		}
	      break;
	    }
	}
    }

  fd = socket (addr->sa_family, SOCK_STREAM, 0);
  if (fd < 0)
    {
      *errn = errno;
      return -1;
    }


  {
    int ign;

    if (0 > setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, (char *)&ign, sizeof (ign)))
      {
	*errn = errno;
	close (fd);
	return -1;
      }
  }

  if (0 > bind (fd, addr, addr_len))
    {
      *errn = errno;
      close (fd);
      return -1;
    }

  if (0 > listen (fd, params->backlog))
    {
      *errn = errno;
      close (fd);
      return -1;
    }

  {
    int orig_flags;
    orig_flags = fcntl (fd, F_GETFL, 0);
    if (orig_flags < 0)
      {
	*errn = errno;
	close (fd);
	return -1;
      }
    memo = (struct server_memo *)ar_push ((void **)&addr_memo, lim_use_must_malloc, sizeof (struct server_memo));
    mem_move (memo->addr_data, (t_uchar *)addr, addr_len);
    memo->fd = fd;
    memo->flags = orig_flags & O_NONBLOCK;
    if (addr->sa_family == AF_INET)
      memo->path = 0;
    else
      memo->path = str_save (lim_use_must_malloc, UN_ADDR (addr)->sun_path);
  }
  
  vu_set_fd_handler (fd, &url_server_socket_vtable, (void *)params);

  if (params->server_callback)
    params->server_callback (path, params->server_flags, flags, mode, fd, addr, addr_len, params->closure);

 got_fd:
  desired_flags = (params->server_flags & O_NONBLOCK);
  if (desired_flags != memo->flags)
    {
      if (-1 == fcntl (fd, F_SETFL, desired_flags))
	{
	  *errn = errno;
	  return -1;
	}
      memo->flags = desired_flags;
    }
  return fd;
}


/* static void remove_memo_for_server_fd (int fd);
 *
 * Remove the cache of data about server socket `fd'.
 */
static struct server_memo *
find_memo_for_server_fd (int fd)
{
  int size;
  int x;

  size = ar_size ((void *)addr_memo, lim_use_must_malloc, sizeof (*addr_memo));
  for (x = 0; x < size; ++x)
    {
      if (addr_memo[x].fd == fd)
	return &addr_memo[x];
    }
  while (1)
    panic ("url-socket.c: trying to find a server fd memo for unknown server");
}


/* static void remove_memo_for_server_fd (int fd);
 *
 * Remove the cache of data about server socket `fd'.
 */
static void
remove_memo_for_server_fd (int fd)
{
  int size;
  int x;

  size = ar_size ((void *)addr_memo, lim_use_must_malloc, sizeof (*addr_memo));
  for (x = 0; x < size; ++x)
    {
      if (addr_memo[x].fd == fd)
	{
	  addr_memo[x] = addr_memo[size - 1];
	  ar_pop ((void **)&addr_memo, lim_use_must_malloc, sizeof (struct server_memo));
	  return;
	}
    }
  panic ("url-socket.c: trying to remove server fd memo for unknown server");
}





/* static int decode_socket_url (int * errn,
 *		   		 struct sockaddr * addr,
 *		   		 int * addr_len,
 *		   		 int * be_a_server,
 *				 char * path,
 *		   		 struct url_socket_params * params,
 *		   		 int be_a_server_hint);
 *
 * Translate a URL to a socket address.  Return 0 on success,
 * -1 in the event of an error.
 *
 * `errn' returns an errno code.
 *
 * `addr' and `addr_len' return the translated address.
 *
 * `be_a_server' returns 0 if a client socket was specified,
 * 1 if a server socket was specified.  The `path' specifies a 
 * server if the URL scheme is "unix-server" or "inet-server",
 * or if the `type' field of `params' is `url_socket_server'.
 * It is an error (EINVAL) if the  `type' field of `params' is 
 * `url_socket_client', but the URL scheme is "unix-server" or 
 * "inet-server"  (Other kinds of errors may also return EINVAL.)
 *
 * `path' is the URL to translate.
 *
 * `params' is the URL params associated with `path'.  For example, this
 * is the `closure' parameter passed to `url_socket_open'.
 *
 * `be_a_server_hint' applies only if the `type' field of `params'
 * is `url_socket_server_or_client'.  
 *
 */
static int
decode_socket_url (int * errn,
		   struct sockaddr * addr,
		   int * addr_len,
		   struct url_socket_params * params,
		   int * be_a_server,
		   char * path,
		   int be_a_server_hint)
{
  int is_server_address;

  is_server_address = 0;
  if (!str_cmp_prefix ("inet://", path) || (is_server_address = !str_cmp_prefix ("inet-server://", path)))
    {
      char * addr_spec;
      char * end_of_addr_spec;
      char * host;

      /* Sanity check the requested socket type against the
       * parameters for this handler.
       *
       * Perhaps this should actually panic.
       */
      if (   (params->domains != url_socket_inet)
	  && (params->domains != url_socket_inet_or_unix))
	{
	bogus_addr:
	  *errn = EINVAL;
	  return -1;
	}

      IN_ADDR(addr)->sin_family = AF_INET;


      /* Find the extent of the host part of the path.
       * Find the port number, if any was specified.
       */
      addr_spec = str_chr_index (path, '/');
      addr_spec += 2;
      end_of_addr_spec = str_chr_index (addr_spec, ':');

      if (!end_of_addr_spec)
	{
	  /* No port specified in path.  Use the default.
	   */
	  end_of_addr_spec = str_chr_index (addr_spec, 0);
	  if (params->default_port < 0)
	    goto bogus_addr;
	  IN_ADDR(addr)->sin_port = htons (params->default_port);
	}
      else
	{
	  char * port_name;
	  unsigned long port;
	  int errn;

	  /* Port number explicitly specified.
	   */
	  port_name = end_of_addr_spec + 1;
	  if (0 > cvt_decimal_to_ulong (&errn, &port, port_name, str_length (port_name)))
	    return -1;

	  IN_ADDR(addr)->sin_port = htons (port);
	}

      /* Make a copy of the hostname, so we can have a 0-terminated
       * form.
       */
      host = alloca (1 + end_of_addr_spec - addr_spec);
      str_cpy_n (host, addr_spec, end_of_addr_spec - addr_spec);
      host[end_of_addr_spec - addr_spec] = 0;

      /* Does it appear to be an IP address?
       */
      if (char_is_digit (*host))
	{
	  
	  IN_ADDR(addr)->sin_addr.s_addr = inet_addr (host);
	  if (IN_ADDR(addr)->sin_addr.s_addr == (t_ulong)-1)
	    goto bogus_addr;
	}
#if 0
      /* INADDR_LOOPBACK is not defined in some versions of BSD/OS. */
      else if (!str_cmp ("INADDR_LOOPBACK", host))
	{
	  IN_ADDR(addr)->sin_addr.s_addr = INADDR_LOOPBACK;
	}
#endif
      else if (!str_cmp ("INADDR_ANY", host))
	{
	  IN_ADDR(addr)->sin_addr.s_addr = INADDR_ANY;
	}
      else
	{
	  struct hostent * hostentp;
	  hostentp = gethostbyname (host);
	  if (!hostentp || (hostentp->h_addrtype != AF_INET))
	    goto bogus_addr;
	  mem_move ((t_uchar *)&IN_ADDR(addr)->sin_addr, hostentp->h_addr, hostentp->h_length);
	}
      *addr_len = sizeof (struct sockaddr_in);
    }
  else if (!str_cmp_prefix ("unix:", path) || (is_server_address = !str_cmp_prefix ("unix-server:", path)))
    {
      char * socket_path;
      char * expanded_socket_path;
      int len;

      /* Sanity check the requested socket type against the
       * parameters for this handler.
       */
      if (   (params->domains != url_socket_unix)
	  && (params->domains != url_socket_inet_or_unix))
	goto bogus_addr;

      UN_ADDR(addr)->sun_family = AF_UNIX;
      socket_path = 1 + str_chr_index (path, ':');
      expanded_socket_path = file_name_tilde_expand (lim_use_must_malloc, socket_path);
      len = str_length (expanded_socket_path);
      if (len + 1 > sizeof (UN_ADDR(addr)->sun_path))
	{
	  *errn = ENAMETOOLONG;
	  return -1;
	}
      str_cpy (UN_ADDR(addr)->sun_path, expanded_socket_path);
      *addr_len = sizeof (struct sockaddr_un);
    }

  if (is_server_address && (params->type == url_socket_client))
    goto bogus_addr;

  *be_a_server = (   is_server_address
		  || (params->type == url_socket_server)
		  || be_a_server_hint);

  if (   (addr->sa_family == AF_INET)
      && !*be_a_server
      && (IN_ADDR(addr)->sin_addr.s_addr == INADDR_ANY))
    {
      *errn = EINVAL;
      return -1;
    }

  return 0;
}




/*c int url_socket_create_server_socket (int * errn, char * path)
 *
 * Create a server socket for `path'.  Return a descriptor number
 * or -1.
 *
 * `errno' returns an errno code.
 *
 * `path' should be a URL matching a pattern added to the VU file
 * namespace.
 *
 * `vu_close' can be used to close a server fd.
 */
int
url_socket_create_server_socket (int * errn, char * path)
{
  struct vu_handler * handler;
  struct url_socket_params * params;
  char addr_space[SOCK_MAXADDRLEN];
  struct sockaddr * addr;
  int addr_len;
  int be_a_server;

  handler = vu_path_dispatch (path);
  params = (struct url_socket_params *)handler->closure;
  addr = (struct sockaddr *)addr_space;

  if (0 > decode_socket_url (errn, addr, &addr_len, params, &be_a_server, path, 0))
    return -1;

  if (!be_a_server)
    {
      *errn = EINVAL;
      return -1;
    }
  else
    {
      int server_fd;

      server_fd = server_fd_for_addr (errn, addr, addr_len, path, 0, 0, params);
      return server_fd;
    }
}



int
url_inet_client (int * errn, t_uchar * host, int port)
{
  t_ulong host_addr;

  if (char_is_digit (*host))
    {
      struct in_addr a;
      
      a.s_addr = inet_addr (host);
      if (a.s_addr == (t_ulong) -1)
	{
	bogus_addr:
	  *errn = EINVAL;
	  return -1;
	}
      host_addr = ntohl (a.s_addr);
    }
  else
    {
      struct hostent * hostentp;

      hostentp = gethostbyname (host);
      if (!hostentp || (hostentp->h_addrtype != AF_INET))
	goto bogus_addr;
      mem_move ((t_uchar *)&host_addr, hostentp->h_addr, hostentp->h_length);

      host_addr = ntohl (host_addr);
    }

  return url_inet_client_addr (errn, host_addr, port);
}



int
url_inet_client_addr (int * errn, t_ulong host, int port)
{
  int fd;
  struct url_socket_params params;
  struct sockaddr_in addr;

  addr.sin_family = AF_INET;
  addr.sin_port = htons (port);
  addr.sin_addr.s_addr = htonl (host);
  
  fd = socket (AF_INET, SOCK_STREAM, 0);
  if (fd < 0)
    {
      *errn = errno;
      return -1;
    }
  if (0 > connect (fd, (struct sockaddr *)&addr, sizeof (addr)))
    {
      *errn = errno;
      close (fd);
      return -1;
    }

  params.type = url_socket_client;
  params.domains = url_socket_inet;
  params.default_port = 0;
  params.backlog = 5;
  params.server_callback = 0;
  params.connection_callback = 0;
  params.server_close_callback = 0;
  vu_set_fd_handler (fd, &url_socket_vtable, (void *)&params);

  return fd;
}



int
url_inet_server_accept (int * errn, int server_fd)
{
  struct url_socket_params params;
  char client_addr_space[SOCK_MAXADDRLEN];
  struct sockaddr * client_addr;
  int client_addr_len;
  int connection_fd;

  client_addr = (struct sockaddr *)client_addr_space;
  client_addr_len = sizeof (client_addr_space);

  connection_fd = accept (server_fd, client_addr, &client_addr_len);
  if (connection_fd < 0)
    {
      *errn = errno;
      return -1;
    }

  params.type = url_socket_server;
  params.domains = url_socket_inet;
  params.default_port = 0;
  params.backlog = 0;
  params.server_callback = 0;
  params.connection_callback = 0;
  params.server_close_callback = 0;
  vu_set_fd_handler (connection_fd, &url_socket_vtable, (void *)&params);

  return connection_fd;
}


int
url_inet_server (alloc_limits limits,
		 t_ulong * host_addr_is,
		 t_uchar ** host_id_is,
		 int * port_is,
		 int * errn,
		 t_uchar * host,
		 int port)
{
  int fd;
  struct url_socket_params params;
  struct sockaddr_in addr;
  t_uchar myhost[MAXHOSTNAMELEN + 1];

  mem_set0 ((t_uchar *)&addr, sizeof (addr));

  addr.sin_family = AF_INET;
  addr.sin_port = htons (port);

  if (!host)
    {
      if (host_id_is)
	{
	  if (0 > gethostname (myhost, sizeof (myhost)))
	    {
	      *errn = errno;
	      return -1;
	    }
	  *host_id_is = str_save (limits, myhost);
	}
      addr.sin_addr.s_addr = htonl (INADDR_ANY);
    }
  else
    {
      if (host_id_is)
	*host_id_is = str_save (limits, host);

      if (char_is_digit (*host))
	{
	  addr.sin_addr.s_addr = inet_addr (host);
	  if (addr.sin_addr.s_addr == (t_ulong)-1)
	    goto bogus_addr;
	  
	}
      else if (!str_cmp ("INADDR_ANY", host))
	{
	  addr.sin_addr.s_addr = htonl (INADDR_ANY);
	}
      else
	{
	  struct hostent * hostentp;

	  hostentp = gethostbyname (host);
	  if (!hostentp || (hostentp->h_addrtype != AF_INET))
	    {
	    bogus_addr:
	      *errn = EINVAL;
	      return -1;
	    }
	  mem_move ((t_uchar *)&addr.sin_addr, hostentp->h_addr, hostentp->h_length);
	}
    }

  mem_set0 ((t_uchar *)&params, sizeof (params));
  params.type = url_socket_server;
  params.domains = url_socket_inet;
  params.default_port = 0;
  params.backlog = 1;
  params.server_callback = 0;
  params.connection_callback = 0;
  params.server_close_callback = 0;

  fd = server_fd_for_addr (errn, (struct sockaddr *)&addr, sizeof (addr), 0, O_RDWR, 0, &params);

  if (fd < 0)
    {
      return -1;
    }
  else
    {
      if (host_addr_is)
	{
	  if (ntohl (addr.sin_addr.s_addr) != INADDR_ANY)
	    {
	      *host_addr_is = ntohl (addr.sin_addr.s_addr);
	    }
	  else
	    {
	      char my_name[MAXHOSTNAMELEN + 1];
	      struct hostent * hostent;
	  
	      if (0 > gethostname (my_name, sizeof (my_name)))
		{
		  int ign;
		  *errn = errno;
		  vu_close (&ign, fd);
		  return -1;
		}

	      hostent = gethostbyname (my_name);
	      if (!hostent)
		{
		  int ign;
		  *errn = errno;
		  vu_close (&ign, fd);
		  return -1;
		}
	      {
		t_uint32 tmp;

		mem_move ((t_uchar *)&tmp, (t_uchar *)hostent->h_addr, 4);
		*host_addr_is = ntohl (tmp);
	      }
	    }
	}

      if (port_is)
	{
	  char name_space[SOCK_MAXADDRLEN];
	  int name_len;
	  struct sockaddr * name;
	  
	  name = (struct sockaddr *)name_space;
	  name_len = sizeof (name_space);

	  if (0 > getsockname (fd, name, &name_len))
	    {
	      int e;
	      *errn = errno;
	      vu_close (&e, fd);
	      return -1;
	    }
	  else
	    {
	      if (port_is)
		{
		  *port_is = ntohs (IN_ADDR(name)->sin_port);
		}
	    }
	}
    }

  return fd;
}




/****************************************************************
 * VU `open' and `close' for Sockets
 *
 *
 */

/*c void * url_socket_make_closure (void * closure);
 *
 * A `make_closure' function for a VU vtable.  (See `vu_push_name_handler'.)
 */
static void *
url_socket_make_closure (void * closure)
{
  struct url_socket_params * answer;
  answer = (struct url_socket_params *)must_malloc (sizeof (*answer));
  *answer = *(struct url_socket_params *)closure;
  return (void *)answer;
}


/*c void url_socket_free_closure (void * closure);
 *
 * A `free_closure' function for a VU vtable.  (See `vu_push_name_handler'.)
 */
static void
url_socket_free_closure (void * closure)
{
  must_free (closure);
}


/*c void url_socket_open (int * errn,
 * 			  char * path,
 *			  int flags,
 *			  int mode,
 *			  void * closure);
 *
 * An `open' function for a VU vtable.  (See `vu_push_name_handler'.)
 * 
 * See `url_socket_push_client_handler' and `url_socket_push_server_handler'.
 */
static int
url_socket_open (int * errn, char * path, int flags, int mode, void * closure)
{
  struct url_socket_params * params;
  char addr_space[SOCK_MAXADDRLEN];
  struct sockaddr * addr;
  int addr_len;
  int be_a_server;

  params = (struct url_socket_params *)closure;
  addr = (struct sockaddr *)addr_space;

  if (0 > decode_socket_url (errn, addr, &addr_len, params, &be_a_server, path, 0))
    return -1;

  if (!be_a_server)
    {
      int fd;

      fd = socket (addr->sa_family, SOCK_STREAM, 0);
      if (fd < 0)
	{
	  *errn = errno;
	  return -1;
	}
      if (0 > connect (fd, addr, addr_len))
	{
	  *errn = errno;
	  close (fd);
	  return -1;
	}
      return fd;
    }
  else
    {
      char client_addr_space[SOCK_MAXADDRLEN];
      struct sockaddr * client_addr;
      int client_addr_len;
      int server_fd;
      int connection_fd;

      client_addr = (struct sockaddr *)client_addr_space;
      client_addr_len = sizeof (client_addr_space);
      server_fd = server_fd_for_addr (errn, addr, addr_len, path, flags, mode, params);
      if (server_fd < 0)
	return -1;

      connection_fd = accept (server_fd, client_addr, &client_addr_len);
      if (connection_fd < 0)
	{
	  *errn = errno;
	  return -1;
	}

      if (params->connection_callback)
	params->connection_callback (path, flags, mode, server_fd, connection_fd, client_addr, client_addr_len, params->closure);

      return connection_fd;
    }
}



/*c void url_socket_close (int * errn, int fd, void * closure);
 * 			   char * path,
 *			   int flags,
 *			   int mode,
 *			   void * closure);
 *
 * A `close' function for a VU vtable.  (See `vu_push_name_handler'.)
 * 
 * See `url_socket_push_client_handler' and `url_socket_push_server_handler'.
 */
static int
url_socket_close (int * errn, int fd, void * closure)
{
  int x;
  int size;

  size = ar_size ((void *)addr_memo, lim_use_must_malloc, sizeof (*addr_memo));
  for (x = 0; x < size; ++x)
    if (fd == addr_memo [x].fd)
      {
	if (0 > vu_sys_close (errn, fd, 0))
	  return -1;
	addr_memo[x] = addr_memo[size - 1];
	ar_pop ((void **)&addr_memo, lim_use_must_malloc, sizeof (struct server_memo));
	return 0;
      }
  return vu_sys_close (errn, fd, 0);
}



/****************************************************************
 * VU Vtable Functions for URL Sockets
 */

static int
url_socket_access (int * errn, char * path, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_chdir (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_chmod (int * errn, char * path, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_chown (int * errn, char * path, int owner, int group, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_chroot (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_closedir (int * errn, DIR * dir, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_fchdir (int * errn, int fd, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_fchmod (int * errn, int fd, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_fchown (int * errn, int fd, int owner, int group, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_fstat (int * errn, int fd, struct stat * buf, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_fsync (int * errn, int fd, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_ftruncate (int * errn, int fd, off_t where, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_link (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static off_t
url_socket_lseek (int * errn, int fd, off_t offset, int whence, void * closure)
{
  *errn = ESPIPE;
  return -1;
}


static int
url_socket_lstat (int * errn, char * path, struct stat * buf, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_mkdir (int * errn, char * path, int mode, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_opendir (int * errn, DIR ** retv,  char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static ssize_t
url_socket_read (int * errn, int fd, char * buf, size_t count, void * closure)
{
  return vu_sys_read (errn, fd, buf, count, 0);
}


static int
url_socket_readdir (int * errn, struct alloc_limits * limits, char ** file_ret, DIR * dir, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_readlink (int * errn, char * path, char * buf, int bufsize, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_rename (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_rmdir (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_stat (int * errn, char * path, struct stat * buf, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_symlink (int * errn, char * from, char * to, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_truncate (int * errn, char * path, off_t where, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_unlink (int * errn, char * path, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static int
url_socket_utime (int * errn, char * path, struct utimbuf * times, void * closure)
{
  *errn = EINVAL;
  return -1;
}


static ssize_t
url_socket_write (int * errn, int fd, char * buf, size_t count, void * closure)
{
  return vu_sys_write (errn, fd, buf, count, 0);
}

static int
url_socket_fcntl (int * errn, int fd, int cmd, long arg, void * closure)
{
  return vu_sys_fcntl (errn, fd, cmd, arg, 0);
}


static int
url_socket_dup (int * errn, int fd, void * closure)
{
  return vu_sys_dup (errn, fd, 0);
}


static int
url_socket_dup2 (int * errn, int fd, int newfd, void * closure)
{
  return vu_sys_dup2 (errn, fd, newfd, 0);
}



static int
url_socket_move_state (int * errn, int fd, int newfd, void * closure)
{
  if (fd == newfd)
    return fd;
  vu_set_fd_handler (newfd, &url_socket_vtable, closure);
  return 0;
}


static struct vu_fs_discipline url_socket_vtable = { VU_FS_DISCIPLINE_INITIALIZERS (url_socket_) };


/****************************************************************
 * VU Vtable Functions for Server Sockets
 */


/*c int url_server_socket_close (int * errn, int fd, void * closure);
 *
 * A `vu_close_fn' implementation.
 *
 * Close a descriptor which is a server-socket created by 
 * `url_socket_open' or `url_socket_create_server_socket'.
 *
 */
static int
url_server_socket_close (int * errn, int fd, void * closure)
{
  struct url_socket_params * params;
  struct server_memo * memo;
  struct sockaddr * addr;
  int addr_len;
  char addr_space[SOCK_MAXADDRLEN];

  params = (struct url_socket_params *)closure;
  memo = find_memo_for_server_fd (fd);
  if (close (fd) < 0)
    {
      *errn = errno;
      return -1;
    }
  if (memo->path)
    {
      int errn2;
      vu_unlink (&errn2, memo->path);
    }
  addr = (struct sockaddr *)memo->addr_data;
  addr_len = (addr->sa_family ? sizeof (struct sockaddr_in) : sizeof (struct sockaddr_un));
  mem_move (addr_space, (t_uchar *)addr, addr_len);
  addr = (struct sockaddr *)addr_space;
  remove_memo_for_server_fd (fd);
  if (params->server_close_callback)
    params->server_close_callback (fd, addr, addr_len, params->closure);
  return 0;
}

#define url_server_socket_make_closure	url_socket_make_closure
#define url_server_socket_free_closure	url_socket_free_closure

#define url_server_socket_access	(vu_access_fn)vu_bad_arg
#define url_server_socket_chdir		(vu_chdir_fn)vu_bad_arg
#define url_server_socket_chmod		(vu_chmod_fn)vu_bad_arg
#define url_server_socket_chown		(vu_chown_fn)vu_bad_arg
#define url_server_socket_chroot	(vu_chroot_fn)vu_bad_arg
#define url_server_socket_closedir	(vu_closedir_fn)vu_bad_arg
#define url_server_socket_fchdir	(vu_fchdir_fn)vu_bad_arg
#define url_server_socket_fchmod	(vu_fchmod_fn)vu_bad_arg
#define url_server_socket_fchown	(vu_fchown_fn)vu_bad_arg
#define url_server_socket_fstat		(vu_fstat_fn)vu_bad_arg
#define url_server_socket_fsync		(vu_fsync_fn)vu_bad_arg
#define url_server_socket_ftruncate	(vu_ftruncate_fn)vu_bad_arg
#define url_server_socket_link		(vu_link_fn)vu_bad_arg
#define url_server_socket_lseek		(vu_lseek_fn)vu_bad_arg
#define url_server_socket_lstat		(vu_lstat_fn)vu_bad_arg
#define url_server_socket_mkdir		(vu_mkdir_fn)vu_bad_arg
#define url_server_socket_open		(vu_open_fn)vu_bad_arg
#define url_server_socket_opendir	(vu_opendir_fn)vu_bad_arg
#define url_server_socket_read		(vu_read_fn)vu_bad_arg
#define url_server_socket_readdir	(vu_readdir_fn)vu_bad_arg
#define url_server_socket_readlink	(vu_readlink_fn)vu_bad_arg
#define url_server_socket_rename	(vu_rename_fn)vu_bad_arg
#define url_server_socket_rmdir		(vu_rmdir_fn)vu_bad_arg
#define url_server_socket_stat		(vu_stat_fn)vu_bad_arg
#define url_server_socket_symlink	(vu_symlink_fn)vu_bad_arg
#define url_server_socket_truncate	(vu_truncate_fn)vu_bad_arg
#define url_server_socket_unlink	(vu_unlink_fn)vu_bad_arg
#define url_server_socket_utime		(vu_utime_fn)vu_bad_arg
#define url_server_socket_write		(vu_write_fn)vu_bad_arg
#define url_server_socket_fcntl		(vu_fcntl_fn)vu_bad_arg
#define url_server_socket_dup		(vu_dup_fn)vu_bad_arg
#define url_server_socket_dup2		(vu_dup2_fn)vu_bad_arg
#define url_server_socket_move_state	(vu_dup2_fn)vu_bad_arg

static struct vu_fs_discipline url_server_socket_vtable = { VU_FS_DISCIPLINE_INITIALIZERS (url_server_socket_) };

