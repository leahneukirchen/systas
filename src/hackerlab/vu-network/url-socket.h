/* url-socket.h - decls for the socket URLs file system
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__VU_NETWORK__URL_SOCKET_H
#define INCLUDE__VU_NETWORK__URL_SOCKET_H


#include "hackerlab/os/sys/socket.h"
#include "hackerlab/os/sys/param.h"
#include "hackerlab/os/sys/un.h"
#include "hackerlab/os/netinet/in.h"
#include "hackerlab/os/arpa/inet.h"
#include "hackerlab/os/netdb.h"
#include "hackerlab/vu/vu.h"


/* enum url_socket_type
 *
 * A single file name regexp can be used to name client sockets,
 * server sockets, or both.
 */
enum url_socket_type
{
  url_socket_client,
  url_socket_server,
  url_socket_server_or_client,
};


/* enum url_socket_domains
 *
 * A single file name regexp can be associated with
 * sockets in the inet domain, the unix domain, or both.
 */
enum url_socket_domains
{
  url_socket_unix,
  url_socket_inet,
  url_socket_inet_or_unix,
};


/* A client-supplied function that is called when a new server socket
 * is created.
 */
typedef void (*url_socket_server_callback) (char * path,
					    int server_flags,
					    int flags,
					    int mode,
					    int server_fd,
					    struct sockaddr * server_addr,
					    int server_addr_len,
					    void * closure);

/* A client-supplied function that is called when a new client
 * connection is received.
 */
typedef void (*url_socket_connect_callback) (char * path,
					     int flags,
					     int mode,
					     int server_fd,
					     int connection_fd,
					     struct sockaddr * client_addr,
					     int client_addr_len,
					     void * closure);


/* A client-supplied function that is called when a server socket is closed.
 */
typedef void (*url_socket_server_close_callback) (int server_fd,
						  struct sockaddr * server_addr,
						  int server_addr_len,
						  void * closure);


/* automatically generated __STDC__ prototypes */
extern void url_socket_push_client_handler (enum url_socket_domains domains,
					    int default_port,
					    int is_optional);
extern void url_socket_push_server_handler (enum url_socket_domains domains,
					    int server_flags,
					    int may_be_client,
					    int only_server_url,
					    int default_port,
					    int backlog,
					    url_socket_server_callback server_callback,
					    url_socket_connect_callback connection_callback,
					    url_socket_server_close_callback server_close_callback,
					    void * closure,
					    int is_optional);
extern int url_socket_create_server_socket (int * errn, char * path);
extern int url_inet_client (int * errn, t_uchar * host, int port);
extern int url_inet_client_addr (int * errn, t_ulong host, int port);
extern int url_inet_server_accept (int * errn, int server_fd);
extern int url_inet_server (alloc_limits limits,
			    t_ulong * host_addr_is,
			    t_uchar ** host_id_is,
			    int * port_is,
			    int * errn,
			    t_uchar * host,
			    int port);
#endif  /* INCLUDE__VU_NETWORK__URL_SOCKET_H */
