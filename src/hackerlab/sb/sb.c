/* sb.c
 *
 ****************************************************************
 * Copyright (C) 1999, 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "config-options.h"
#include "hackerlab/os/errno-to-string.h"
#include "hackerlab/vu/vu-help.h"
#include "hackerlab/vu/vu-dash.h"
#include "hackerlab/vu/url-fd.h"
#include "hackerlab/vu-network/url-socket.h"
#include "hackerlab/cmd/main.h"




static t_uchar * program_name = "sb";
static t_uchar * usage = "[options] [input-file [output-file]]";
static t_uchar * version_string = (cfg__std__package " from regexps.com\n"
				   "\n"
				   "Copyright 2001, 2002 Tom Lord\n"
				   "\n"
				   "This is free software; see the source for copying conditions.\n"
				   "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A\n"
				   "PARTICULAR PURPOSE.\n"
				   "\n"
				   "Report bugs to <lord@regexps.com>.\n"
				   "\n");



#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, "Display a help message and exit.") \
  OP (opt_help_namespaces, 0, "help-namespaces", 0, "Display help for --namespace and exit.") \
  OP (opt_version, "V", "version", 0, "Display a release identifier string and exit.") \
  OP (opt_append, "a", "append", 0, "Open the output file for append.") \
  OP (opt_namespace, "N type", "namespace category", 1, "Extend the file namespace.") \
  OP2 (opt_namespace, 0, 0, 0, "This option may be used more than once.") \
  OP2 (opt_namespace, 0, 0, 0, "See also --help-namespaces.")


enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



int
main (int argc, char * argv[])
{
  int errn;
  int in_fd;
  int out_fd;
  int append;
  int o;
  struct opt_parsed * option =  0;

  if (   vfdbuf_buffer_fd (&errn, 0, 0, O_RDONLY, 0)
      || vfdbuf_buffer_fd (&errn, 1, 0, O_WRONLY, 0)
      || vfdbuf_buffer_fd (&errn, 2, 0, O_WRONLY, 0))
    {
      printfmt (&errn, 2, "error establishing buffers on standard descriptors (%s).", errno_to_string (errn));
      exit (1);
    }

  url_socket_push_server_handler (url_socket_inet_or_unix, 0, 0, 1, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_inet, 0, 0, 1, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_unix, 0, 0, 1, -1, 1, 0, 0, 0, 0, 1);

  url_socket_push_server_handler (url_socket_inet_or_unix, 0, 0, 0, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_inet, 0, 0, 0, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_unix, 0, 0, 0, -1, 1, 0, 0, 0, 0, 1);

  url_socket_push_server_handler (url_socket_unix, 0, 1, 0, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_inet, 0, 1, 0, -1, 1, 0, 0, 0, 0, 1);
  url_socket_push_server_handler (url_socket_inet_or_unix, 0, 1, 0, -1, 1, 0, 0, 0, 0, 1);

  url_socket_push_client_handler (url_socket_inet_or_unix, -1, 1);
  url_socket_push_client_handler (url_socket_inet, -1, 1);
  url_socket_push_client_handler (url_socket_unix, -1, 1);

  url_fd_push_handler (1);
  vu_push_dash_handler (0);

  append = 0;

  option = 0;

  while (1)
    {
      o = opt_standard (lim_use_must_malloc, &option, opts, &argc, argv, program_name, usage, version_string, opt_help_msg, opt_version);
      if (o == opt_none)
	break;
      switch (o)
	{
	default:
	  safe_printfmt (2, "unhandled option `%s'\n", option->opt_string);
	  panic ("internal error parsing arguments");

	usage_error:
	  opt_usage (2, argv[0], program_name, usage, 1);
	  panic_exit ();

	bogus_arg:
	  safe_printfmt (2, "ill-formed argument for `%s' (`%s')\n", option->opt_string, option->arg_string);
	  goto usage_error;

	case opt_help_namespaces:
	  printfmt (&errn, 1, "\n");
	  printfmt (&errn, 1, "--namespace=category adds support for a category of special file names.\n");
	  printfmt (&errn, 1, "Special file names are URLs with the general syntax $SCHEME:$PARAMETERS\n");
	  printfmt (&errn, 1, "\n\n");
	  vu_help_for_optional_handlers ("--namespace", 1);
	  exit (0);
	  
	case opt_append:
	  append = 1;
	  break;

	case opt_namespace:
	  if (0 > vu_enable_optional_name_handler (option->arg_string))
	    goto bogus_arg;
	  break;
	}
    }

  if (argc < 2)
    in_fd = 0;
  else
    {
      in_fd = vu_open (&errn, argv[1], O_RDONLY, 0);
      if (in_fd < 0)
	{
	  printfmt (&errn, 2, "unable to open `%s' for input (\"%s\")\n", argv[1], errno_to_string (errn));
	  exit (1);
	}
      opt_shift (&argc, argv);
    }

  if (argc > 3)
    goto usage_error;

  if (argc < 2)
    out_fd = 1;
  else
    {
      out_fd = vu_open (&errn, argv[1], O_WRONLY | O_CREAT | (append ? O_APPEND : O_EXCL), 0666);
      if (out_fd < 0)
	{
	  printfmt (&errn, 2, "unable to open `%s' for output (\"%s\")\n", argv[1], errno_to_string (errn));
	  exit (1);
	}
      opt_shift (&argc, argv);
    }

  {
    int page_size;
    int buf_size;
    char * buf;
    int read_size;
    int write_size;

    page_size = getpagesize ();
    buf_size = page_size * 10;
    buf = must_malloc (buf_size);
    while (1)
      {
	read_size = vu_read (&errn, in_fd, buf, buf_size);
	if (read_size < 0)
	  {
	    printfmt (&errn, 2, "i/o error reading input (%s)\n", errno_to_string (errn));
	    exit (1);
	  }
	if (read_size == 0)
	  exit (0);
	write_size = vu_write_retry (&errn, out_fd, buf, read_size);
	if (write_size < 0)
	  {
	    printfmt (&errn, 2, "i/o error writing output (%s)\n", errno_to_string (errn));
	    exit (1);
	  }
	if (write_size != read_size)
	  panic ("bizarre vu_write_retry error (wrong amount written)");
      }
  }
  return 0;
}

/* tag: suck and blow
 */
