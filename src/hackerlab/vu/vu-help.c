/* tag: Tom Lord Tue Dec  4 14:41:39 2001 (vu-help.c)
 */
/* vu-help.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vu-help.h"




/* static int vu_help_with_handlers (t_uchar * long_option_name,
 *		       		     int fd,
 *		       		     struct _vu_namespace_handler * handlers)
 *
 * Print a help message describing namespace handlers in `handlers'.
 * Documentation for the handlers is printed in reverse order (treating
 * `handlers' as a stack and starting at the top of the stack).
 *
 * Output is in two columns.  The first column is the name of a category
 * of file names, the second column is documentation for that category.
 * `long_option_name', if not 0, is the title for the first column.
 *
 * `fd' is the descriptor on which to write output.
 */
static void
vu_help_with_handlers (t_uchar * long_option_name,
		       int fd,
		       struct _vu_namespace_handler * handlers)
{
  t_uchar * col1_head;
  t_uchar * col2_head;
  int col1_width;
  int col2_width;
  int x;
  int n_handlers;
  int errn;

  col1_head = (long_option_name
	       ? long_option_name
	       : (t_uchar *)"Category");
  col2_head = "File Names Supported";
  
  n_handlers = ar_size ((void *)handlers, lim_use_must_malloc, sizeof (*handlers));
  col1_width = str_length (col1_head);
  col2_width = str_length (col2_head);
  for (x = 0; x < n_handlers; ++x)
    {
      int this_width;
      int y;

      this_width = str_length (handlers[x].name);
      if (this_width > col1_width)
	col1_width = this_width;
      for (y = 0; handlers[x].doc[y]; ++y)
	{
	  this_width = str_length (handlers[x].doc[y]);
	  if (this_width > col2_width)
	    col2_width = this_width;
	}
    }

  if (0 > printfmt (&errn, fd, "  %-*s    %s\n\n", col1_width, col1_head, col2_head))
    {
    io_error:
      panic ("I/O error in vu_help_with_handlers");
    }    
  for (x = n_handlers - 1; x >= 0; --x)
    {
      int y;
      if (0 > printfmt (&errn, fd, "  %-*s    ", col1_width, handlers[x].name))
	goto io_error;
      if (0 > printfmt (&errn, fd, "%s\n", handlers[x].doc[0]))
	goto io_error;
      for (y = 1; handlers[x].doc[y]; ++y)
	{
	  if (0 > printfmt (&errn, fd, "  %*s    %s\n", col1_width, "", handlers[x].doc[y]))
	    goto io_error;
	}
      if (0 > printfmt (&errn, fd, "\n"))
	goto io_error;
    }
}


/*(c vu_help_for_optional_handlers)
 * int vu_help_for_optional_handlers (t_uchar * long_option_name, 
 *				      int fd);
 *
 * Print a help message describing the optional namespace handlers
 * that have been established by `vu_push_optional_name_handler'.
 * Documentation for the handlers is printed in the reverse order
 * of calls to `vu_push_optional_name_handler'.
 *
 * Output is in two columns.  The first column is the name of a category
 * of file names, the second column is documentation for that category.
 * `long_option_name', if not 0, is the title for the first column.
 *
 * `fd' is the descriptor on which to write output.
 */
void
vu_help_for_optional_handlers (t_uchar * long_option_name, int fd)
{
  vu_help_with_handlers (long_option_name, fd, _vu_optional_fs_handlers);
}


/*(c vu_help_for_enabled_handlers)
 * int vu_help_for_enabled_handlers (t_uchar * long_option_name,
 *				     int fd);
 *
 * Print a help message describing the enabled namespace handlers that
 * have been established by `vu_push_name_handler'.  Documentation for
 * the handlers is printed in the reverse order of calls to
 * `vu_push_name_handler' -- that is, in order of precedence from
 * highest to lowest.
 *
 * `fd' is the descriptor on which to write output.
 */
void
vu_help_for_enabled_handlers (t_uchar * long_option_name, int fd)
{
  vu_help_with_handlers (long_option_name, fd, _vu_fs_handlers);
}
