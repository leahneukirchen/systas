/* escape.c - exitting long-running matches
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/rx/escape.h"


/* For those reasons, Rx makes provisions for asynchronously aborting
 * a long-running match.  Matches by `regexec', `regnexec', the
 * functions in xref:"DFA String Comparisons" and the functions in
 * xref:"Regexp Matching" can be aborted.  When `regexec' or
 * `regnexec' is aborted, it returns `REG_MATCH_INTERRUPTED'.  Other
 * functions are aborted by calling `longjmp' to return to a point
 * outside the call to the function.
 */


/************************************************************************
 *(h1 "Escaping Long-Running Matches" 
 *    :includes ("hackerlab/rx/escape.h"))
 * 
 * |escaping long-running regexp matches|
 * |aborting long-running regexp matches|
 * |asynchronously aborting long-running regexp matches|
 * |$REG_MATCH_INTERRUPTED|
 * 
 * Regexp searches can take a long time.  Rx makes provisions for
 * asynchronously aborting a long-running match.  When `regexec' or
 * `regnexec' is aborted, it returns `REG_MATCH_INTERRUPTED'.  Callers
 * of other match functions (such as `rx_xml_is_match') can catch
 * asynchronous interrupts using the jump buffer `rx_escape_jmp_buf'
 * (documented below).
 * 
 * Asynchronous match interrupts are permitted whenever Rx calls the
 * function pointed to by `rx_poll' (see below).  If that pointer is
 * 0, no interrupts will occur.  If it points to a function, that
 * function may cause an interrupt by calling `longjmp' to reach 
 * the point from which the interrupt resumes.
 *
 * By convention, the global jump buffer `rx_escape_jmp_buf' is used.
 * To cause an interrupt the next time `rx_poll' is called, set
 * `rx_poll' to the function `rx_escape' which performs a `longjmp' to
 * `rx_escape_jmp_buf'.
 * 
 */

/*(c rx_poll :category variable)
 * extern void (*rx_poll)(void);
 * 
 * A function pointer that is called by Rx (if not 0) whenever it is
 * safe to interrupt an on-going search.
 */
void (*rx_poll)(void) = 0;


/*(c rx_escape_jmp_buf)
 * 
 * The conventionally used jump buffer for defining where to resume
 * execution after an Rx match function is interrupted.  
 *
 * See xref:"rx_escape".
 */
jmp_buf rx_escape_jmp_buf;


/*(c rx_escape)
 * void rx_escape (void);
 * 
 * This function is conventionally used for interrupting a
 * long-running Rx match function.  To cause an interrupt of an
 * on-going match from an asynchronously called function, such as a
 * signal handler, assign `rx_escape' to `rx_poll' and return normally
 * from the asynchronously called function.  When `rx_poll' is next
 * called, `rx_escape' will assign 0 to `rx_poll' and `longjmp' to
 * `rx_escape_jmp_buf'.  `rx_escape' is quite simple:
 * 
 insert*/
void
rx_escape (void)
{
  rx_poll = 0;
  longjmp (rx_escape_jmp_buf, 1);
}
/*end-insert
 * 
 * 
 * Here is how `rx_escape' might be used in conjunction with 
 * a signal handler while using `regexec':
 * 
 * 
 * 	void
 * 	match_fn (void)
 * 	{
 * 	   int status;
 * 	   ...;
 * 	   status = regexec (...);
 * 	   rx_poll = 0;			// prevent a race condition
 * 	   if (status == REG_MATCH_INTERRUPTED)
 * 	     {
 *		"matching was cut short";
 *	     }
 * 	}
 * 
 * 	void
 * 	signal_handler (int signal)
 * 	{
 *	  rx_poll = rx_escape;		// interrupt an ongoing match.
 *	}
 * 
 * 
 * Here is how the same signal handler might be used in conjunction with a
 * Unicode regular expression match function (such as `rx_xml_is_match'):
 * 
 * 
 * 	void
 * 	match_fn (void)
 * 	{
 * 	   int status;
 * 	   ...;
 * 	   if (setjmp (rx_escape_jmp_buf))
 *	     {
 *		"matching was cut short";
 *		return;
 *	     }
 * 	   status = xml_is_match (...);
 * 	   rx_poll = 0;			// prevent a race condition
 *	   ...
 * 	}
 *
 */
