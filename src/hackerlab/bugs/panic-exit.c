/* tag: Tom Lord Tue Dec  4 14:41:26 2001 (panic-exit.c)
 */
/* panic-exit.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/unistd.h"
#include "hackerlab/os/exit.h"
#include "hackerlab/bugs/panic-exit.h"


/************************************************************************
 *(h1 "Exiting Due to Panic"
 * 	:includes ("hackerlab/bugs/panic-exit.h"))
 * 
 * |exitting on panic|
 */

/*(c panic_exit)
 * void panic_exit (void);
 * 
 * This function is called by `panic' to terminate the process with
 * status 1.  (It does not return.  It calls `_exit'.)
 * 
 * For your convenience, this function is defined in its own object
 * file.  You can define your own version and still link against
 * this library.  If you do define your own version, *it must not
 * return*.
 * 
 */
void
panic_exit (void)
{
  /* Why exit with status 2 instead of EXIT_FAILURE?
   *
   * Some programs return information in the exit status and
   * conventionally statuses 0 and 1 are ordinary returns,
   * with status 2 indicating an abnormal exit.  `panic_exit' should 
   * be useful in such programs and in forked subprocesses preparing to 
   * `exec' such programs.
   */
  _exit (2);
}
