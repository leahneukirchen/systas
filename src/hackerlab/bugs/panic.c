/* panic.c - fatal errors
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/unistd.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/bugs/panic-exit.h"
#include "hackerlab/bugs/panic.h"


/************************************************************************
 *(h1 "Panic"
 * 	:includes ("hackerlab/bugs/panic.h"))
 * 
 * |panic|
 * |invariant|
 */



/*(c panic)
 * int panic (char * str);
 * 
 * Print an error message containing `str' on descriptor 2 and exit
 * the process by calling `panic_exit'.
 * 
 * This function uses `write' to print `str'.
 * 
 * This function does not return.
 */
void
panic (char * str)
{
  write (2, "PANIC: ", str_length ("PANIC: "));
  write (2, str, str_length (str));
  write (2, "\n", 1);
  panic_exit ();
}


/*(c panic_msg)
 * int panic_msg (char * str);
 * 
 * Print an error message containing `str' on descriptor 2.
 * 
 * This function uses `write' to print `str'.
 * 
 * This function *does* return.
 */
void
panic_msg (char * str)
{
  write (2, "PANIC MESSAGE: ", str_length ("PANIC MESSAGE: "));
  write (2, str, str_length (str));
  write (2, "\n", 1);
}




/*(c invariant :category macro)
 * void invariant(CONDITION);
 * 
 * Defined as:
 * 
 *   #define invariant(X) invariant_test(X, #X, __FILE__, __LINE__)
 * 
 * 
 * If `CONDITION' evaluates to 0, write a message to the standard
 * error output (descriptor 2) and exit by calling `panic_exit'.
 * See xref:"panic".
 */



/*(c invariant_test)
 * void invariant_test (int condition, char * str, char * file, int line);
 * 
 * If `condition' is 0, write a message to stderr (fd 2) and exit.
 * See xref:"invariant".
 */
void
invariant_test (int condition, char * str, char * file, int line)
{
  char buffer[2 + sizeof (long) * 3];

# define botched "botched invariant\n    "

  if (condition)
    return;
  cvt_long_to_decimal (buffer, line);
  write (2, file, str_length (file));
  write (2, ":", 1);
  write (2, buffer, str_length (buffer));
  write (2, ":", 1);
  write (2, botched, str_length (botched));
  write (2, str, str_length (str));
  write (2, "\n", 1);
  while (1)
    panic ("exiting on botched invariant");
}

