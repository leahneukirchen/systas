/* tag: Tom Lord Tue Dec  4 14:41:33 2001 (char-class-locale.c)
 */
/* char-class-locale.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <ctype.h>
#include "hackerlab/os/char-class-locale.h"



int
char_is_alnum_locale (t_uchar c)
{
  return isalnum ((char)c);
}


int
char_is_alpha_locale (t_uchar c)
{
  return isalpha ((char)c);
}


int
char_is_control_locale (t_uchar c)
{
  return iscntrl ((char)c);
}


int
char_is_digit_locale (t_uchar c)
{
  return isdigit ((char)c);
}


int
char_is_graph_locale (t_uchar c)
{
  return isgraph ((char)c);
}


int
char_is_lower_locale (t_uchar c)
{
  return islower ((char)c);
}


int
char_is_printable_locale (t_uchar c)
{
  return isprint ((char)c);
}


int
char_is_punct_locale (t_uchar c)
{
  return ispunct ((char)c);
}


int
char_is_space_locale (t_uchar c)
{
  return isspace ((char)c);
}


int
char_is_upper_locale (t_uchar c)
{
  return isupper ((char)c);
}


int
char_is_xdigit_locale (t_uchar c)
{
  return isxdigit ((char)c);
}


