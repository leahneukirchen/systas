/* tag: Tom Lord Tue Dec  4 14:41:33 2001 (char-cmp-locale.c)
 */
/* char-cmp-locale.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <string.h>
#include "hackerlab/os/char-cmp-locale.h"



int
char_cmp_locale (t_uchar a, t_uchar b)
{
  t_uchar as[2];
  t_uchar bs[2];

  as[0] = a;
  as[1] = 0;
  bs[0] = b;
  bs[1] = 0;

  return strcoll ((char *)as, (char *)bs);
}
