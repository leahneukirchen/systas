/* errnorx.c - error codes for rx
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/rx-posix/errnorx.h"


/************************************************************************
 *(h0 "Rx Error Codes"
 *    :includes ("rx-posix/errnorx.h"))
 * 
 */




/*(c rx_error_msg)
 * extern const t_uchar *rx_error_msg[];
 * 
 * `rx_error_msg' is an array mapping error codes to strings.  This is
 * useful with the return values of `regcomp', `regncomp', `regexec',
 * `regnexec', and `rx_parse'.
 */
const t_uchar *rx_error_msg[] =
{
#undef RX_ERRNO
#define RX_ERRNO(A,B)	B,
  RX_ERRNO_LIST
};

