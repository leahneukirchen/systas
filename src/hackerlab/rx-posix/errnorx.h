/* errnorx.h - error decls for rx
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX_POSIX__ERRNORX_H
#define INCLUDE__RX_POSIX__ERRNORX_H



#include "hackerlab/machine/types.h"



extern const t_uchar * rx_error_msg[];

#define RX_ERRNO_LIST \
  RX_ERRNO(REG_NOERROR, "no error") \
  RX_ERRNO(REG_NOMATCH, "no match") \
  RX_ERRNO(REG_MATCH_INTERRUPTED, "match interrupted") \
  RX_ERRNO(REG_BADPAT, "invalid regular expression") \
  RX_ERRNO(REG_ECOLLATE, "invalid collation character") \
  RX_ERRNO(REG_ECTYPE, "invalid character class name") \
  RX_ERRNO(REG_EESCAPE, "trailing backslash") \
  RX_ERRNO(REG_ESUBREG, "invalid back reference") \
  RX_ERRNO(REG_EBRACK, "unmatched [ or [^") \
  RX_ERRNO(REG_EPAREN, "unmatched (, \\(, ) or \\)") \
  RX_ERRNO(REG_EBRACE, "unmatched \\{") \
  RX_ERRNO(REG_BADBR, "invalid content of \\{\\}") \
  RX_ERRNO(REG_ERANGE, "invalid range end") \
  RX_ERRNO(REG_ESPACE, "memory exhausted") \
  RX_ERRNO(REG_BADRPT, "invalid preceding regular expression") \
  RX_ERRNO(REG_EEND, "premature end of regular expression") \
  RX_ERRNO(REG_ESIZE, "regular expression too big") \
  RX_ERRNO(REG_ELPAREN, "unmatched (, or \\(") \
  RX_ERRNO(REG_ERPAREN, "unmatched ) or \\)")

enum rx_errno
{
#undef RX_ERRNO
#define RX_ERRNO(A,B)	A,
  RX_ERRNO_LIST
};


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__RX_POSIX__ERRNORX_H */
