/* md5.h:
 *
 ****************************************************************
 * _Minor_ modifications, Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 * 
 * The copyright for the original code (available on SourceForge) is:
 *
 * Copyright (C) 1999, 2002 Aladdin Enterprises.  All rights reserved.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * L. Peter Deutsch
 * ghost@aladdin.com
 * 
 * Aladdin's source id: md5.h,v 1.4 2002/04/13 19:20:28 lpd Exp
 */

#ifndef INCLUDE__LIBSYSTAS__MD5_H
#define INCLUDE__LIBSYSTAS__MD5_H

/*
 * Independent implementation of MD5 (RFC 1321).
 * 
 * This code implements the MD5 Algorithm defined in RFC 1321, whose
 * text is available at
 * 
 * 	http://www.ietf.org/rfc/rfc1321.txt
 */


#include "hackerlab/machine/types.h"

/* struct md5_state_s
 * 
 * The state of the MD5 Algorithm:
 */

typedef struct md5_state_s 
{
  t_uint32 count[2];		/* message length in bits, lsw first */
  t_uint32 abcd[4];		/* digest buffer */
  t_uchar buf[64];		/* accumulate block */
} md5_state_t;



/* automatically generated __STDC__ prototypes */
extern void md5_init (md5_state_t * pms);
extern void md5_append (md5_state_t * pms, const t_uchar * data, int nbytes);
extern void md5_finish(md5_state_t * pms, t_uchar digest[16]);
#endif  /* INCLUDE__LIBSYSTAS__MD5_H */


/* tag: Tom Lord Sat Jan  5 15:26:10 2002 (md5.h)
 */
