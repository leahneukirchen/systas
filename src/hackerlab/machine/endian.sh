#!/bin/sh
# tag: Tom Lord Tue Dec  4 14:54:41 2001 (machine/endian.sh)
#

CC="$1"

cat > endian-test.c << EOF

main()
{
  unsigned int x = 1;

  exit (*(unsigned char *)&x);
}
 
EOF

cat > ,tmp << EOF
/* endian.h - endianness
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__MACHINE__ENDIAN_H
#define INCLUDE__MACHINE__ENDIAN_H

#define MACHINE_IS_BIGENDIAN  @BIG@


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__MACHINE__ENDIAN_H */
EOF

$CC -o endian-test endian-test.c

if ./endian-test ; then
  sed -e s/@BIG@/1/ ,tmp > endian.h
else
  sed -e s/@BIG@/0/ ,tmp > endian.h
fi

rm ,tmp

