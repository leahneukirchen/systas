#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:26 2001 (char-tests/unit-char-class.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-char-class tests ================
rm -f ,msg
./unit-char-class > ,msg
cmp ,msg $srcdir/unit-char-class.h

echo ...passed
