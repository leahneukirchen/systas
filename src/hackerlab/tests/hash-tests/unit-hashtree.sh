#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:33 2001 (hash-tests/unit-hashtree.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-hashtree tests ================
./unit-hashtree < $srcdir/unit-hashtree.script | cmp - $srcdir/unit-hashtree.answers

echo ...passed
