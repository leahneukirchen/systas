#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:31 2001 (bitset-tests/unit-bitset.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-bitset: basic bitset tests ================"
./unit-bitset < $srcdir/unit-bitset.tests | cmp $srcdir/unit-bitset.answers -
echo "...passed"
