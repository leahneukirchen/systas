#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:32 2001 (bitset-tests/unit-bitset-tree.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-bitset-tree: bitset trees ================"
./unit-bitset-tree
echo "...passed"
