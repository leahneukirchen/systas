#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:29 2001 (mem-tests/unit-mem.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-mem tests ================
./unit-mem
echo ...passed
