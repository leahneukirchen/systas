#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:35 2001 (rx-tests/unit-dfa-utf.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-dfa-utf: dfa tests for UTF-16 and UTF-8 using one NFA ================"
./unit-dfa-utf
echo "...passed"
