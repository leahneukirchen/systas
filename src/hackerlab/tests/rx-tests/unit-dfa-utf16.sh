#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:35 2001 (rx-tests/unit-dfa-utf16.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-dfa-utf16: dfa tests for UTF-16 ================"
./unit-dfa-utf16
echo "...passed"
