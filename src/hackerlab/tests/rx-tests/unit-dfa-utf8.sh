#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:35 2001 (rx-tests/unit-dfa-utf8.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-dfa-utf8: dfa tests for UTF-8 ================"
./unit-dfa-utf8
echo "...passed"
