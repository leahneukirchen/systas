#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:36 2001 (rx-tests/unit-dfa-iso8859-1.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-dfa-iso8859-1: dfa tests for ascii8 strings of unicode characters ================"
./unit-dfa-iso8859-1
echo "...passed"
