#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:27 2001 (char-tests/unit-str.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-str tests ================
./unit-str

echo ...passed
