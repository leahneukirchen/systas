#!/bin/sh
# tag: Tom Lord Tue Dec  4 14:54:34 2001 (hash-tests/unit-hash-utils.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-hash-utils tests ================
./unit-hash-utils

echo ...passed
