#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:29 2001 (mem-tests/unit-alloc-limits.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-alloc-limits tests ================

./unit-alloc-limits 

echo ...passed


