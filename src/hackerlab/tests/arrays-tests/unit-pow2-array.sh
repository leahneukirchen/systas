#!/bin/sh 
# Copyright (C) 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-pow2-array tests ================
./unit-pow2-array
echo ...passed

# tag: Tom Lord Tue Dec  4 14:54:30 2001 (arrays-tests/unit-pow2-array.sh)
#
