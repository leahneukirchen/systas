#!/bin/sh 
# Copyright (C) 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 

set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-bits: generic bitsets ================"
./unit-bits
echo "...passed"


# tag: Tom Lord Tue Dec  4 14:54:31 2001 (bitset-tests/unit-bits.sh)
#

