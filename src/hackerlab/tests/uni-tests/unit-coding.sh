#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:36 2001 (uni-tests/unit-coding.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ unit-coding: UTF-8/UTF-16 conversion tests ================"

echo WARNING: non-native byte order of UTF-16 not tested yet.

./unit-cvt -8 < $srcdir/demo.utf8 > ,ref-utf16
./unit-cvt < ,ref-utf16 > ,ref-utf8

cmp $srcdir/demo.utf8 ,ref-utf8

./unit-coding  -8 < $srcdir/demo.utf8 > ,uni-utf16
./unit-coding < ,uni-utf16 > ,uni-utf8

cmp ,ref-utf16 ,uni-utf16
cmp ,ref-utf8 ,uni-utf8

echo "...passed"
