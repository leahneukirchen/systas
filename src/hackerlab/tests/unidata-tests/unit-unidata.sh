#!/bin/sh
# tag: Tom Lord Tue Dec  4 14:54:36 2001 (unidata-tests/unit-unidata.sh)
#


set -e

echo "================ unit-unidata test ================"

rm -f ,tmp ,tmp2
echo "pruning unidata.txt"

sed -e "s/\([^;]*;\)[^;]*;\([^;]*;[^;]*;[^;]*;[^;]*;[^;]*;\)[^;]*;[^;]*;\([^;]*;\)[^;]*;[^;]*;\([^;]*;[^;]*;[^;]*\)/\1\2\3\4/" < $srcroot/hackerlab/unidata-scaffolding/unidata.txt > ,tmp

echo "printing hackerlab db"
./unit-unidata > ,tmp2

echo "comparing results"
cmp ,tmp ,tmp2

echo ...passed



