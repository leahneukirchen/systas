#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:19 2001 (bugs-tests/unit-panic.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ panic and invariant tests ================

echo "panic test..."
rm -f ,msg
(./unit-panic --panic "this way" 2> ,msg) && (echo "incorrect exit status" ; exit 1)
cmp ,msg $srcdir/unit-panic.panic-msg
echo "... passed"


echo "invariant test..."
./unit-panic --invariant || (echo "incorrect exit status" ; exit 1)
echo "... passed"



echo "botched invariant test..."
rm -f ,msg
(./unit-panic --botched 2> ,msg)  && (echo "incorrect exit status" ; exit 1)
grep -q "unit-panic\\.c:[0-9]*:botched invariant" ,msg
echo "... passed"
