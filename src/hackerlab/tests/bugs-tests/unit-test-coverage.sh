#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:26 2001 (bugs-tests/unit-test-coverage.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ test coverage tests ================

echo "full coverage test..."
rm -f ,msg
(./unit-test-coverage 2> ,msg) || (echo "incorrect exit status" ; exit 1)
cmp ,msg /dev/null
echo "... passed"


echo "skip point B test..."
rm -f ,msg ,correct
(./unit-test-coverage --skip-b 2> ,msg) && (echo "incorrect exit status" ; exit 1)
echo "PANIC: test coverage failure: point_b" > ,correct
cmp ,msg ,correct
echo "... passed"


echo "skip point A test..."
rm -f ,msg ,correct
(./unit-test-coverage --skip-a 2> ,msg) && (echo "incorrect exit status" ; exit 1)
echo "PANIC: test coverage failure: point_a" > ,correct
cmp ,msg ,correct
echo "... passed"

echo "skip points A and B test..."
rm -f ,msg ,correct
(./unit-test-coverage --skip-a --skip-b 2> ,msg) && (echo "incorrect exit status" ; exit 1)
echo "PANIC: test coverage failure: point_a" > ,correct
cmp ,msg ,correct
echo "... passed"

