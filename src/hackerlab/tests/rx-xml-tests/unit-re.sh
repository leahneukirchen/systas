#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:37 2001 (rx-xml-tests/unit-re.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ xml regexp tests ================"
time ./unit-re
echo "================ xml regexp tests (alternate syntax) ================"
time ./unit-re --alternate-syntax
echo "...passed"

echo "================ xml regexp tests (10K caches) ================"
time ./unit-re --nfa-cache-threshold=10240 --dfa-cache-threshold=10240

echo "================ xml regexp tests (8MB caches) ================"
time ./unit-re --nfa-cache-threshold=8388608 --dfa-cache-threshold=8388608

echo "================ xml regexp tests (stress test < 5 minutes) ================"
time ./unit-re --iterations=500 --cache-compilations


