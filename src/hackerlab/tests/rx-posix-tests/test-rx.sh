#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:34 2001 (rx-posix-tests/test-rx.sh)
#


set -e 

arg0="$0"
srcdir=`dirname "$arg0"`

echo "================ test-rx: rx validation tests ================"

echo 
echo "---------------- quick test ----------------"
echo "% time ./test-rx --iterations=1 --repeat=1"
time ./test-rx --iterations=1 --repeat=1
echo "...passed"


echo 
echo "---------------- default cache sizes ----------------"
echo "% time ./test-rx --iterations=2 --repeat=3"
time ./test-rx --iterations=2 --repeat=3
echo "...passed"


echo 
echo "---------------- very small cache sizes ----------------"
echo "|             (long running test < 5 minutes)          |"
echo "--------------------------------------------------------"
echo "% time ./test-rx --dfa-cache-threshold=10240 --nfa-cache-threshold=10240 --iterations=1 --repeat=1"
time ./test-rx --dfa-cache-threshold=10240 --nfa-cache-threshold=10240 --iterations=1 --repeat=1
echo "...passed"


echo 
echo "---------------- very large cache sizes ----------------"
echo "% time ./test-rx --dfa-cache-threshold=8388608 --nfa-cache-threshold=8388608 --iterations=2 --repeat=3"
time ./test-rx --dfa-cache-threshold=8388608 --nfa-cache-threshold=8388608 --iterations=2 --repeat=3
echo "...passed"

echo
echo
# 
# 
# echo "================ timing tests ================"
# echo
# 
# echo 
# echo "---------------- very small cache sizes ----------------"
# echo "% time ./test-rx --dfa-cache-threshold=32768 --nfa-cache-threshold=32768 --iterations=1 --repeat=3"
# /usr/bin/time ./test-rx --dfa-cache-threshold=32768 --nfa-cache-threshold=32768 --iterations=1 --repeat=3
# echo "...passed"
# echo
# 
# echo 
# echo "---------------- small cache sizes ----------------"
# echo "% time ./test-rx --dfa-cache-threshold=65536 --nfa-cache-threshold=65536 --iterations=1 --repeat=3"
# /usr/bin/time ./test-rx --dfa-cache-threshold=65536 --nfa-cache-threshold=65536 --iterations=1 --repeat=3
# echo "...passed"
# echo
# 
# 
# echo 
# echo "---------------- default cache sizes ----------------"
# echo "% time ./test-rx --iterations=1 --repeat=3"
# /usr/bin/time ./test-rx --iterations=1 --repeat=3
# echo "...passed"
# echo
# 
# echo 
# echo "---------------- very large cache sizes ----------------"
# echo "% time ./test-rx --dfa-cache-threshold=8388608 --nfa-cache-threshold=8388608 --iterations=1 --repeat=3"
# /usr/bin/time ./test-rx --dfa-cache-threshold=8388608 --nfa-cache-threshold=8388608 --iterations=1 --repeat=3
# echo "...passed"
# 
# 
