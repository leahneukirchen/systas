#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:28 2001 (fmt-tests/unit-cvt.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-cvt tests ================

intsize=`./unit-cvt --si`
longsize=`./unit-cvt --sl`

if [ $longsize -eq 4 ] ; then

  maxuld=4294967295
  uldovfl=4294967296

  maxulx=ffffffff
  ulxovfl=100000000

  maxulX=FFFFFFFF
  ulXovfl=100000000

  maxulo=37777777777
  uloovfl=40000000000

  maxld=2147483647
  minld=-2147483648
  ldovfl=2147483648
  ldunfl=-2147483649

  maxlx=7fffffff
  minlx=-80000000
  lxovfl=80000000
  lxunfl=-80000001

  maxlX=7FFFFFFF
  minlX=-80000000
  lXovfl=80000000
  lXunfl=-80000001

  maxlo=17777777777
  minlo=-20000000000
  loovfl=20000000000
  lounfl=-20000000001
elif [ $longsize -eq 8 ] ; then
  maxuld=18446744073709551615
  uldovfl=18446744073709551616

  maxulx=ffffffffffffffff
  ulxovfl=10000000000000000

  maxulX=FFFFFFFFFFFFFFFF
  ulXovfl=10000000000000000

  maxulo=777777777777777777777
  uloovfl=1000000000000000000000

  maxld=2147483647
  minld=-9223372036854775808
  ldovfl=2147483648
  ldunfl=-9223372036854775809

  maxlx=7fffffffffffffff
  minlx=-8000000000000000
  lxovfl=8000000000000000
  lxunfl=-8000000000000001

  maxlX=7FFFFFFFFFFFFFFF
  minlX=-8000000000000000
  lXovfl=8000000000000000
  lXunfl=-8000000000000001

  maxlo=777777777777777777777
  minlo=-1000000000000000000000
  loovfl=1000000000000000000000
  lounfl=-1000000000000000000001
else
  echo "odd word size ($longsize bytes)" 1>&2
  exit 1
fi


echo   unsigned long decimals...
for uld in 0 1 13 1023 $maxuld ; do
  # echo  "uld = %uld " `./unit-cvt --uld $uld`
  test `./unit-cvt --uld $uld`  = $uld
done
# echo "uld = $uldovfl " `./unit-cvt --uld $uldovfl`
test `./unit-cvt --uld $uldovfl` = ERANGE

echo   unsigned long hex...
for ulx in 0 1 13 1023 cafe $maxulx ; do
  # echo  "ulx = %ulx " `./unit-cvt --ulx $ulx`
  test `./unit-cvt --ulx $ulx`  = $ulx
done
# echo "ulx = $ulxovfl " `./unit-cvt --ulx $ulxovfl`
test `./unit-cvt --ulx $ulxovfl` = ERANGE

echo   unsigned long HEX...
for ulX in 0 1 13 1023 CAFE $maxulX ; do
  # echo  "ulX = %ulX " `./unit-cvt --ulX $ulX`
  test `./unit-cvt --ulX $ulX`  = $ulX
done
# echo "ulX = $ulXovfl " `./unit-cvt --ulX $ulXovfl`
test `./unit-cvt --ulX $ulXovfl` = ERANGE
# echo "ulX = cafe (=> CAFE) " `./unit-cvt --ulX cafe`
test `./unit-cvt --ulX cafe` = CAFE

echo   unsigned long octal...
for ulo in 0 1 13 1023 7654 $maxulo ; do
  # echo  "ulo = $ulo " `./unit-cvt --ulo $ulo`
  test `./unit-cvt --ulo $ulo`  = $ulo
done
# echo "ulo = $uloovfl " `./unit-cvt --ulo $uloovfl`
test `./unit-cvt --ulo $uloovfl` = ERANGE



echo long decimals...
for ld in 0 1 13 1023 -1 -9 -21 -1023 $maxld $minld ; do
  # echo "ld == $ld " `./unit-cvt --ld $ld`
  test `./unit-cvt --ld $ld`  = $ld
done
# echo "ld = $ldovfl " `./unit-cvt --ld $ldovfl`
test `./unit-cvt --ld $ldovfl` = ERANGE
# echo "ld = $ldunfl " `./unit-cvt --ld $ldunfl`
test `./unit-cvt --ld $ldunfl` = ERANGE


echo long hex...
for lx in 0 1 13 1023 -1 -9 -21 -1023 cafe -babe $maxlx $minlx ; do
  # echo "lx == $lx " `./unit-cvt --lx $lx`
  test `./unit-cvt --lx $lx`  = $lx
done
# echo "lx = $lxovfl " `./unit-cvt --lx $lxovfl`
test `./unit-cvt --lx $lxovfl` = ERANGE
# echo "lx = $lxunfl " `./unit-cvt --lx $lxunfl`
test `./unit-cvt --lx $lxunfl` = ERANGE

echo long HEX...
for lX in 0 1 13 1023 -1 -9 -21 -1023 CAFE -BABE $maxlX $minlX ; do
  # echo "lX == $lX " `./unit-cvt --lX $lX`
  test `./unit-cvt --lX $lX`  = $lX
done
# echo "lX = $lXovfl " `./unit-cvt --lX $lXovfl`
test `./unit-cvt --lX $lXovfl` = ERANGE
# echo "lX = $lXunfl " `./unit-cvt --lX $lXunfl`
test `./unit-cvt --lX $lXunfl` = ERANGE

echo long octal...
for lo in 0 1 13 1023 -7654 $maxlo ; do
  # echo  "lo = $lo " `./unit-cvt --lo $lo`
  test `./unit-cvt --lo $lo`  = $lo
done
# echo "lo = $loovfl " `./unit-cvt --lo $loovfl`
test `./unit-cvt --lo $loovfl` = ERANGE



if [ $intsize -eq 4 ] ; then

  maxud=4294967295
  udovfl=4294967296

  maxux=ffffffff
  uxovfl=100000000

  maxuX=FFFFFFFF
  uXovfl=100000000

  maxuo=37777777777
  uoovfl=40000000000
    
  maxd=2147483647
  mind=-2147483648
  dovfl=2147483648
  dunfl=-2147483649

  maxx=7fffffff
  minx=-80000000
  xovfl=80000000
  xunfl=-80000001

  maxX=7FFFFFFF
  minX=-80000000
  Xovfl=80000000
  Xunfl=-80000001

  maxo=17777777777
  mino=-20000000000
  oovfl=20000000000
  ounfl=-20000000001
elif [ $intsize -eq 8 ] ; then
  maxud=18446744073709551615
  udovfl=18446744073709551616
  maxux=ffffffffffffffff
  uxovfl=10000000000000000
  maxuX=FFFFFFFFFFFFFFFF
  uXovfl=10000000000000000

  maxd=9223372036854775807
  mind=-9223372036854775808
  dovfl=9223372036854775808
  dunfl=-9223372036854775809

  maxx=7fffffffffffffff
  minx=-8000000000000000
  xovfl=8000000000000000
  xunfl=-8000000000000001

  maxX=7FFFFFFFFFFFFFFF
  minX=-8000000000000000
  Xovfl=8000000000000000
  Xunfl=-8000000000000001

  maxo=777777777777777777777
  mino=-1000000000000000000000
  oovfl=1000000000000000000000
  ounfl=-1000000000000000000001
else
  echo "odd int size ($intsize bytes)" 1>&2
  exit 1
fi



echo   unsigned int decimals...
for ud in 0 1 13 1023 $maxud ; do
  # echo  "ud = $ud " `./unit-cvt --ud $ud`
  test `./unit-cvt --ud $ud`  = $ud
done
# echo "ud = $udovfl " `./unit-cvt --ud $udovfl`
test `./unit-cvt --ud $udovfl` = ERANGE

echo   unsigned int hex...
for ux in 0 1 13 1023 cafe $maxux ; do
  # echo  "ux = $ux " `./unit-cvt --ux $ux`
  test `./unit-cvt --ux $ux`  = $ux
done
# echo "ux = $uxovfl " `./unit-cvt --ux $uxovfl`
test `./unit-cvt --ux $uxovfl` = ERANGE

echo   unsigned int HEX...
for uX in 0 1 13 1023 CAFE $maxuX ; do
  # echo  "uX = $uX " `./unit-cvt --uX $uX`
  test `./unit-cvt --uX $uX`  = $uX
done
# echo "uX = $uXovfl " `./unit-cvt --uX $uXovfl`
test `./unit-cvt --uX $uXovfl` = ERANGE
# echo "uX = cafe (=> CAFE) " `./unit-cvt --uX cafe`
test `./unit-cvt --uX cafe` = CAFE

echo   unsigned int octal...
for uo in 0 1 13 1023 7654 $maxuo ; do
  # echo  "uo = $uo " `./unit-cvt --uo $uo`
  test `./unit-cvt --uo $uo`  = $uo
done
# echo "uo = $uoovfl " `./unit-cvt --uo $uoovfl`
test `./unit-cvt --uo $uoovfl` = ERANGE


echo int decimals...
for d in 0 1 13 1023 -1 -9 -21 -1023 $maxd $mind ; do
  # echo "d == $d " `./unit-cvt --d $d`
  test `./unit-cvt --d $d`  = $d
done
# echo "d = $dovfl " `./unit-cvt --d $dovfl`
test `./unit-cvt --d $dovfl` = ERANGE
# echo "d = $dunfl " `./unit-cvt --d $dunfl`
test `./unit-cvt --d $dunfl` = ERANGE

echo int hex...
for x in 0 1 13 1023 -1 -9 -21 -1023 cafe -babe $maxx $minx ; do
  # echo "x == $x " `./unit-cvt --x $x`
  test `./unit-cvt --x $x`  = $x
done
# echo "x = $xovfl " `./unit-cvt --x $xovfl`
test `./unit-cvt --x $xovfl` = ERANGE
# echo "x = $xunfl " `./unit-cvt --x $xunfl`
test `./unit-cvt --x $xunfl` = ERANGE

echo int HEX...
for X in 0 1 13 1023 -1 -9 -21 -1023 CAFE -BABE $maxX $minX ; do
  # echo "X == $X " `./unit-cvt --X $X`
  test `./unit-cvt --X $X`  = $X
done
# echo "X = $Xovfl " `./unit-cvt --X $Xovfl`
test `./unit-cvt --X $Xovfl` = ERANGE
# echo "X = $Xunfl " `./unit-cvt --X $Xunfl`
test `./unit-cvt --X $Xunfl` = ERANGE

echo   int octal...
for o in 0 1 13 1023 -7654 $maxo ; do
  # echo  "o = $o " `./unit-cvt --o $o`
  test `./unit-cvt --o $o`  = $o
done
# echo "o = $oovfl " `./unit-cvt --o $oovfl`
test `./unit-cvt --o $oovfl` = ERANGE

echo ...passed
