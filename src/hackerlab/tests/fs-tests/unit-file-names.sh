#!/bin/sh 
# tag: Tom Lord Tue Dec  4 14:54:32 2001 (fs-tests/unit-file-names.sh)
#


set -e

arg0="$0"
srcdir=`dirname "$arg0"`

echo ================ unit-file-names tests ================

echo home...
test `./unit-file-names --home` = `echo ~/`

echo is-absolute...
test `./unit-file-names --is-absolute /this/is` = yes
test `./unit-file-names --is-absolute /` = yes
test `./unit-file-names --is-absolute this/is/not` = no
test `./unit-file-names --is-absolute ./this/is/not` = no


echo expand...
test `./unit-file-names --expand /no/change` = /no/change
test `./unit-file-names --expand "~/some/file"` = `echo ~/some/file`
test `./unit-file-names --expand "~root/some/file"` = `echo ~root/some/file`
test `./unit-file-names --expand "~no_such_user/some/file"` = `echo ~no_such_user/some/file`

echo tail...
test `./unit-file-names --tail /usr/bin/ls` = ls
test x`./unit-file-names --tail /usr/bin/` = x
test `./unit-file-names --tail ls` = "ls"

echo as-directory...
test `./unit-file-names --as-directory /usr/bin/ls` = /usr/bin/ls/
test `./unit-file-names --as-directory /usr/bin/` = /usr/bin/
test `./unit-file-names --as-directory /usr/bin////` = /usr/bin/
test `./unit-file-names --as-directory ls` = ls/


echo directory-file-name...
test `./unit-file-names --directory-file-name /usr/bin/ls` = /usr/bin/ls
test `./unit-file-names --directory-file-name ls` = ls
test `./unit-file-names --directory-file-name /usr/bin/` = /usr/bin
test `./unit-file-names --directory-file-name /usr/bin/////` = /usr/bin
test `./unit-file-names --directory-file-name /` = /

echo file-name-directory...
test `./unit-file-names --file-name-directory /usr/bin/ls` = /usr/bin/
test `./unit-file-names --file-name-directory /usr/bin/////ls` = /usr/bin/
test `./unit-file-names --file-name-directory /usr/bin/` = /usr/bin/
test `./unit-file-names --file-name-directory /usr/bin` = /usr/
test `./unit-file-names --file-name-directory /` = /
test `./unit-file-names --file-name-directory ls` = "(null)"

echo in-vicinity...
test `./unit-file-names --in-vicinity /usr/bin ls`  = /usr/bin/ls
test `./unit-file-names --in-vicinity /usr/bin/ ls`  = /usr/bin/ls
test `./unit-file-names --in-vicinity /usr/bin/// ls`  = /usr/bin/ls
test `./unit-file-names --in-vicinity /etc /usr/bin/ls`  = /usr/bin/ls
test `./unit-file-names --in-vicinity "" ls`  = ls

echo path...
rm -f ,path
echo /bin > ,path
echo /usr/bin >> ,path
echo . >> ,path
echo /usr/local/bin >> ,path
./unit-file-names --path /bin:/usr/bin::/usr/local/bin | cmp - ,path

echo which...
test `./unit-file-names --which ls` = `which ls`
test `./unit-file-names --which cat` = `which cat`

# `which test` doesn't reliably return a path when /bin/sh
# is zsh.
# 
# test `./unit-file-names --which test` = `which test`

echo ...passed
