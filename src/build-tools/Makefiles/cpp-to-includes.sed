
# Given the output of "cc -E", produce a list of all
# source files mentioned in line number data output
# by the pre-processor in a format suitable for use
# as makefile dependencies.  (sort -u of the output
# is advisable, though).
# 

s/^#[[:space:]]*line[[:space:]]/# /
/^#[[:space:]][[:space:]]*[[:digit:]][[:digit:]]*[[:space:]][[:space:]]*"/!d
s/%/\\%/g
s/=/\\=/g
s/\$/\\$/g
s/^[^"]*"//
s/"[^"]*$//

# Gcc 3.1 generates these, apparently:
#
s/<built-in>//
s/<command line>//


s/^/    /

# tag: Tom Lord Tue Jan 22 18:29:40 2002 (Makefiles/cpp-to-includes.sed)
#
