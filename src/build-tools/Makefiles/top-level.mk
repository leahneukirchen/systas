# top-level.mk:	the top level makefile
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 

################################################################
# This is a generic top-level makefile for Hackerlab packages.
# It should never be necessary to edit this file.
# 
# The default target is "all".
# 
# The generic top-level configure script will compute a list of
# subdirectories to build.  When it copies this "Makefile.in"
# to a build directory (to create "Makefile"), configure adds
# a definition for the variable `make-dirs'.
# 
# `make-dirs' will be a list of directories to build, in the order
# in which they should be built.
# 
# For most makefile targets, this makefile simply invokes make
# in each subdirectory with the same target and arguments.
# 
# For the target `clean', this makefile invokes make in each 
# subdirectory, adding the definition `clean-only=1'.
# That definition causes the generic Hackerlab makefiles to 
# avoid needlessly computing header file dependencies.
# 


all:

test:

install:

clean:
ifdef make-dirs
	set -e ; \
	for dir in $(make-dirs); do \
		$(MAKE) -C $$dir clean-only=1 clean ; \
	done
endif

%:
ifdef make-dirs
	set -e ; \
	for dir in $(make-dirs); do \
		$(MAKE) -C $$dir $* ; \
	done
endif



# tag: Tom Lord Tue Dec  4 14:47:19 2001 (top-level.mk)
#
