# dirs.mk: makefile for a directory of sub-directories
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
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


# tag: Tom Lord Tue Dec  4 14:47:14 2001 (dirs.mk)
#
