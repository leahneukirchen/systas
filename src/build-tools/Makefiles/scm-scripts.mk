# tag: Tom Lord Tue Dec  4 14:47:18 2001 (scm-scripts.mk)
#
# scm-scripts.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef scm-scripts-mk
scm-scripts-mk	:= 1

# This file creates Scheme programs organized as scripts
# suitable for use with execve.  These programs are
# created in the build directory and not installed.
# (See "install-scm-scripts.mk".)
#
# The variable $(scm-scripts) may be set on entry to a
# list of the scripts which should have the extension ".ss", 
# for example:
#
#	scm-scripts	:=	piw-analyze.ss
#
# By default, scm-scripts is all files in the source directory
# with the extension "ss".
#
# On `make all', the scripts are copied to the build directory
# with this line prefixed to the script:
#
#	#!$(shell pwd)/../systas/systas
#
# If the source file already has a first line beginning
# with "#!", that line is removed.
#
# The extension is removed from the script when copied to the build
# directory.
#

ifndef scm-scripts
scm-scripts			:=	$(sort $(notdir $(wildcard $(srcdir)/[^,+=]*.ss)))
endif

script-names			:=	$(basename $(scm-scripts))
build-systas			:=	$(cfg__std__objroot)/systas/systas/systas

include $(makefiles)/rules.mk

all: $(script-names)

$(script-names):%:%.ss
	-rm -f $@
	echo "#!$(build-systas)" > $@
	cat $< | sed -e "1{/#!/d;}" >> $@
	chmod ugo+x $@

clean: clean-scm-scripts

clean-scm-scripts:
	-rm -f $(script-names)

endif
