# shell-scripts.mk: "make all" rules for shell scripts
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef shell-scripts-mk
shell-scripts-mk	:= 1

# Already set upon entry: see "rules.mk".
# 
# Optionally set on entry:
# 
# 	$(shell-scripts)	source files, with the extension .sh,
# 				to turn into executable scripts.
# 
# 	$(cfg__posix_shell)	the program to use in "#!" lines
#				for shell scripts (default "/bin/sh")
# 
# This file creates executable programs from shell scripts.  These
# programs are created in the build directory and not installed.  (See
# "install-program.mk".)
#
# By default, shell-scripts is all files in the source directory with
# the extension "sh" and beginning with any of [a-zA-Z0-9].
#
# On `make all', the scripts are copied to the build directory,
# without the ".sh" extension, and chmoded to be executable by
# anyone.
#
# If the source file does not already have a first line beginning
# with "#!", then a line "#!/bin/sh" is added.
#
#

ifndef shell-scripts
shell-scripts			:=	$(sort $(notdir $(wildcard $(srcdir)/[a-zA-Z0-9]*.sh)))
endif

shell-script-names		:=	$(basename $(shell-scripts))

ifndef cfg__posix_shell
cfg__posix_shell		:=	/bin/sh
endif

include $(makefiles)/rules.mk

all: $(shell-script-names) Shell-scripts

$(shell-script-names):%:%.sh
	-rm -f $@
	if test "`head -n 1 $< | sed -e 's/\(..\).*/\1/'`" != "#!" ; then echo "#!$(cfg__posix_shell)" ; fi > $@
	cat $< >> $@
	chmod ugo+x $@

Shell-scripts:  $(shell-script-names)
	echo $(programs) | sed -f $(makefiles)/column.sed > Programs

clean: clean-shell-scripts

clean-shell-scripts:
	-rm -f $(shell-script-names) Shell-scripts

endif

# tag: Tom Lord Tue Dec  4 14:47:20 2001 (shell-scripts.mk)
#
