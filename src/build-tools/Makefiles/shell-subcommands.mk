# shell-subcommands.mk: build shell scripts which implement
#			sub-commands of a program
# 
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef shell-subcommands-mk
shell-subcommands-mk	:= 1

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
# In addition, files in the list:
# 
# 	shell-subcommand-help-source	:=	Help.category.in ...
#
# are copied to the build directory with .in stripped off.
#

ifndef shell-subcommands
shell-subcommands			:=	$(sort $(notdir $(wildcard $(srcdir)/[a-zA-Z0-9]*.sh)))
endif

ifndef subcommand-help-data
shell-subcommand-help-source		:=	$(notdir $(wildcard $(srcdir)/Help.*.in))
endif

shell-subcommand-names		:=	$(basename $(shell-subcommands))
shell-subcommand-help		:=	$(basename $(shell-subcommand-help-source))

ifndef cfg__posix_shell
cfg__posix_shell		:=	/bin/sh
endif

include $(makefiles)/rules.mk

all: $(shell-subcommand-names) $(shell-subcommand-help) Shell-subcommands Shell-subcommand-help

$(shell-subcommand-names):%:%.sh
	-rm -f $@
	if test "`head -n 1 $< | sed -e 's/\(..\).*/\1/'`" != "#!" ; then echo "#!$(cfg__posix_shell)" ; fi > $@
	cat $< >> $@
	chmod ugo+x $@

$(shell-subcommand-help):%:%.in
	-rm -f $@
	cp $< $@

Shell-subcommands: $(shell-subcommand-names)
	echo $(shell-subcommand-names) | sed -f $(makefiles)/column.sed > Programs

Shell-subcommand-help: $(shell-subcommand-help)
	echo $(shell-subcommand-help) | sed -f $(makefiles)/column.sed > Programs

clean: clean-shell-subcommands

clean-shell-subcommands:
	-rm -f $(shell-subcommand-names) Shell-subcommands
	-rm -f $(shell-subcommand-help) Shell-subcommand-help

endif

# tag: Tom Lord Tue Dec  4 14:47:21 2001 (shell-subcommands.mk)
#
