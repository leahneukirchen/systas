# config-info.mk: build/install rules for the generic 
#		  config program.  This program can 
#		  tell users what version of a program 
# 		  was installed and what options it was
#		  compiled with.
# 
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 

ifndef config-info-mk
config-info-mk	:= 1

# Build programs.
#
# Set on entry:
#
#	$(cfg__std__advertised_as)	a list of names under which to install
#					the config program
# 
#
# config-info.c is compiled as a program.
# 
# It is installed as each of the names in $(cfg__std__advertised_as).
#

mains		:= config-info.c

no-Programs-rule	:= 1
include $(makefiles)/programs.mk

install_names	:=	$(patsubst %, %-cfg, $(cfg__std__advertised_as))

all: $(install_names) Programs

$(install_names) Programs: config-info
	for a in $(install_names) ; do cp config-info $$a ; done
	echo config-info $(install_names) | sed -f $(makefiles)/column.sed > Programs

install:	install-advertised Installed-programs

install-advertised Installed-programs: $(install_names)
ifdef install_names
	-for f in $(install_names) ; do rm -f $(program-install-dir)/$$f; done
	mkdir -p $(program-install-dir)
	cp $(install_names) $(program-install-dir)
endif
	echo $(install_names) | sed -f $(makefiles)/column.sed > Installed-programs

clean: clean-advertised

clean-advertised:
	-rm -f $(install_names) Installed-programs


endif

# tag: Tom Lord Tue Dec  4 14:47:19 2001 (config-info.mk)
#
