# install-shell-subcommands.mk
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-shell-subcommands-mk
install-shell-subcommands-mk	:= 1

# Already set upon entry (see also "shell-subcommands.mk"):
#
#	$(shell-subcommand-names)	the file-names (no directories) of 
#					the subcommands
# 
#	$(shell-subcommand-help)	the file-names (no directories) of 
# 					help files to install
# 
# Optionally set:
# 
# 	$(application-name)		name of the command for which this 
#					directory provides sub-commands
# 
# 	$(shell-subcommand-helpers)	files that should be installed
#					in the same directory as subcommands,
# 					but without symlinks (see below)
# 
# If $(application-name) *is not* set, programs are installed in:
# 
# 	$(prefix)/libexec/$(thisdir)/
# 
# If $(application-name) *is* set, programs are installed in:
# 
# 	$(prefix)/libexec/$(application-name)/$(thisdir)/
# 
# along with a symbolic link to the program in:
# 
# 	$(prefix)/libexec/$(application-name)/$(application-name)
# 
# Along with the scripts, copies of the files $(shell-subcommand-help)
# are installed (in the same location, but without symbolic links).
# 
# After installation, the build directory will contain
# "Installed-shell-subcommands" which is a list of the programs that
# were installed.
#

ifdef application-name
install-location	:=	$(libexec-install-dir)/$(application-name)/$(thisdir)
else
install-location	:=	$(libexec-install-dir)/$(thisdir)
endif

include $(makefiles)/rules.mk

install:	install-shell-subcommands Installed-shell-subcommands

install-shell-subcommands Installed-shell-subcommands: $(shell-subcommand-names) $(shell-subcommand-help)
	-rm -rf $(install-location)
	mkdir -p $(install-location)
ifdef shell-subcommand-helpers
	cp $(addprefix $(srcdir)/, $(shell-subcommand-helpers)) $(install-location)
endif
ifdef shell-subcommand-names
	cp $(shell-subcommand-names) $(install-location)
ifdef application-name
	for f in $(shell-subcommand-names) ; do \
	  mkdir -p $(install-location)/../$(application-name) ; \
	  rm -f  $(install-location)/../$(application-name)/$$f ; \
	  ln -s $(install-location)/$$f $(install-location)/../$(application-name)/$$f ; \
	done
endif
endif
ifdef shell-subcommand-help
	cp $(shell-subcommand-help) $(install-location)
endif
	echo $(shell-subcommand-names) | sed -f $(makefiles)/column.sed > Installed-shell-subcommands

clean: clean-install-shell-subcommands

clean-install-shell-subcommands:
	-rm -f Installed-shell-subcommands

endif

# tag: Tom Lord Tue Dec  4 14:47:20 2001 (install-shell-subcommands.mk)
#
