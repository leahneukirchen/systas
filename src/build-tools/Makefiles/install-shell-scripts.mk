# install-shell-scripts.mk: install shell scripts
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-shell-scripts-mk
install-shell-scripts-mk	:= 1

# Already set upon entry (see also "shell-scripts.mk"):
#
#	$(shell-script-names)	the file-names (no directories) of the scripts
#
# After installation, the build directory will contain "Installed-shell-script"
# which is a list of the programs that were installed.
#

include $(makefiles)/rules.mk

install:	install-shell-scripts Installed-shell-scripts

install-shell-scripts Installed-shell-scripts: $(shell-script-names)
ifdef shell-script-names
	-for f in $(shell-script-names) ; do rm -f $(program-install-dir)/$$f; done
	mkdir -p $(program-install-dir)
	cp $(shell-script-names) $(program-install-dir)
endif
	echo $(shell-script-names) | sed -f $(makefiles)/column.sed > Installed-shell-scripts

clean: clean-install-shell-scripts

clean-install-shell-scripts:
	-rm -f Installed-shell-scripts

endif

# tag: Tom Lord Tue Dec  4 14:47:20 2001 (install-shell-scripts.mk)
#
