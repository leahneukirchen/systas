# install-shell-cgi-scripts.mk: install shell cgi scripts
#
################################################################
# Copyright (C) 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-shell-cgi-scripts-mk
install-shell-cgi-scripts-mk	:= 1

# Already set upon entry (see also "shell-scripts.mk"):
#
#	$(shell-script-names)	the file-names (no directories) of the scripts
#
# After installation, the build directory will contain "Installed-shell-cgi-scripts"
# which is a list of the programs that were installed.
#

include $(makefiles)/rules.mk

install:	install-shell-cgi-scripts Installed-shell-cgi-scripts

install-shell-cgi-scripts Installed-shell-cgi-scripts: $(shell-script-names)
ifdef shell-script-names
	-for f in $(shell-script-names) ; do rm -f $(cgi-install-dir)/$$f; done
	mkdir -p $(cgi-install-dir)
	cp $(shell-script-names) $(cgi-install-dir)
endif
	echo $(shell-script-names) | sed -f $(makefiles)/column.sed > Installed-shell-cgi-scripts

clean: clean-install-shell-cgi-scripts

clean-install-shell-cgi-scripts:
	-rm -f Installed-shell-cgi-scripts

endif

# tag: Tom Lord Mon Apr 29 23:38:43 2002 (Makefiles/install-shell-cgi-scripts.mk)
#
