# install-scm-cgi-scripts.mk:
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-scm-cgi-scripts-mk
install-scm-cgi-scripts-mk	:= 1

# This file installs Scheme programs organized as scripts suitable for
# use with execve.
#
# Already set upon entry (see also "scm-scripts.scm"):
# 
# 	$(scm-scripts)		The scripts to be installed, without
#				the (source) extension (".ss").
# 
# On `make install', the scripts are copied to the install directory
#
#	$(program-install-dir)
#
# with this line prefixed to the script:
#
#	#!$(prefix)/bin/systas
# 
# If the source file already has a first line beginning with "#!",
# that line is removed.
#
# The extension is removed from the script when installed.
#

installed-systas		:=	$(prefix)/bin/systas

include $(makefiles)/rules.mk

install: install-scm-cgi-scripts

install-scm-cgi-scripts: $(addsufix .ss, $(script-names))
	set -e ; \
	for f in $(script-names) ; do \
	  rm -f $(cgi-install-dir)/$$f ; \
	  rm -f $(cgi-install-dir)/,,$$f ; \
	  mkdir -p $(cgi-install-dir) ; \
	  echo "#!$(installed-systas)" > $(cgi-install-dir)/,,$$f ; \
	  cat $(srcdir)/$$f.ss | sed -e "1{/#!/d;}" >> $(cgi-install-dir)/,,$$f ; \
	  chmod ugo+x $(cgi-install-dir)/,,$$f ; \
	  mv $(cgi-install-dir)/,,$$f  $(cgi-install-dir)/$$f ; \
	done
	echo $(script-names) | sed -f $(makefiles)/column.sed > Installed-scm-cgi-scripts

clean: clean-install-scm-cgi-scripts

clean-install-scm-cgi-scripts:
	-rm -f Installed-scm-cgi-scripts

endif

# tag: Tom Lord Tue Dec  4 14:47:15 2001 (install-scm-cgi-scripts.mk)
#
