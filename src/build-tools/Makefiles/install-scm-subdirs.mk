# tag: Tom Lord Tue Dec  4 14:47:16 2001 (install-scm-subdirs.mk)
#
# install-scm-subdirs.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-scm-mk
install-scm-subdirs-mk	:= 1

# Set on entry:	(see also "rules.mk")
#
# Optionally set on entry:
#
#	$(scm-files)	the relative path names of Scheme files to install.
#
# This file installs a library of Scheme programs organized as 
# subdirectories of a source tree, e.g.:
#
#	$(srcdir)/scheme/data-structures
#					/runq.scm
#					/ratlist.scm
#					...
#			/ice-9
#			      ...
#			/unix
#			     ...
#	...
#
# They are installed as corresponding subdirectories of
#
#	$(scm-install-dir)
#
# and links to the subdirectories are installed in
#
#	$(scm-linkdir)
#
# No other directory should install in these same subdirectories of 
# $(scm-install-dir) and $(scm-linkdir).
#
# After installation, the build directory will contain "Installed-scm-files"
# which is a list of the Scheme files that were installed.  The list 
# includes relative path names, e.g.:
#
#	data-structures/runq.scm
#	data-structures/ratlist.scm
#	...
#

include $(makefiles)/rules.mk

ifndef scm-files
scm-files		:=	$(sort $(shell	cd $(srcdir) ;\
						find [a-zA-Z]* -type f -name "*.scm" ))
endif

scm-dirs		:=	$(shell echo $(dir $(scm-files)) | sed -f $(makefiles)/column.sed | sed -e "s,/$$,," | sort -u)


install:	install-scm-files

install-scm-files: $(scm-files)
	echo $(scm-files) | sed -f $(makefiles)/column.sed > Installed-scm-files
	-rm -rf $(addprefix $(scm-install-dir)/,$(scm-dirs))
	mkdir -p $(addprefix $(scm-install-dir)/,$(scm-dirs))
	cd $(srcdir) ; \
	for f in $(scm-files) ; do \
	  cp $$f $(scm-install-dir)/`dirname $$f` ; \
	done

clean: clean-install-scm-files

clean-install-scm-files:
	-rm -f Installed-scm-files

endif
