# tag: Tom Lord Tue Dec  4 14:47:16 2001 (install-scm.mk)
#
# install-scm.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-scm-mk
install-scm-mk		:= 1

# This file installs a library of Scheme programs organized as 
# one-level directory of a source tree, e.g.:
#
#	$(srcdir)/piw-analyze/report.scm
#			     /sim.scm
#
# They are installed as a corresponding subdirectory of
#
#	$(scm-install-dir)
#
# For example:
#
#	$(scm-install-dir)/piw-analyze
# 
# After installation, the build directory will contain
# "Installed-scm-files" which is a list of the Scheme files that were
# installed.  The list includes relative path names, e.g.:
#
#	piw-analyze/report.scm
#	piw-analyze/sim.scm
#	...
#

include $(makefiles)/rules.mk

ifndef scm-files
scm-files		:=	$(sort $(notdir $(wildcard $(srcdir)/[^,+=]*.scm)))
endif


install:	install-scm-files

install-scm-files: $(scm-files)
	echo $(addprefix $(thisdir)/, $(scm-files)) | sed -f $(makefiles)/column.sed > Installed-scm-files
	-rm -rf $(scm-install-dir)/$(thisdir)
	mkdir -p $(scm-install-dir)/$(thisdir)
	cp $(addprefix $(srcdir)/, $(scm-files)) $(scm-install-dir)/$(thisdir)

clean: clean-install-scm-files

clean-install-scm-files:
	-rm -f Installed-scm-files

endif
