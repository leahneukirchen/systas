# install-programs.mk: install programs
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-programs-mk
install-programs-mk	:= 1

# Already set upon entry (see also "programs.mk"):
#
#	$(programs)	the file-names (no directories) of the programs
#
# After installation, the build directory will contain "Installed-programs"
# which is a list of the programs that were installed.
#

include $(makefiles)/rules.mk

install:	install-programs Installed-programs

install-programs Installed-programs: $(programs)
ifdef programs
	-for f in $(programs) ; do rm -f $(program-install-dir)/$$f; done
	mkdir -p $(program-install-dir)
	cp $(programs) $(program-install-dir)
endif
	echo $(programs) | sed -f $(makefiles)/column.sed > Installed-programs

clean: clean-install-programs

clean-install-programs:
	-rm -f Installed-programs

endif

# tag: Tom Lord Tue Dec  4 14:47:15 2001 (install-programs.mk)
#
