# install-library.mk:
#
#	Install a library.
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-library-mk
install-library-mk	:= 1

# Already set upon entry (see also "library.mk"):
#
#	$(thelib)	the file-name (no directory) of the library
#
# After installation, the build directory will contain "Installed-library"
# which contains the name of the library that was installed.
#

include $(makefiles)/rules.mk

install:	install-library Installed-library

install-library Installed-library: $(thelib)
	-rm -f $(library-install-dir)/$(thelib)
	mkdir -p $(library-install-dir)
	cp $(thelib) $(library-install-dir)
	echo $(thelib) | sed -f $(makefiles)/column.sed > Installed-library

clean: clean-install-library

clean-install-library:
	-rm -f Installed-library

endif

# tag: Tom Lord Tue Dec  4 14:47:15 2001 (install-library.mk)
#
