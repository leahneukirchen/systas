# install-library-objects.mk:
#
#	Install a library.
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef install-library-objects-mk
install-library-objects-mk	:= 1

# Already set upon entry (see also "library-objects.mk"):
#
#	$(libobjs)	the list of object files to install
#
# After installation, the build directory will contain "Installed-library-objects"
# which contains the name of the object files that were installed.
#

include $(makefiles)/rules.mk

install:	install-library-objects Installed-library-objects

install-library-objects Installed-library-objects: $(libobjs)
	for obj in $(libobjs) ; do \
		-rm -f $(library-install-dir)/$$obj ; \
	done
	mkdir -p $(library-install-dir)
	cp $(libobjs) $(library-install-dir)
	echo $(libobjs) | sed -f $(makefiles)/column.sed > Installed-library-objects

clean: clean-install-library-objects

clean-install-library-objects:
	-rm -f Installed-library-objects

endif

# tag: Tom Lord Tue Dec  4 14:47:15 2001 (install-library-objects.mk)
#
