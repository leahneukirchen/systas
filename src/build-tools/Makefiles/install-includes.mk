# install-includes.mk: Install include files.
#	
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 


ifndef install-includes-mk
install-includes-mk	:=	1

include $(makefiles)/rules.mk

# Set on entry:	see "rules.mk"
#
# Optionally set on entry:
#
#	$(includes)	the names of the include files to install.
# 
# By default, every .h file in the source directory and every header
# named in $(generated-includes) is installed.
# 
# Before installing any files, the installation directory is
# removed and re-created.  Any additional files that happen to be 
# stored there are removed.
# 
#	$(include-subdirs)
#			If $(includes) is not set, then .h files from
#			these subdirectories of the source dir will
# 			also be installed, in corresponding subdirs
# 			of the installation directory.
# 
# The relative path from the root of the source to this dir is $(thispath).
# Include files are installed in:
# 
# 		$(prefix)/include/$(thispath)/
# 
# After installation, the build directory will contain "Installed-includes"
# which is a list of the include files that were installed.
#
ifndef includes

top-includes	:=	$(wildcard $(srcdir)/[^,+=]*.h)

ifdef include-subdirs
sub-includes	:=	$(foreach subdir, $(include-subdirs), $(wildcard $(srcdir)/$(subdir)/[^,+=]*.h))
mkdir-list	:=	$(include-install-dir) $(addprefix $(include-install-dir)/, $(include-subdirs))
else
mkdir-list	:=	$(include-install-dir)
endif

includes	:=	$(sort $(generated-includes) $(top-includes) $(sub-includes))

endif

source-include-tails	:= $(patsubst $(srcdir)/%, %, $(top-includes) $(sub-includes))
include-tails	:=	$(patsubst $(srcdir)/%, %, $(includes))

install:	install-includes Installed-includes

install-includes Installed-includes: $(include-tails)
	-rm -rf $(include-install-dir)
	mkdir -p $(mkdir-list)
ifdef source-include-tails
	for header in $(source-include-tails) ; do \
	  cp $(srcdir)/$$header $(include-install-dir)/$$header ; \
	done
endif
ifdef generated-includes
	cp $(generated-includes) $(include-install-dir)
endif
	echo $(includes) | sed -f $(makefiles)/column.sed > Installed-includes

clean: clean-install-includes

clean-install-includes:
	-rm -f Installed-includes

endif

# tag: Tom Lord Tue Dec  4 14:47:14 2001 (install-includes.mk)
#
