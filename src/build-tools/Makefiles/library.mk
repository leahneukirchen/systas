# library.mk:
#
#	Assemble object files into a static library.
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 

ifndef library-mk
library-mk	:= 1

# Build a library from objects in this directory and optionally,
# in sibling directories.
#
# Set on entry:	see "library-objects.mk"
#
# Optionally set on entry:
#
#	$(thelib)	the name of the library to build.
#	$(libobjs)	object files to include in the library
#	$(otherdirs)	sibling directories (tail only) to 
#			also include in the library
#
# The default value of $(thelib) is "lib$(thisdir).a"  However, if 
# $(thisdir) begins with the string "lib", then $(thelib) defaults 
# to "$(thisdir).a"
# 
# When other directories are specified with $(otherdirs), they 
# should be built before this directory, and the build directory
# should contain the file "Objects" which is a list of object files
# to include in the library, one per line.  In this directory,
# the make variable $(libobjs) defines which objects to include.
#
# After "make all", the build directory will contain "Library" which
# contains the name of the library that was built.
#

include $(makefiles)/library-objects.mk

otherobjs	:=	$(foreach sibling,$(otherdirs),\
			   $(addprefix ../$(sibling)/, $(shell cat ../$(sibling)/Objects)))

ifeq ($(libobjs)$(otherobjs),)

  # no object files for this library
  # 
  # Don't build it because some systems don't like empty 
  # libraries.
  # 

  thelib	:=

else

ifndef thelib
thelib		:=	$(subst liblib,lib,lib$(thisdir).a)
endif

all: $(thelib)

$(thelib): $(libobjs) $(otherobjs)
	rm -f $(thelib)
	ar -rc $(thelib) $(libobjs) $(otherobjs)
	ranlib $(thelib)
	echo $(thelib) > Library

clean: clean-lib

clean-lib:
	-rm -f $(thelib) Library

endif

endif

# tag: Tom Lord Tue Dec  4 14:47:17 2001 (library.mk)
#
