# library-objects.mk:
#
#	Compile all of the source files which are not the `main'
#	modules for programs into .o files.
# 
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef library-objects-mk
library-objects-mk	:= 1

# Build object files, as for a library.
#
# Set on entry:
#
#	$(source-files)	files from which to build objects and programs
#	$(mains)	source files to exclude from this part of the build
#
# Ordinarily, $(mains) is the list of source files that define "main".
# This is usually defined in the "Makefile.in" of the source directory.
# $(source-files) is usually computed by "rules.mk".
#
# After "make all", the build directory will contain "Objects" which
# is a list of the object files that were built by this makefile.  
# For every source file which is not in $(mains), the build directory will
# contain a .o file.
# 


include $(makefiles)/rules.mk

libsource	:=	$(filter-out $(mains), $(source-files))
libobjs		:= 	$(sort $(libobjs) $(patsubst %.c, %.o, $(notdir $(libsource))))
libdepfiles	:= 	$(patsubst %.c, %.d, $(notdir $(libsource)))

all Objects: $(libobjs)
	echo $(libobjs) | sed -f $(makefiles)/column.sed > Objects

clean: clean-libobjs

clean-libobjs:
	-rm -f $(libobjs) Objects $(libdepfiles)

endif

# tag: Tom Lord Tue Dec  4 14:47:16 2001 (library-objects.mk)
#
