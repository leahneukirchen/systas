# tag: Tom Lord Tue Dec  4 14:47:17 2001 (library-pdml.mk)
#
# library-pdml.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef library-pdml
library-pdml	:= 1

# Build documentation pdml files from library source code.
#
# Set on entry:
#
#	$(source-files)		files from which to build pdml files.
#	$(documentation-skip)	file to ignore in $(source-files).
#	$(other-documentation)  files in other directories to include in documentation
#
# Optionally set:
#
#	$(pdml-files)	source files already in pdml format.
#
# After "make pdml", the build directory will contain pdml files
# of documentation extracted from the library source.
#

ifdef other-documentation
vpath % $(addprefix $(srcdir)/, $(dir $(other-documentation)))
endif

include $(makefiles)/rules.mk

doc-source	:=	$(filter-out $(documentation-skip), $(wildcard $(srcdir)/*.doc) $(filter %.doc, $(other-documentation)))
pdml-source	:=	$(filter-out $(documentation-skip), $(source-files)) $(filter %.c, $(other-documentation))
pdml-files	:= 	$(sort $(pdml-files) $(patsubst %.doc, %.pdml, $(notdir $(doc-source)))  $(patsubst %.c, %.pdml, $(notdir $(pdml-source))))

ifdef clean-only
pdml-depfiles	:=
else
pdml-depfiles	:= 	$(patsubst %.c, %.d, $(notdir $(pdml-source)))
endif


pdml PDML: $(pdml-files)
	echo $(pdml-files) | sed -f $(makefiles)/column.sed > PDML

clean: clean-pdml-files

clean-doc: clean-pdml-files 

clean-pdml-files:
	-rm -f $(pdml-files) PDML *.d

ifndef clean-only
ifdef libdepfiles
-include $(pdml-depfiles)
endif
endif


endif
