# tag: Tom Lord Tue Dec  4 14:47:17 2001 (library-texi.mk)
#
# library-texi.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef library-texi
library-texi	:= 1

# Build documentation texi from pdml files.
#
# Set on entry:
#
#	$(pdml-files)	pdml files from which to build texi files.
#
# After "make texi", the build directory will contain .texi files
# of documentation extracted from the library source.
#

include $(makefiles)/rules.mk
include $(makefiles)/library-pdml.mk
include $(makefiles)/pdml-index.mk

texi-source	:=	$(pdml-files)
texi-files	:= 	$(sort $(patsubst %.pdml, %.texi, $(notdir $(texi-source))))
pdml-index-processors	+= --output-format texinfo

texi TEXI: $(texi-files)
	echo $(texi-files) | sed -f $(makefiles)/column.sed > TEXI

clean: clean-texi-files

clean-texi-files:
	-rm -f $(texi-files) TEXI *.d

endif
