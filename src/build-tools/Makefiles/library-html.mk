# tag: Tom Lord Tue Dec  4 14:47:16 2001 (library-html.mk)
#
# library-html.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef library-html
library-html	:= 1

# Build documentation html files from pdml files.
#
# Set on entry:
#
#	$(pdml-files)	pdml files from which to build html files.
#	$(html-skip)	files to ignore in $(pdml-files)
#	$(docindex)	the .pdml-index file to use when building html.
#
# After "make html", the build directory will contain .html files
# of documentation extracted from the library source.
#

include $(makefiles)/rules.mk
include $(makefiles)/library-pdml.mk

html-source	:=	$(filter-out $(html-skip), $(pdml-files))
html-files	:= 	$(sort $(patsubst %.pdml, %.html, $(notdir $(html-source))))
pdml-index-processors	+= --output-format html

html HTML: $(html-files)
	echo $(html-files) | sed -f $(makefiles)/column.sed > HTML

clean: clean-html-files

clean-doc: clean-html-files 

clean-html-files:
	-rm -f $(html-files) HTML *.d

endif
