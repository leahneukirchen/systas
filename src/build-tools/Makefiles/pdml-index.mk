# tag: Tom Lord Tue Dec  4 14:47:18 2001 (pdml-index.mk)
#
# pdml-index.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef pdml-index
pdml-index	:= 1

# Build a pdml index from pdml files in this directory and optionally,
# in sibling directories.
#
# Set on entry:	see "library-pdml.mk"
#
# Optionally set on entry:
#
#	$(the-pdml-index)	the name of the library to build.
#	$(pdml-index-otherdirs)	sibling directories (tail only) to include in the library
#
# The default value of $(the-pdml-index) is "$(thisdir).pdml-index".
#
# After "make pdml-index", the build directory will contain "PDML-index" which
# contains the name of the index that was built.
#

include $(makefiles)/library-pdml.mk

ifndef the-pdml-index
the-pdml-index		:=	$(thisdir).pdml-index
endif

other-pdml	:=	$(foreach sibling,$(pdml-index-otherdirs),\
			   $(addprefix ../$(sibling)/, $(shell cat ../$(sibling)/PDML)))

pdml-index: $(the-pdml-index)

$(the-pdml-index): $(pdml-files) $(other-pdml)
	$(xdmlcomp) --markup-language doc/old-pdml $(pdml-index-processors) --make-index $^ > $@
	echo $(the-pdml-index) > PDML-index

clean: clean-pdml-index

clean-pdml-index:
	-rm -f $(the-pdml-index) PDML-index

endif
