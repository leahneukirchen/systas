# tag: Tom Lord Tue Dec  4 14:47:17 2001 (library-ps.mk)
#
# library-ps.mk -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef library-ps
library-ps	:= 1

# Build documentation ps from texi files.
#
# Set on entry:
#
#	$(texi-files)	texi files from which to build a ps file.
#	$(texi-main)	texi file to pass to `texi2dvi' 
#			(default "manual.texi")
#
# After "make ps", the build directory will contain a .ps file
# of documentation compiled from texinfo sources.
#

include $(makefiles)/rules.mk
include $(makefiles)/library-texi.mk

ifndef texi-main
texi-main	:=	manual.texi
endif

ps-file		:=	$(patsubst %.texi,%.ps,$(texi-main))
dvi-file	:=	$(patsubst %.texi,%.dvi,$(texi-main))
ps-dir		:=	$(patsubst %.texi,%.dir,$(texi-main))

$(ps-file): $(texi-main) $(texi-files)
	-mkdir $(ps-dir)
	cd $(ps-dir) ; \
	rm *.texi ; \
	ln -s ../*.texi . ; \
	texi2dvi $(texi-main) ; \
	dvips -O0in,0.5in -o $(ps-file) $(dvi-file) ; \
	mv $(ps-file) ..

ps PS: $(ps-file)
	echo $(ps-file) > PS

clean: clean-ps-files

clean-doc: clean-ps-files 

clean-ps-files:
	-rm -rf $(ps-file) $(ps-dir) PS *.d

endif
