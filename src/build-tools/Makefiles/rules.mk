# rules.mk: the makefile all other makefiles include first
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 


################################################################
# Presumed already set (from configure):
#
#	$(prefix)	root of the install directory
#	$(srcdir)	the source file directory
#	$(srcroot)	the root of the source tree
#	$(thispath)	$(srcdir) relative to $(srcroot)
#
# May be already set:
#
#	CFLAGS		_additional_ flags for the C compiler (e.g. "-O")
#			default: -g.  This may be set from the command line
#			but should not be set in a Makefile that includes 
# 			rules.mk
# 
# 	EXTRA_CFLAGS	_additional_ flags for the C compiler.  This may be
#			set in a Makefile that includes rules.mk.
#
#	generated-includes
#			a list of include files generated during compilation
# 
#	generated-source
#			a list of source files generated during compilation
# 
#	clean-only
#			if defined, this was invoked from the generic top-level
#			makefile for `make clean'.
# 
################################################################

ifndef rules-mk

rules-mk		:= 1


################################################################
# Standard Targets
# 
################################################################

# The default dependency
#
all:

# Some targets that must be defined (by convention):
#

install: all

test: all


# There must never be a source file named "FORCE", nor any rule
# for producing a file named "FORCE"
# 
FORCE:


################################################################
# Automatic Identification of Source Files
# 
################################################################

# $(source-files)	the complete list of source files.   
#			For the moment, we only like C, but really
#			anything for which we have a rule to produce
#			a .o file is fine.
#
#			Source files must begin with [a-zA-Z0-9].  In
#			"larch inventory", source can also begin with "=".
#			Source files beginning with "=" are presumed to be
#			`scaffolding' -- programs used during development that
#			should not be compiled during an ordinary build.
#

ifndef source-files
    source-files		:= 	$(sort 	$(generated-source) \
						$(source-files) \
						$(notdir $(wildcard $(srcdir)/[a-zA-Z0-9]*.c)))
endif

################################################################
# Derived Paths
# 
################################################################

# $(thisdir)	tail of `pwd` in the build directory
# 
# thisdir is used to automatically generate names.  For instance,
# the names of installation directories.
# 

thisdir			:=	$(notdir $(shell pwd))

# Where programs are installed:
# 
#  	     $(prefix)/bin			# binaries
#	     	      /cgi			# CGI executables
#  	              /lib			# libraries
#  	              /include/$(thispath)	# includes
#		      /etc			# the dreaded miscellaneous pile
#		      /libexec			# programs invoked by other programs
#  	              /share/info		# info files
#  	              /share/man		# manual entries
#		      /share/scheme		# Scheme libraries
#
program-install-dir	:=	$(prefix)/bin
cgi-install-dir		:=	$(prefix)/cgi
library-install-dir	:=	$(prefix)/lib
include-install-dir	:=	$(prefix)/include/$(thispath)
etc-install-dir		:=	$(prefix)/etc
libexec-install-dir	:=	$(prefix)/libexec
info-install-dir	:=	$(prefix)/share/info
man-install-dir		:=	$(prefix)/share/man
scm-install-dir		:=	$(prefix)/share/scheme

################################################################
# CFLAGS
# 
################################################################

# $(CFLAGS)
#
# Extra compiler flags can be specified to make in the usual way, e.g.:
#
#	make CFLAGS=-O
#	
# The default value of CFLAGS is "-g"
#
# Makefile.in should never define $(CFLAGS).  A particular Makefile.in can
# add extra C flags by defining $(EXTRA_CFLAGS).
#
ifdef CFLAGS
USER_CFLAGS	:=	$(CFLAGS)
else
USER_CFLAGS	:=	-g
endif

override CFLAGS	:=	 -I$(objroot)/config-include -I$(objroot) -I$(srcroot) $(USER_CFLAGS) $(EXTRA_CFLAGS)



################################################################
# Tools
#
################################################################

ifdef cfg__cc
  CC	:=	$(cfg__cc)
endif


################################################################
# Automatic Dependency Tracking
# 
# For every .c file, produce a .d file which states dependencies
# of the corresponding .o file and executable upon include files.
# 
################################################################

%.d: %.c $(generated-includes)
	printf "%s %s.o %s.d: " $(basename $@) $(basename $@) $(basename $@) > $@
	$(CC) -DFOR_MAKEFILE_DEPENDENCIES -E $(CFLAGS) $< | sed -f $(makefiles)/cpp-to-includes.sed | sort -u | sed -e 's/$$/ \\/' >> $@
	echo >> $@

$(addsuffix .o, $(basename $(source-files))): %.o: %.d
$(basename $(source-files)): %: %.d

ifndef clean-only
ifdef source-files
-include $(addsuffix .d, $(notdir $(basename $(source-files))))
endif
endif


################################################################
# Automatically Generated Source and Include Files
# 
################################################################

ifndef clean-only
ifdef generated-includes
all:	$(generated-includes)
endif

ifdef generated-source
all:	$(generated-source)
endif
endif

################################################################
# Documenation Rules
# 
# A temporary solution.
# 
################################################################


.SUFFIXES:	.pdml .pdml-index .html .doc

xdmlcomp	:=	$(objroot)/systas/systas/systas $(srcroot)/systas/scheme-library/doc/xdml-comp.ss

%.pdml : %.c %.d
	$(xdmlcomp) --reader c-comment --markup-language doc/old-pdml --print $< > $@

%.pdml : %.doc
	$(xdmlcomp) --reader c-comment --markup-language doc/old-pdml --print $< > $@

%.html : %.pdml $(the-pdml-index)
	$(xdmlcomp) --markup-language doc/old-pdml --output-format html $(foreach index, $(the-pdml-index), --use-index $(index)) --format $<

%.texi : %.pdml $(the-pdml-index)
	$(xdmlcomp) --markup-language doc/old-pdml --output-format texinfo $(foreach index, $(the-pdml-index), --use-index $(index)) --format $<





endif  # ifndef rules-mk
# tag: Tom Lord Tue Dec  4 14:47:18 2001 (rules.mk)
#

