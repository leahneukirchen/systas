# programs.mk: compile programs
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef programs-mk
programs-mk	:= 1

# Build programs.
#
# Set on entry:	(see also "library.mk")
#
#	$(mains)	a list of source files defining "main"
#
# Optionally set on entry:
#
#	$(libs)		additional libraries to link against
#
# One program is built for each file in $(mains).  A library is built
# from all of the other source files in this directory and the programs
# are linked against that library.
#
# After "make all", the build directory will contain "Programs" which
# contains the names of the programs that were built.
#

include $(makefiles)/library.mk

programs	:=	$(patsubst %.c, %, $(mains))
progdepfiles	:=	$(patsubst %.c, %.d, $(mains))

all: $(programs) Programs

ifndef no-Programs-rule
Programs: $(programs)
	echo $(programs) | sed -f $(makefiles)/column.sed > Programs
endif

$(programs):%:%.o $(thelib) $(filter-out -l%, $(libs))
	$(CC) $(CFLAGS) -o $@ $< $(thelib) $(libs)

clean: clean-prog

clean-prog:
	-rm -f $(programs) $(progdepfiles) $(patsubst %.c, %.o, $(mains)) Programs

endif

# tag: Tom Lord Tue Dec  4 14:47:18 2001 (programs.mk)
#
