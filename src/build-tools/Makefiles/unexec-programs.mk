# unexec-programs.mk: compile programs that use unexec
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef unexec-programs-mk
unexec-programs-mk	:= 1

# Build programs which might call unexec.
#
# This is a variant on `programs.mk' and the two might eventually
# be merged.
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

ifndef cfg__unexec_linker

ifdef cfg__gcc_version
  cfg__unexec_linker	:=	$(CC) $(CFLAGS) -nostdlib
  cfg__gnulib_var	:=	$(shell $(CC) -print-libgcc-file-name)
else
  cfg__unexec_linker	:=	$(LD)
  cfg__gnulib_var	:=  
endif

endif


ifeq ($(cfg__unexec_needs_precrt0),1)

	maybe_precrt0 	:=	,precrt0.o

,precrt0.o:
	echo "int data_start = 0;" > ,precrt0.c
	$(CC) -g -c -o ,precrt0.o ,precrt0.c

endif



$(programs):%:%.o $(thelib) $(filter-out -l%, $(libs)) $(maybe_precrt0)
	$(cfg__unexec_linker) $(cfg__unexec_ld_switches) \
		-o $@ \
		$(maybe_precrt0) \
		$(cfg__unexec_start_files) \
		$< \
		$(thelib) \
		$(libs) \
		$(cfg__unexec_libs) \
		-lm \
		$(cfg__unexec_libc_substitute)

clean: clean-prog

clean-prog:
	-rm -f $(programs) $(progdepfiles) $(patsubst %.c, %.o, $(mains)) Programs

endif

# tag: Tom Lord Fri May 10 02:15:12 2002 (Makefiles/unexec-programs.mk)
#
