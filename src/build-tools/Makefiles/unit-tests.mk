# unit-tests.mk: build test programs and run them for `make test'
#
################################################################
# Copyright (C) 2001, 2002 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 



ifndef unit-tests-mk
unit-tests-mk		:= 1

# Build and run unit tests.
#
# Optionally set on entry:
#
#	$(mains)	a list of source files defining "main"
#			(default "$(notdir $(wildcard $(srcdir)/unit-*.c) $(wildcard $(srcdir)/test-*.c))")
#
#	$(libs)		additional libraries to link against
#			(no default)
#
#	$(test-scripts) a list of shell scripts that perform tests
#			(default "$(wildcard $(srcdir)/*.sh)")
#
#	$(test-headers) a list of headers that should compile in isolation
#			(default none)
# 
# If $(test-headers) is not defined, it can be derived from two other
# optional variables:
# 
# 	$(tested-heades) a list of headers to test, relative to some
#			 some root directory
# 
# 	$(tested-prefix) the root directory for $(tested-headers)
# 
# For each tested header, "foo.h", `make all' compiles "hdr-foo.c" which
# does nothing but include "foo.h".
# 
# Derived:
#
#	$(test-progs)		:=	$(patsubst %.c, %, $(mains))
#
#
# Target `test' depends on `tests-timestamp'.
#
# `tests-timestamp' depends on `test-progs' and `test-scripts'.
# The action is to run all scripts.  If they all exit with status 0,
# then `touch tests-timestamp'.
# 
# After "make all", the build directory will contain "Programs" which
# contains the names of the programs that were built.  (See also
# "programs.mk")
# 


ifndef mains
mains			:=	$(notdir $(wildcard $(srcdir)/unit-*.c) $(wildcard $(srcdir)/test-*.c))
endif

include $(makefiles)/programs.mk

ifndef test-progs
test-progs		:=	$(patsubst %.c, %, $(mains))
endif

ifndef test-scripts
test-scripts		:=	$(wildcard $(srcdir)/*.sh)
endif

ifndef test-headers
ifdef tested-headers
ifdef tested-prefix
test-headers		:=	$(addprefix $(tested-prefix)/, $(tested-headers))
else
test-headers		:=	$(tested-headers)
endif
endif
endif

header-prog		=	$(addprefix hdr-, $(patsubst %.h, %.c, $(notdir $(1))))
header-obj		=	$(addprefix hdr-, $(patsubst %.h, %.o, $(notdir $(1))))
prog-header		= 	$(filter %/$(patsubst hdr-%.c,%.h,$(1)), $(test-headers))

header-objs		:=	$(call header-obj, $(test-headers))
header-progs		:=	$(call header-prog, $(test-headers))
header-deps		:=	$(patsubst %.c, %.d, $(header-progs))



################################################################
# header file compilation tests (at "make all")
#
################################################################

all: $(header-objs)

$(header-progs):
	echo "#include \"$(call prog-header, $@)\"" > $@



ifndef clean-only
ifneq "$(header-deps)" ""
-include $(header-deps)
endif
endif


################################################################
# test script execution (at "make test")
# 
################################################################

test: tests-timestamp

ifeq ($(test-scripts),)

tests-timestamp: $(test-progs)

else

tests-timestamp: $(test-progs) $(test-scripts)
	set -e ; \
	for s in $(test-scripts) ; do \
	  sh $$s; \
	done
	touch tests-timestamp

endif


################################################################
# clean target
# 
################################################################

clean: clean-unit-tests

clean-unit-tests:
	-rm -f tests-timestamp
	-rm -f $(header-progs) $(header-objs)


endif

# tag: Tom Lord Tue Dec  4 14:47:19 2001 (unit-tests.mk)
#
