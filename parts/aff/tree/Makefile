# This is a main Makefile for the AFF library and utilities.
#
CONFIG=none
subdirs=lib utils

prefix=/usr/local
prefix_lib=$(prefix)/lib
prefix_include=$(prefix)/include
prefix_bin=$(prefix)/bin
prefix_math=$(prefix)/math
prefix_doc=$(prefix)/doc
.PHONY: all clean realclean install do.install

ifeq "$(wildcard config/$(CONFIG))" ""
all install:
	@echo "configuration $(CONFIG) not found"
	@exit 1

export CONFIG
else
include config/$(CONFIG)

#you can transfer all the parameters to sub-make with preceding directive
# export VAR1 VAR2 ...
## Sergey Syritsyn 11/6/2007
export CONFIG CC CFLAGS AR RANLIB
all:
	for d in $(subdirs); do \
	  make CC='$(CC)' \
               CFLAGS='$(CFLAGS)' \
               AR='$(AR)' \
               RANLIB='$(RANLIB)' \
               -C $$d $@; \
	done
install: all do.install
endif

clean:
	for d in $(subdirs); do \
	  make -C $$d $@; \
	done

realclean:
	for d in $(subdirs); do \
	  make -C $$d $@; \
	done
	$(RM) lhpc-aff-config

do.install: lhpc-aff-config docs/aff_spec.pdf COPYRIGHT
	mkdir -p $(prefix_bin)
	$(RM) $(prefix_bin)/lhpc-aff-config
	cp lhpc-aff-config $(prefix_bin)/lhpc-aff-config
	chmod 555 $(prefix_bin)/lhpc-aff-config
	$(RM) $(prefix_bin)/lhpc-aff
	cp utils/lhpc-aff $(prefix_bin)/lhpc-aff
	chmod 555 $(prefix_bin)/lhpc-aff
	mkdir -p $(prefix_lib)
	$(RM) $(prefix_lib)/liblhpc-aff.a
	cp lib/liblhpc-aff.a $(prefix_lib)/liblhpc-aff.a
	chmod 444 $(prefix_lib)/liblhpc-aff.a
	mkdir -p $(prefix_include)
	$(RM) $(prefix_include)/lhpc-aff.h
	cp lib/lhpc-aff.h $(prefix_include)/lhpc-aff.h
	chmod 444 $(prefix_include)/lhpc-aff.h
	$(RM) $(prefix_include)/lhpc-aff++.h
	cp lib/lhpc-aff++.h $(prefix_include)/lhpc-aff++.h
	chmod 444 $(prefix_include)/lhpc-aff++.h
	mkdir -p $(prefix_doc)
	for f in docs/aff_spec.pdf COPYRIGHT; do \
	   bf=`basename $$f`; \
	   $(RM) $(prefix_doc)/$$bf; \
	   cp $$f $(prefix_doc)/$$bf; \
	   chmod 444 $(prefix_doc)/$$bf; \
	done
	mkdir -p $(prefix_math)
	cp math/aff.m $(prefix_math)/aff.m

.PHONY: lhpc-aff-config
lhpc-aff-config: Makefile config/$(CONFIG) aff-config.in
	sed -e 's?@CC@?$(install.CC)?' \
            -e 's?@CFLAGS@?$(install.CFLAGS)?' \
            -e 's?@LIBS@?$(install.LIBS)?' \
            -e 's?@LDFLAGS@?$(install.LDFLAGS)?' \
            -e 's?@prefix_include@?$(prefix_include)?' \
            -e 's?@prefix_lib@?$(prefix_lib)?' \
        < aff-config.in > lhpc-aff-config
