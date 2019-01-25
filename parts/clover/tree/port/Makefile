#
CONFIG     = macosx
LIBRARY    = libqop-clover.a
LIMPDIR    = .

QMP_CFLAGS = $(shell $(QMP_TOP:%=%/bin/qmp-config) --cflags)
lib        = $(LIMPDIR)/$(LIBRARY)
HDR        = qop-clover.h

include ../config/$(CONFIG)

V          = 0

ifeq ("$V", "0")
   E=@echo "  "
   C=@
else
   E=@echo > /dev/null
   C=
endif

.PHONY: all clean realclean library dist

x.sources = version.c \
            error.c \
            perf.c \
            gauge-d2f.c \
            alloc-aligned.c \
            alloc.c \
            dealloc.c \
            init.c \
            fini.c \
            comm-setup.c \
            comm-free.c \
            deflator-alloc.c \
            deflator-create-from-mat.c \
            deflator-free.c \
            deflator-la-x.c \
            deflator-uspace.c \
            deflator-preamble.c \
            deflator-postamble.c \
            deflator-update0.c \
            deflator-update1.c \
            deflator-ctrl.c \
            deflator-extract-vector.c \
            deflator-add-vector.c \
            poly-normalize.c \
            hfermion-size.c \
            cg-mixed.c \
            solver-dirac-mixed.c \
            solver-dirac-deflated-mixed.c \
            hfermion-mat-alloc.c \
            hfermion-mat-free.c

p.sources = fermion-dot.c \
            fermion-madd.c \
            fermion-norm2.c \
            fermion-alloc.c \
            fermion-free.c \
            fermion-import.c \
            fermion-export.c \
            inv-clover-export.c \
            hfermion-dot.c \
            hfermion-madd.c \
            hfermion-norm2.c  \
            hfermion-alloc.c \
            hfermion-free.c \
            hfermion-import.c \
            hfermion-import-blas.c \
            hfermion-export.c \
            hfermion-export-blas.c \
            alloc-eo.c \
            x-import.c \
            x-export.c \
            gauge-import.c \
            gauge-free.c \
            op-dirac.c \
            op-dirac-conj.c \
            op-precond.c \
            op-precond-conj.c \
            op-apb.c \
            op-axpbx.c \
            op-boundary.c \
            boundary-n.c \
            boundary-x.c \
            solver-dirac.c \
            solver-mxm.c \
            op-mxm-poly.c \
            op-print.c \
            cg-dirac-error.c \
            cg-inflate.c \
	    cg-precondition.c \
            cg-solver.c \
            cg-log.c \
            op-m.c \
            op-mn.c \
            op-mx.c \
            op-a1b.c \
            op-a1xbx.c \
            op-amb.c \
            op-axmbx.c \
            op-ambn.c \
            op-cmb.c \
            op-a.c \
            step-even.c \
            step-odd.c 

x.objects = $(x.sources:%.c=$(LIMPDIR)/%.o)

p.objects = $(p.sources:%.c=$(LIMPDIR)/%f.o) \
            $(p.sources:%.c=$(LIMPDIR)/%d.o)

objects = $(x.objects) $(p.objects)

headers = clover.h clover-x.h \
          deflator-x.h deflator-la-x.h \
          $(HDR)


all: $(lib)
ifeq ("$(LIMPDIR)", ".")
	$E $(HDR) in place
else
	$E CP $(HDR)
	$C cp $(HDR) $(LIMPDIR)/
endif

clean:
	$E RM objects
	$C$(RM) $(objects)

realclean: clean
ifneq ("$(LIMPDIR)", ".")
	$C$(RM) $(LIMPDIR)/$(HDR)
endif


dist: clean

$(lib): library

library: $(objects)
	$E AR $(lib)
	$C$(AR) cr $(lib) $^
	$C$(RANLIB) $(lib)

$(x.objects): $(LIMPDIR)/%.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -I. -c -o $@ $<

$(p.sources:%.c=$(LIMPDIR)/%f.o): $(LIMPDIR)/%f.o: %.c
	$E CC-f $<
	$C$(CC) $(COPTS) $(CFLAGS) -I. \
             -DQOP_CLOVER_DEFAULT_PRECISION=\'F\' -c -o $@ $<

$(p.sources:%.c=$(LIMPDIR)/%d.o): $(LIMPDIR)/%d.o: %.c
	$E CC-d $<
	$C$(CC) $(COPTS) $(CFLAGS) -I. \
             -DQOP_CLOVER_DEFAULT_PRECISION=\'D\' -c -o $@ $<

$(objects): $(headers)
