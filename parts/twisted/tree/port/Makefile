#
CONFIG     = macosx
LIBRARY    = libqop-twisted.a
LIMPDIR    = .

QMP_CFLAGS = $(shell $(QMP_TOP:%=%/bin/qmp-config) --cflags)
lib        = $(LIMPDIR)/$(LIBRARY)
HDR        = qop-twisted.h

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
            init.c \
            fini.c \
            alloc-aligned.c \
            alloc.c \
            dealloc.c \
            comm-setup.c \
            comm-free.c \
            perf.c \
            cg-mixed.c \
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
            hfermion-size.c \
            hfermion-mat-alloc.c \
            hfermion-mat-free.c \
            poly-normalize.c \
            solver-dirac-mixed.c \
            solver-dirac-deflated-mixed.c \
            gauge-d2f.c \

p.sources = fermion-dot.c \
            fermion-norm2.c \
            cg-dirac-error.c \
            cg-inflate.c \
            cg-precondition.c \
            cg-solver.c \
            cg-log.c \
            fermion-madd.c \
            fermion-alloc.c \
            fermion-free.c \
            fermion-import.c \
            fermion-export.c \
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
            op-print.c \
            step-even.c \
            step-odd.c \
            x-import.c \
            x-export.c \
            boundary-n.c \
            boundary-x.c \
            op-m.c \
            op-mn.c \
            op-mx.c \
            solver-dirac.c \
            solver-mxm.c \
            op-dirac.c \
            op-dirac-conj.c \
            op-precond.c \
            op-precond-conj.c \
            op-boundary.c \
            op-cmb.c \
            op-apb.c \
            op-axpbx.c \
            op-mxm-poly.c \
            op-a1b.c \
            op-a1xbx.c \
            op-amb.c \
            op-axmbx.c \
            op-ambn.c \
            op-a.c \
            gauge-import.c \
            gauge-free.c \

#            inv-twisted-export.c \

x.objects = $(x.sources:%.c=$(LIMPDIR)/%.o)

p.objects = $(p.sources:%.c=$(LIMPDIR)/%f.o) \
            $(p.sources:%.c=$(LIMPDIR)/%d.o)

objects = $(x.objects) $(p.objects)

headers = twisted.h \
          twisted-x.h \
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
             -DQOP_TWISTED_DEFAULT_PRECISION=\'F\' -c -o $@ $<

$(p.sources:%.c=$(LIMPDIR)/%d.o): $(LIMPDIR)/%d.o: %.c
	$E CC-d $<
	$C$(CC) $(COPTS) $(CFLAGS) -I. \
             -DQOP_TWISTED_DEFAULT_PRECISION=\'D\' -c -o $@ $<

$(objects): $(headers)
