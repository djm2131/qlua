# common makefile for standard targets
TOP        = ..
V          = 0
LIBRARY    = libqop-twisted.a

include $(TOP)/config/$(CONFIG)

.PHONY: all clean realclean dist

ifeq ("$V", "0")
   E=@echo "  "
   C=@
else
   E=@echo > /dev/null
   C=
endif

headers = ../../port/twisted.h

i.sources = sizeof-down-pack.c \
            sizeof-neighbor.c \
            sizeof-up-pack.c \
            sizeof-twist.c \
            put-neighbor.c \
            get-neighbor.c \
            put-up.c \
            get-up.c \
            get-up-f.c \
            fix-up.c \
            put-down.c \
            get-down.c \
            fix-down.c \
            get-down-f.c \
            f-d-eq-d-plus-f.c \
            f-f-eq-d-minus-d.c \
            g-f-eq-d.c \
            put-twisted.c

x.sources = f-norm \
            f-dot \
            sizeof-fermion \
            sizeof-gauge \
            sizeof-vfermion \
            strideof-vfermion \
            blas2fermion \
            fermion2blas \
            f-zero \
            f-add3 \
            f-rmul \
            f-cadd2 \
            f-add2 \
            f-add2-norm \
            f-copy \
            f-diff-norm \
            fv-dot-vz \
            fv-dot-mz \
            proj-minus \
            proj-plus \
            proj-u-minus \
            proj-u-plus \
            do-vfH-dot-f \
            do-vfH-dot-vf \
            zero-vfermion \
            copy-vfermion \
            put-vfermion \
            get-vfermion \
            get-fermion \
            put-fermion \
            put-gauge \
            cg-xp \
            do-C-minus-B \
            do-A \
            do-A-times-B \
            do-A-conj-times-B-conj \
            do-A-plus-B \
            do-A-minus-B \
            do-A-minus-B-norm \
            do-A-conj-plus-B-conj \
            do-A-conj-minus-B-conj \

sources = $(i.sources) \
          $(x.sources:%=%f.c) \
          $(x.sources:%=%d.c)

i.objects = $(i.sources:%.c=%.o)
f.objects = $(x.sources:%=%f.o)
d.objects = $(x.sources:%=%d.o)

objects = $(i.objects) $(f.objects) $(d.objects)

all: $(objects)
	$E AR $@/$(LIMP)
	$C$(AR) cr $(LIBRARY) $^
	$C$(RANLIB) $(LIBRARY)
	$C$(MAKE) CONFIG='$(CONFIG)' V='$V' LIBRARY='$(LIBRARY)' \
		LIMPDIR=../$(LIMP) -C ../port $@

dist clean:
	$E RM $(LIMP)/objects
	$C$(RM) $(objects)
	$C$(MAKE) CONFIG='$(CONFIG)' V='$V' LIBRARY='$(LIBRARY)' \
		LIMPDIR=../$(LIMP) -C ../port $@

realclean: clean
	$E RM $(LIMP)/sources
	$C$(RM) $(sources)
	$C$(MAKE) CONFIG='$(CONFIG)' V='$V' LIBRARY='$(LIBRARY)' \
		LIMPDIR=../$(LIMP) -C ../port $@
	$C$(RM) $(LIBRARY)

$(i.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -I../port -c -o $@ $<

$(f.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -I../port -DQOP_TWISTED_DEFAULT_PRECISION="'F'" -c -o $@ $<

$(d.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -I../port -DQOP_TWISTED_DEFAULT_PRECISION="'D'" -c -o $@ $<
