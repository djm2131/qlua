# common makefile for standard targets
TOP        = ..
V          = 0
LIBRARY    = libqop-mdwf3.a
QMP_CFLAGS = $(shell $(QMP_TOP:%=%/bin/qmp-config) --cflags)

include $(TOP)/config/$(CONFIG)

.PHONY: all clean realclean dist

ifeq ("$V", "0")
   E=@echo "  "
   C=@
else
   E=@echo > /dev/null
   C=
endif

headers = ../port/mdwf.h

i.sources = put-ab.c \
            put-abi.c \
            put-abi-z.c \
            put-c-ab.c \
            put-c-abi.c \
            put-c-abi-z.c \
            put-down.c \
            get-down.c \
            get-down-f.c \
            put-up.c \
            get-up.c \
            get-up-f.c \
            put-neighbor.c \
            get-neighbor.c \
            put-k.c \
            put-ki.c \
            put-c-k.c \
            put-c-ki.c \
            fix-down.c \
            fix-up.c \
            f-f-eq-d-minus-d.c \
            f-d-plus-eq-f.c \
            g-f-eq-d.c \
            sizeof-ab-table.c \
            sizeof-abi-table.c \
            sizeof-c-ab-table.c \
            sizeof-c-abi-table.c \
            sizeof-k-table.c \
            sizeof-ki-table.c \
            sizeof-c-k-table.c \
            sizeof-c-ki-table.c \
            sizeof-down-pack.c \
            sizeof-neighbor.c \
            sizeof-up-pack.c

x.sources = get-fermion \
            put-fermion \
            blas2fermion \
            fermion2blas \
            put-gauge \
            sizeof-fermion \
            sizeof-gauge \
            sizeof-pfermion \
            sizeof-vfermion \
            strideof-vfermion \
            fv-zero \
            fv-copy \
            fv-get \
            fv-put \
            f-zero \
            f-copy \
            f-norm \
            f-rmul1 \
            f-add3 \
            f-add2 \
            f-cadd2 \
            f-add2-norm \
            f-add2x \
            f-dot \
            f-diff-norm \
            vf-put \
            vf-get \
            vf-copy \
            do-vfH-dot-f \
            do-vfH-dot-vf \
            vf-dot-vz \
            vf-dot-mz \
            cg-xp \
            scg-madd \
            scg-xp \
            proj-minus \
            proj-plus \
            proj-u-minus \
            proj-u-plus \
            do-axial-current \
            do-A \
            do-A-conj \
            do-A-inv \
            do-A-inv-conj \
            do-F \
            do-F-conj \
            do-A-plus-F \
            do-A-plus-F-norm \
            do-B-A-inv \
            do-B-A-inv-F \
            do-1-sub-B-A-inv-F \
            do-1-sub-F \
            do-1-sub-F-conj \
            do-1-sub-F-conj-norm \
            do-1-sub-B-A-inv-F-norm \
            do-A-conj-plus-B-conj-F-conj \
            do-A-inv-conj-B-conj \
            do-A-inv-conj-B-conj-F-conj \
            do-c-A \
            do-c-A-conj \
            do-c-A-plus-F \
            do-c-A-plus-F-norm \
            do-c-A-conj-plus-B-conj-F-conj \
            do-c-A-inv \
            do-c-A-inv-conj \
            do-c-A-inv-conj-B-conj \
            do-c-A-inv-conj-B-conj-F-conj \
            do-c-B-A-inv \
            do-c-B-A-inv-F \
            do-c-1-sub-B-A-inv-F \
            do-c-1-sub-B-A-inv-F-norm \
            do-K \
            do-B-A-inv-K \
            do-1-sub-K-inv-conj-A-inv-conj-plus-B-conj-F-conj \
            do-1-sub-K-inv-conj-A-inv-conj-plus-B-conj-F-conj-norm \
            do-1-sub-K-F \
            do-1-sub-K-F-norm \
            do-c-K \
            do-c-K-conj \
            do-c-B-A-inv-K \
            do-c-1-sub-K-inv-conj-A-inv-conj-plus-B-conj-F-conj \
            do-c-1-sub-K-inv-conj-A-inv-conj-plus-B-conj-F-conj-norm \
            do-c-1-sub-K-F \
            do-c-1-sub-K-F-norm \

sources = $(i.sources) \
          $(x.sources:%=%f.c) \
          $(x.sources:%=%d.c)


i.objects = $(i.sources:%.c=%.o)
f.objects = $(x.sources:%=%f.o)
d.objects = $(x.sources:%=%d.o)

objects   = $(i.objects) $(f.objects) $(d.objects)

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

#$(sources:%.c=%.o): %.o: %.c
#       $E CC $<
#       $C$(CC) $(COPTS) $(CFLAGS) -I../port -c -o $@ $<

$(i.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -I../port -c -o $@ $<

$(f.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -DQOP_MDWF_DEFAULT_PRECISION="'F'" -I../port -c -o $@ $<

$(d.objects): %.o: %.c
	$E CC $<
	$C$(CC) $(COPTS) $(CFLAGS) -DQOP_MDWF_DEFAULT_PRECISION="'D'" -I../port -c -o $@ $<
