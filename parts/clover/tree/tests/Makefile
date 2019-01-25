TOP = ..
V = 0
CONFIG = ../target
library = qop-clover
CDEBUG = -g
# PREC = D

TCFLAGS  = $(CFLAGS) -I../port -g
TLDFLAGS = $(LDFLAGS)
TCLIBS   = $(LIBS)

include $(TOP)/config/$(CONFIG)

.PHONY: all clean realclean thelib dist

ifeq ("$V", "0")
   E=@echo "  "
   C=@
else
   E=@echo > /dev/null " "
   C=
endif

programs = get-gauge \
           check-gamma \
           check-clover \
           check-dxd \
           check-cg \
           check-vfsize \
           check-vgp \
           check-vf-dot-f \
           check-vf-dot-vf \
           check-vf-dot-zv \
           check-vf-dot-zm

programs.f = $(programs:%=%F)
programs.d = $(programs:%=%D)

programs.all = $(programs.f) $(programs.d) check_mcg

utils = gauge.c \
        fermion.c \
        clover.c \
        vfermion.c

headers = clover-test.h \
          vfermion-test.h \
          $(TOP)/$(LIMPDIR)/qop-clover.h

sources = $(programs:%=%.c) $(utils)

objects.f = $(sources:%.c=%F.o)
objects.d = $(sources:%.c=%D.o)

###############################################################################
all: $(programs.all)

clean:
	$E RM objects
	$C$(RM) $(objects.d) $(objects.f)

realclean: clean
	$E RM programs
	$C$(RM) $(programs.all)

dist: realclean

thelib:
	$(MAKE) -C $(TOP)

###############################################################################

$(programs.all): thelib

$(programs.f):%: %.o
	$E LD $@
	$C$(CC) $(CDEBUG) $(TLDFLAGS)  \
               -DQOP_CLOVER_DEFAULT_PRECISION=\'F\' \
               -L$(TOP)/$(LIMPDIR)  -o $@ $< $(utils:%.c=%F.o) \
               -l$(library) $(TCLIBS)

$(programs.d):%: %.o
	$E LD $@
	$C$(CC) $(CDEBUG) $(TLDFLAGS)  \
               -DQOP_CLOVER_DEFAULT_PRECISION=\'D\' \
               -L$(TOP)/$(LIMPDIR)  -o $@ $< $(utils:%.c=%D.o) \
               -l$(library) $(TCLIBS)

check-mcg: check-mcg.o $(utils:%.c=%D.o)
	$E LD $@
	$C$(CC) $(CDEBUG) $(TLDFLAGS)  \
               -L$(TOP)/$(LIMPDIR)  -o $@ $<  $(utils:%.c=%D.o) \
               -l$(library) $(TCLIBS)

check-mcg.o: check-mcg.c
	$E CC $@
	$C$(CC) $(TCFLAGS) -c -o $@ $<

$(programs.f): $(utils:%.c=%F.o)
$(programs.d): $(utils:%.c=%D.o)

$(objects.f): %F.o: %.c
	$E CC $@
	$C$(CC) $(CDEBUG) -DQOP_CLOVER_DEFAULT_PRECISION=\'F\' \
               $(TCFLAGS) -c -o $@ $<

$(objects.d): %D.o: %.c
	$E CC $@
	$C$(CC) $(CDEBUG) -DQOP_CLOVER_DEFAULT_PRECISION=\'D\' \
               $(TCFLAGS) -c -o $@ $<

$(objects.f): $(headers)
$(objects.d): $(headers)

$(programs.all): $(TOP)/$(LIMPDIR)/lib$(library).a
