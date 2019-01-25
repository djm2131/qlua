# driver Makefile for Qlua
.PHONY: all clean

include ../../configs/$(TARGET)

install=$(PREFIX)/qlua

all: config
	make -j $(XJOBS) -C tree
	make -C tree install
	cp -r tree/lhpc $(install)/lib/qlua/.

clean:
	make -C tree realclean

config:
	cd tree; \
	CC="$(QLUA.cc)" \
	LD="$(QLUA.ld)" \
	CFLAGS="$(QLUA.cflags) -DQDP_Nc=3" \
        COPTS="$(QLUA.copts)" \
	LIBS="$(QLUA.libs:%=-l%)" \
	./configure $(QLUA.config)
