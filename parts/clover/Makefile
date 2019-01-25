# driver Makefile for CLOVER library
.PHONY: all clean config gen-srcs

include ../../configs/$(TARGET)

xqa0 = ../qa0/tree/bootstrap/qa0

ifeq ($(CLOVER.build),yes)
all: gen-srcs
	make -j $(XJOBS) -C tree
	make -C tree install

clean:
	make -C tree realclean
	make -C tree/qa0 realclean
	rm -f tree/configure

gen-srcs: config
	make -j $(XJOBS) -C tree/qa0 NC="$(CLOVER.nc)" QA0="$$(pwd)/$(xqa0)" TARGETS="$(CLOVER.target)"

config:
	cd tree; utils/setup-nc $(CLOVER.nc)
	cd tree; CC="$(CLOVER.cc)" CFLAGS="$(CLOVER.cflags)" ./configure $(CLOVER.config)
else
all:
	@echo Skipping CLOVER build because CLOVER.build is "$(CLOVER.build)"
clean:

config:

endif
