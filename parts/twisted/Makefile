# driver Makefile for TWISTED library
.PHONY: all clean config gen-srcs

include ../../configs/$(TARGET)

xqa0 = ../qa0/tree/bootstrap/qa0

ifeq ($(TWISTED.build), yes)
all: gen-srcs
	make -j $(XJOBS) -C tree
	make -C tree install

clean:
	make -C tree realclean
	make -C tree/qa0 realclean
	rm -f tree/configure

gen-srcs: config
	make -j $(XJOBS) -C tree/qa0 NC="$(TWISTED.nc)" QA0="$$(pwd)/$(xqa0)" TARGETS="$(TWISTED.target)"

config:
	cd tree; utils/setup-nc $(TWISTED.nc)
	cd tree; CC="$(TWISTED.cc)" CFLAGS="$(TWISTED.cflags)" ./configure $(TWISTED.config)
else
all:
	@echo Skipping TWISTED build because TWISTED.build = "$(TWISTED.build)"
clean:

config:

gen-srcs:

endif