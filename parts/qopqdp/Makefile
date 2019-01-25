# driver Makefile for QOPQDP libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QOPQDP.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -fr build
	rm -f tree/configure

config: tree/configure
	rm -fr build
	mkdir build
	cd build; ../tree/configure $(QOPQDP.config)

tree/configure:
	cd tree; autoreconf
else
all:
	@echo Skipping QOPQDP build because QOPQDP.build is "$(QOPQDP.build)"
clean:

config:

endif
