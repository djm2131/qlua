# driver Makefile for QDP libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QDP.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -fr build
	rm -f tree/configure

config: tree/configure
	rm -fr build
	mkdir build
	cd build; ../tree/configure $(QDP.config)

tree/configure:
	cd tree; autoreconf
else
all:
	@echo Skipping QDP build because QDP.build is "$(QDP.build)"
clean:

config:

endif
