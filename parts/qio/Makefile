# driver Makefile for QIO libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QIO.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -fr build
	rm -f tree/configure

config: tree/configure
	rm -fr build
	mkdir build
	cd build; ../tree/configure $(QIO.config)

tree/configure:
	cd tree; autoreconf
	cd tree/other_libs/c-lime; autoreconf
else
all:
	@echo Skipping QIO build because QIO.build is "$(QIO.build)"
clean:

config:

endif
