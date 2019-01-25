# driver Makefile for QLA libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QLA.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -f tree/configure
	rm -fr build

config: tree/configure
	rm -fr build
	mkdir build
	cd build; ../tree/configure $(QLA.config)

tree/configure:
	-cd tree; automake; autoconf; autoreconf
else
all:
	@echo Skipping QLA build because QLA.build is "$(QLA.build)"
clean:

config:

endif
