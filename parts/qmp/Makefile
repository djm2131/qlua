# driver Makefile for QMP libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QMP.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -fr build

config:
	rm -fr build
	mkdir build
	cd tree; autoreconf; autoconf
	cd build; ../tree/configure $(QMP.config)
else
all:
	@echo Skipping QMP build because QMP.build is "$(QMP.build)"
clean:

config:

endif
