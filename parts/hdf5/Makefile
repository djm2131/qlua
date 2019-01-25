# driver Makefile for HDF5 libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(HDF5.build),yes)
all: config
	make -j $(XJOBS) -C build 
	make -C build install

clean:
	rm -fr build

config:
	rm -fr build
	mkdir build
	cd build; ../tree/configure $(HDF5.config)
else
all:
	@echo Skipping HDF5 build because HDF5.build is "$(HDF5.build)"
clean:

config:

endif
