# driver Makefile for the QUDA library
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(QUDA.build),yes)
all: config
	cd build; cmake ../tree $(QUDA.cmake)
	make -j $(XJOBS) -C build
	mkdir -p $(QUDA.dest:%=%/lib)
	mkdir -p $(QUDA.dest:%=%/include)
	[ -f build/lib/libquda.a ] && cp build/lib/libquda.a $(QUDA.dest:%=%/lib/.)
	[ -d build/include ] && cp -r build/include/*  $(QUDA.dest:%=%/include/.)

config:
	rm -fr build
	mkdir build

clean:
	rm -fr build

else
all:
	@echo Skipping QUDA build because QUDA.build is "$(QUDA.build)"
clean:

config:

endif
