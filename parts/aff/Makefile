# driver Makefile for aff library
.PHONY: all clean

include ../../configs/$(TARGET)

install="$(AFF.dest)"

ifeq ($(AFF.build),yes)
all:
	make -C tree \
            CC="$(AFF.cc)" \
            CXX="$(AFF.cxx)" \
            CFLAGS="$(AFF.cflags)" \
            CXXFLAGS="$(AFF.cxxflags)" \
            AR="$(AFF.ar)" \
            RANLIB="$(AFF.ranlib)" \
            CONFIG="$(AFF.config)" \
            prefix=$(install) \
            install

clean:
	make -C tree clean
else
all:
	@echo Skipping AFF build because AFF.build is "$(AFF.build)"
clean:


endif
