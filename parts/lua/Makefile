# driver Makefile for lua library
.PHONY: all clean

include ../../configs/$(TARGET)

mkARGS= CC="$(LUA.cc)" CFLAGS="$(LUA.cflags)" AR="$(LUA.ar)" RANLIB="$(LUA.ranlib)"

ifeq ($(LUA.build),yes)
all:
	make -C tree $(mkARGS) generic
	make -C tree $(mkARGS) install INSTALL_TOP=$(LUA.dest)

clean:
	make -C tree clean
else
all:
	@echo Skipping LUA build because LUA.build is "$(LUA.build)"
clean:

endif
