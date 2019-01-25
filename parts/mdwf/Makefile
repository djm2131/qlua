# driver Makefile for MDWF library
.PHONY: all clean config gen-srcs

include ../../configs/$(TARGET)

xqa0 = ../qa0/tree/bootstrap/qa0

ifeq ($(MDWF.build),yes)
all: gen-srcs
	make -j $(XJOBS) -C tree
	make -C tree install

clean:
	make -C tree clean
	make -C tree/qa0 realclean

gen-srcs: config
	make -j $(XJOBS) -C tree/qa0 QA0="$$(pwd)/$(xqa0)" TARGETS="$(MDWF.target)"

config:
	cd tree; CC="$(MDWF.cc)" CFLAGS="$(MDWF.cflags)" ./configure $(MDWF.config)
	cp tree/nc/qop-mdwf3.h tree/port/.
else
all:
	@echo Skipping MDWF build because MDWF.build is "$(MDWF.build)"
clean:

config:

endif
