# driver Makefile for gsl libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(GSL.build),yes)
all: config
	make -j $(XJOBS) -C tree 
	make -C tree install

clean:
	make -C tree clean

config:
	cd tree; ./configure $(GSL.config)
else
all:
	@echo Skipping GSL build because GSL.build is "$(GSL.build)"
clean:

config:

endif
