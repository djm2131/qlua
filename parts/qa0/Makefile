# driver Makefile for qa0
.PHONY: all clean

include ../../configs/$(TARGET)

ifeq ($(QA0.build),yes)
all:
	make -C tree/scheme CC="$(QA0.cc)" SFC="$$(pwd)/../sfc/sfc" prefix="$$(pwd)" all
	make -C tree CC="$(QA0.cc)" SFC="$$(pwd)/../sfc/sfc" prefix="$$(pwd)"

clean:
	make -C tree realclean
else
all:
	@echo Skipping QA0 build because QA0.build is "$(QA0.build)"
clean:

config:

endif
