# driver Makefile for SFC
.PHONY: all clean

include ../../configs/$(TARGET)

ifeq ($(SFC.build),yes)
all:
	$(SFC.cc) -o sfc tree/fixpoint/*.c

clean:
	$(RM) sfc
else
all:
	@echo Skipping SFC build because SFC.build is "$(SFC.build)"
clean:

config:

endif
