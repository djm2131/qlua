# driver Makefile for arpack libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

install="$(ARPACK.dest)"

ifeq ($(ARPACK.build),yes)
all: config
	[ -d $(install)/lib ] || mkdir -p $(install)/lib
	make -j $(XJOBS) -C tree home="$$(pwd)/tree" prefix="$(install)" clean
	make -C tree home="$$(pwd)/tree" prefix="$(install)" lib plib
	cp tree/ARmake.config $(install)/.

clean:
	make -C tree home="$$(pwd)/tree" clean
	$(RM) tree/ARmake.config

config:
	echo "FC      = ${ARPACK.fc}"          > tree/ARmake.config
	echo "FFLAGS  = ${ARPACK.fflags}"     >> tree/ARmake.config
	echo "PFC     = ${ARPACK.pfc}"        >> tree/ARmake.config
	echo "PFFLAGS = ${ARPACK.pfflags}"    >> tree/ARmake.config
	echo "MPILIBS = ${ARPACK.mpilibs}"    >> tree/ARmake.config

else
all:
	@echo Skipping ARPACK build because ARPACK.build is "$(ARPACK.build)"
clean:

config:

endif
