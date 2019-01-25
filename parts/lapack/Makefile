# driver Makefile for lapack libraries
.PHONY: all clean config

include ../../configs/$(TARGET)

ifeq ($(LAPACK.build),yes)
all: config
	[ -d "$(LAPACK.dest)"/lib ] || mkdir -p "$(LAPACK.dest)"/lib
	make -j $(XJOBS) -C tree home="$$(pwd)" prefix="$(LAPACK.dest)" clean
	make -j $(XJOBS) -C tree home="$$(pwd)" prefix="$(LAPACK.dest)" lapacklib
	cp tree/make.inc "$(LAPACK.dest)"/lapack.make.inc
	cp tree/build-lib/liblapack.a "$(LAPACK.dest)"/lib/.

config:
	echo "SHELL       = $(LAPACK.shell)"           > tree/make.inc
	echo "FORTRAN     = $(LAPACK.fortran)"        >> tree/make.inc
	echo "OPTS        = $(LAPACK.opts)"           >> tree/make.inc
	echo "DRVOPTS     = $(LAPACK.drvopts)"        >> tree/make.inc
	echo "NOOPT       = $(LAPACK.noopts)"         >> tree/make.inc
	echo "LOADER      = $(LAPACK.loader)"         >> tree/make.inc
	echo "LOADOPTS    = $(LAPACK.loadopts)"       >> tree/make.inc
	echo "TIMER       = $(LAPACK.timer)"          >> tree/make.inc
	echo "CC          = $(LAPACK.cc)"             >> tree/make.inc
	echo "CFLAGS      = $(LAPACK.cflags)"         >> tree/make.inc
	echo "ARCH        = $(LAPACK.arch)"           >> tree/make.inc
	echo "ARCHFLAGS   = $(LAPACK.archflags)"      >> tree/make.inc
	echo "RANLIB      = $(LAPACK.ranlib)"         >> tree/make.inc
	echo "BLASLIB     = build-lib/librefblas.a"   >> tree/make.inc
	echo "LAPACKLIB   = build-lib/liblapack.a"    >> tree/make.inc
	echo "TMGLIB      = build-lib/libtmglib.a"    >> tree/make.inc
	echo "LAPACKELIB  = build-lib/liblapacke.a"   >> tree/make.inc
	[ -d tree/build-lib ] || mkdir -p tree/build-lib

clean:
	make -C tree clean
else
all:
	@echo Skipping LAPACK build because LAPACK.build is "$(LAPACK.build)"
clean:

config:

endif
