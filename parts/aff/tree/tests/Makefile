# This is a makefile for tests, it does not need to be converted for configure
XMLCONFIG = xml2-config
XMLCFLAGS = $(shell $(XMLCONFIG) --cflags)
XMLLIBS   = $(shell $(XMLCONFIG) --libs)
CC        = gcc -std=c99
CFLAGS    = -Wall -g -I../lib $(XMLCFLAGS)
LIBS      = -L../lib -llhpc-aff $(XMLLIBS)

tests = test-stable.c \
        test-treap.c \
        test-md5.c \
        test-waff.c \
        test-double.c \
        write-aff.c \
        read-aff.c

.PHONY: all clean realclean lib

all: $(tests:%.c=%)

clean:
	$(RM) *.o

realclean: clean
	$(RM) $(tests:%.c=%)

$(tests:%.c=%): %: %.c
	$(CC) $(CFLAGS) -o $@ $< $(LIBS)

$(tests:%.c=%): lib

lib:
	make -C ../lib
