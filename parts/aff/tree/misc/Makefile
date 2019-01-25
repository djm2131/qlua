XMLCFLAGS=$(shell xml2-config --cflags)
XMLLIBS=$(shell xml2-config --libs)

CC=gcc
CFLAGS=-g -Wall $(XMLCFLAGS)
CLIBS=$(XMLLIBS)

.PHONY: all clean realclean

all: grok-xml

grok-xml: grok-xml.c
	$(CC) $(CFLAGS) -o $@ $< $(CLIBS)

clean:
	$(RM) *.o

realclean: clean
	$(RM) grok-xml
