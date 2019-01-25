SFC    = sfc -v
CC     = gcc -O3 -Wall
prefix = $$HOME/Local

bindir = $(prefix)/bin
docdir = $(prefix)/share/doc/$(VERSION)

SUBDIRS = scheme bootstrap doc
VERSION = qa0-XXX
DOCS    =  doc/qa0.pdf README COPYRIGHT AUTHORS

.PHONY: all dist clean realclean install install-doc tar

all:
	$(MAKE) CC='$(CC)'  -C bootstrap $@

clean realclean:
	for d in $(SUBDIRS); do \
	  $(MAKE) -C $$d $@ || exit 1; \
	done

dist:
	$(MAKE) -C scheme all
	$(MAKE) -C doc all

install: all install-doc
	[ -d $(bindir) ] || mkdir -p $(bindir)
	rm -f $(bindir)/qa0
	cp bootstrap/qa0 $(bindir)/
	chmod -w $(bindir)/qa0

install-doc:
	[ -d $(docdir) ] || mkdir -p $(docdir)
	for f in $(DOCS); do \
	   rm -f $(docdir)/`basename $$f`; \
	   cp $$f $(docdir)/ ; \
	   chmod -w $(docdir)/`basename $$f`; \
	done

tar: realclean
	x=`pwd`; cd .. ; tar -cvf - `basename $$x` | bzip2 -9 > $$x.tar.bz2
