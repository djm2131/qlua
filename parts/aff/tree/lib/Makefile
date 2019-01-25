CONFIG	= config/none
TOP	= ..#
.PHONY: all clean realclean install


library = liblhpc-aff.a
interface = lhpc-aff.h

sources=$(treap.sources) \
        $(stable.sources) \
        $(tree.sources) \
        $(node.sources) \
        $(md5.sources) \
        $(coding.sources) \
        $(aff.sources) \
        $(io.sources)

treap.sources = treap-cmp.c \
                treap-insert.c	\
                treap-fini.c \
	        treap-init.c \
                treap-lookup.c \
                treap-print.c

treap.headers = treap-i.h \
                treap.h

stable.sources = stable-fini.c \
                 stable-index.c \
                 stable-print.c \
                 stable-foreach.c \
                 stable-init.c \
                 stable-insert.c \
                 stable-iblock.c \
                 stable-lookup.c \
                 stable-name.c \
                 stable-id.c \

stable.headers = stable-i.h \
                 stable.h

tree.sources = tree-fini.c \
               tree-foreach.c \
               tree-iblock.c \
               tree-index.c \
               tree-init.c \
               tree-insert.c \
               tree-lookup.c \
               tree-print.c \
               tree-root.c

tree.headers = tree-i.h \
               tree.h

node.sources = node-foreach.c \
               node-id.c \
               node-name.c \
               node-parent.c \
               node-type.c \
               node-size.c \
               node-offset.c \
               node-assign.c \
               node-chdir.c \
               node-cda.c \
               node-cdv.c \
               node-cd.c

node.headers = node-i.h \
               node.h

md5.sources = md5.c

md5.headers = md5.h

aff.sources = aff-signature.c \
              aff-version.c \
              aff-namecheck.c \
              aff-alloc.c \
              waff-init.c \
              waff-fini.c \
              waff-errstr.c \
              waff-clearerr.c \
              waff-root.c \
              waff-stable.c \
              waff-tree.c \
              waff-mkdir.c \
              waff-mkpath.c \
              waff-putchar.c \
              waff-putint.c \
              waff-putdouble.c \
              waff-putcomplex.c \
              raff-init.c \
              raff-fini.c \
              raff-check.c \
              raff-errstr.c \
	      raff-clearerr.c \
              raff-root.c \
              raff-stable.c \
              raff-tree.c \
              raff-chdir.c \
              raff-chpath.c \
              raff-getchar.c \
              raff-getint.c \
              raff-getdouble.c \
              raff-getcomplex.c \
              separator.c

aff.headers = aff-i.h \
              aff.h

coding.sources = encode-type.c \
                 encode-u32.c \
                 encode-u64.c \
                 encode-double.c \
                 decode-type.c \
                 decode-u32.c \
                 decode-u64.c \
                 decode-double.c

alloc.sources = aff-alloc.c \
                aff-alloc.c \
                raff-chpath.c \
                raff-fini.c \
                raff-init.c \
                stable-fini.c \
                stable-init.c \
                stable-insert.c \
                treap-fini.c \
                treap-init.c \
                treap-insert.c \
                tree-fini.c \
                tree-init.c \
                tree-insert.c \
                waff-fini.c \
                waff-init.c \
                waff-mkpath.c

alloc.headers = alloc.h

coding.headers = coding.h

io.sources = io.c

cpp.sources = rwaff.cpp \
	      mdarray.cpp

io.headers = io.h

interface.parts = aff.h node.h tree.h stable.h treap.h md5.h coding.h alloc.h


ifeq "$(wildcard $(TOP)/config/$(CONFIG))" ""
all install:
	@echo "configuration $(CONFIG) not found"
	@exit 1

else
include $(TOP)/config/$(CONFIG)

all: $(library) $(interface)


install:
	echo "Run install in the top directory"
	exit 1

$(interface): $(interface.parts) interface.prologue interface.epilogue
	( cat interface.prologue > $(interface) && \
	  for f in $(interface.parts) ; do \
             echo '/*' $$f '*/' >> $(interface) ; \
             grep -v MARK_ $$f >> $(interface) ; done && \
          cat interface.epilogue >> $(interface)) || \
        $(RM) $(interface)

$(interface): Makefile

$(library): $(sources:%.c=%.o) $(cpp.sources:%.cpp=%.o)
	$(AR) cr $(library) $(sources:%.c=%.o) $(cpp.sources:%.cpp=%.o)
	$(RANLIB) $(library)

$(sources:%.c=%.o): %.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(cpp.sources:%.cpp=%.o): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(node.sources:%.c=%.o): $(node.headers)
$(treap.sources:%.c=%.o): $(treap.headers)
$(stable.sources:%.c=%.o): $(stable.headers) treap.h
$(tree.sources:%.c=%.o): $(tree.headers) treap.h stable.h node.h node-i.h
$(md5.sources:%.c=%.o): $(md5.headers)
$(aff.sources:%.c=%.o): $(aff.headers) md5.h tree.h stable.h coding.h io.h
$(coding.sources:%.c=%.o): $(coding.headers) node.h
$(io.sources:%.c=%.o): $(io.headers)
$(alloc.sources:%.c=%.o): $(alloc.headers)
$(cpp.sources:%.cpp=%.o): $(interface)

endif

clean:
	$(RM) $(sources:%.c=%.o)

realclean: clean
	$(RM) $(library) $(interface)
