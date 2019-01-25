CONFIG	= config/none
TOP	= ..#
.PHONY: all clean realclean lib

program = lhpc-aff
headers = util.h \
          procs.def

sources = lhpc-aff.c \
	  cat.c \
	  chdir_keypath.c \
          check.c \
	  copy.c \
	  copy_nodes.c \
	  diff.c \
	  extract.c \
	  import.c \
	  insert.c \
          ls.c \
	  common.c


ifeq "$(wildcard $(TOP)/config/$(CONFIG))" ""
all install:
	@echo "configuration $(CONFIG) not found"
	@exit 1

else
include $(TOP)/config/$(CONFIG)

all: $(program)

lib:
	make -C ../lib

$(program): $(sources:%.c=%.o)
	$(CC) $(CFLAGS) -I../lib -o $@ $^ -L../lib -llhpc-aff -lm

$(sources:%.c=%.o): %.o: %.c
	$(CC) $(CFLAGS) -I../lib -c -o $@ $<

$(sources:%.c=%.o): $(headers)

endif

clean:
	$(RM) $(sources:%.c=%.o)

realclean: clean
	$(RM) $(program)

