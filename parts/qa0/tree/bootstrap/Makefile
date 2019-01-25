CC  = gcc -O3 -Wall

sources =  \
	sfc.c \
	common.c \
        error.c \
        print.c \
        read.c \
        format.c \
	ast.c \
        split.c \
	attr.c \
	basis.c \
	cenv.c \
	verbose.c \
	parser.c \
	qa0print.c \
	cheader.c \
	q2complex.c \
	c2real.c \
	cx2qh.c \
	cfold.c \
	backend.c \
	be-ckind.c \
	be-bgq-xlc.c \
	be-c99-64.c \
	be-c99.c \
	be-dry.c \
	qa0.c \
        fmap.c \
	main-sfc.c

obj.o = $(sources:%.c=%.o)

program = qa0

.PHONY: all clean realclean

all: $(program)

clean:
	$(RM) $(obj.o)

realclean: clean
	$(RM) $(program)

####
$(program): $(obj.o)
	$(CC) -o $(program) $(obj.o)

$(obj.o): %.o: %.c
	$(CC) -c -o $@ $<
