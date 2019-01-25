SFC = sfc
CP  = cp
V = 0

ifeq ("$V", "0")
   E=@echo "  "
   C=@
else
   E=@echo > /dev/null ""
   C=
endif

sources.sf =  \
	sfc.sf \
	common.sf

sources.ss = \
        error.ss \
        print.ss \
        read.ss \
        format.ss \
	ast.ss \
        split.ss \
	attr.ss \
	basis.ss \
	cenv.ss \
	verbose.ss \
	parser.ss \
	qa0print.ss \
	cheader.ss \
	q2complex.ss \
	c2real.ss \
	cx2qh.ss \
	cfold.ss \
	backend.ss \
	be-ckind.ss \
        be-bgq-xlc.ss \
	be-c99-64.ss \
	be-c99.ss \
	be-dry.ss \
	qa0.ss \
        fmap.ss \
	main-sfc.ss

obj.c = $(sources.sf:%.sf=%.c) $(sources.ss:%.ss=%.c)

.PHONY: all clean realclean

all: $(obj.c:%=../bootstrap/%)

clean:
	$ERM .c
	$C$(RM) $(obj.c)

realclean: clean

####
$(obj.c:%=../bootstrap/%): ../bootstrap/%: %
	$EMV $@
	$C$(CP) $< $@

$(sources.ss:%.ss=%.c): %.c: %.ss
	$ESFC $<
	$C$(SFC) $<

$(sources.sf:%.sf=%.c): %.c: %.sf
	$ESFC $<
	$C$(SFC) $<

$(sources.ss:%.ss=%.c): sfc.sf
####
common.c: sfc.sf
error.c:  print.ss
print.c:  error.ss
print.c:  format.ss
read.c:  error.ss
format.c:  error.ss
ast.c:  common.sf
ast.c:  error.ss
ast.c:  format.ss
attr.c:  common.sf
attr.c:  error.ss
attr.c:  ast.ss
attr.c:  cenv.ss
cenv.c:  common.sf
cenv.c:  error.ss
cenv.c:  basis.ss
cenv.c:  ast.ss
verbose.c:  print.ss
parser.c:  common.sf
parser.c:  read.ss
parser.c:  error.ss
parser.c:  ast.ss
parser.c:  cenv.ss
qa0print.c:  common.sf
qa0print.c:  error.ss
qa0print.c:  print.ss
qa0print.c:  ast.ss
cheader.c:  common.sf
cheader.c:  error.ss
cheader.c:  print.ss
cheader.c:  format.ss
cheader.c:  ast.ss
cheader.c:  cenv.ss
cheader.c:  attr.ss
cheader.c:  verbose.ss
q2complex.c:  common.sf
q2complex.c:  error.ss
q2complex.c:  ast.ss
q2complex.c:  cenv.ss
q2complex.c:  attr.ss
c2real.c:  common.sf
c2real.c:  error.ss
c2real.c:  ast.ss
c2real.c:  cenv.ss
c2real.c:  attr.ss
cx2qh.c: sfc.sf
cx2qh.c: common.sf
cx2qh.c: error.ss
cx2qh.c: ast.ss
cfold.c:  common.sf
cfold.c:  error.ss
cfold.c:  basis.ss
cfold.c:  ast.ss
cfold.c:  parser.ss
cfold.c:  cenv.ss
backend.c:  error.ss
backend.c:  cenv.ss
be-ckind.c:  common.sf
be-ckind.c:  error.ss
be-ckind.c:  print.ss
be-ckind.c:  format.ss
be-ckind.c:  ast.ss
be-ckind.c:  parser.ss
be-ckind.c:  attr.ss
be-ckind.c:  backend.ss
be-ckind.c:  cenv.ss
be-ckind.c:  cheader.ss
be-ckind.c:  verbose.ss
be-bgq-xlc.c: sfc.sf
be-bgq-xlc.c: common.sf
be-bgq-xlc.c: error.ss
be-bgq-xlc.c: format.ss
be-bgq-xlc.c: ast.ss
be-bgq-xlc.c: parser.ss
be-bgq-xlc.c: attr.ss
be-bgq-xlc.c: backend.ss
be-bgq-xlc.c: cx2qh.ss
be-bgq-xlc.c: cenv.ss
be-bgq-xlc.c: cheader.ss
be-bgq-xlc.c: verbose.ss
be-bgq-xlc.c: be-ckind.ss
be-c99-64.c:  common.sf
be-c99-64.c:  error.ss
be-c99-64.c:  ast.ss
be-c99-64.c:  parser.ss
be-c99-64.c:  attr.ss
be-c99-64.c:  backend.ss
be-c99-64.c:  cenv.ss
be-c99-64.c:  cheader.ss
be-c99-64.c:  verbose.ss
be-c99-64.c:  be-ckind.ss
be-c99.c:  common.sf
be-c99.c:  error.ss
be-c99.c:  ast.ss
be-c99.c:  parser.ss
be-c99.c:  attr.ss
be-c99.c:  backend.ss
be-c99.c:  cenv.ss
be-c99.c:  cheader.ss
be-c99.c:  verbose.ss
be-c99.c:  be-ckind.ss
be-dry.c:  common.sf
be-dry.c:  error.ss
be-dry.c:  ast.ss
be-dry.c:  parser.ss
be-dry.c:  attr.ss
be-dry.c:  backend.ss
be-dry.c:  cenv.ss
be-dry.c:  cheader.ss
be-dry.c:  verbose.ss
be-dry.c:  be-ckind.ss
qa0.c:  common.sf
qa0.c:  error.ss
qa0.c:  print.ss
qa0.c:  cheader.ss
qa0.c:  backend.ss
qa0.c:  parser.ss
qa0.c:  cfold.ss
qa0.c:  q2complex.ss
qa0.c:  c2real.ss
qa0.c:  cx2qh.ss
qa0.c:  qa0print.ss
qa0.c:  be-c99.ss
qa0.c:  be-c99-64.ss
qa0.c:  be-dry.ss
main-sfc.c:  common.sf
main-sfc.c:  qa0.ss
fmap.c: common.sf
fmap.c: error.ss
split.c: common.sf
split.c: error.ss
split.c: ast.ss