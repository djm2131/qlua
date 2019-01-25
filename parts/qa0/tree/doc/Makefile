NOWEAVE  = noweave -filter btdefn -index -latex -delay
NOTANGLE = notangle -filter btdefn
NOINDEX  = noindex
CPIF     = cpif
LATEX    = latex
DVIPDFM  = dvipdfm

noweb = qa0.nw

f.tex = $(noweb:%.nw=%.tex)
f.dvi = $(noweb:%.nw=%.dvi)
f.pdf = $(noweb:%.nw=%.pdf)

.PHONY: all clean realclean

all: $(f.pdf)

clean:
	$(RM) $(noweb:%.nw=%.aux) $(noweb:%.nw=%.dvi) $(noweb:%.nw=%.log) \
              $(noweb:%.nw=%.nwi) $(noweb:%.nw=%.out) $(noweb:%.nw=%.tex) \
              $(noweb:%.nw=%.toc)

realclean: clean

pdf: $(f.pdf)

###
$(f.pdf): %.pdf: %.dvi
	$(RM) $@
	$(DVIPDFM) -o $@ $<

$(f.dvi): %.dvi: %.tex
	$(LATEX) $<
	$(NOINDEX) $(<:%.tex=%)
	$(LATEX) $<
	$(NOINDEX) $(<:%.tex=%)
	$(LATEX) $<

$(f.tex): %.tex: %.nw
	$(NOWEAVE) $< > $@

