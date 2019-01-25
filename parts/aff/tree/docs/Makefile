LATEX     = latex
MAKEINDEX = makeindex
DVIPDFM   = dvipdfm

.PHONY: all clean realclean

sources = aff_spec.tex
pdf     = $(sources:%.tex=%.pdf)

all: $(pdf)

clean:
	$(RM) $(sources:%.tex=%.aux)
	$(RM) $(sources:%.tex=%.dvi)
	$(RM) $(sources:%.tex=%.idx)
	$(RM) $(sources:%.tex=%.ilg)
	$(RM) $(sources:%.tex=%.ind)
	$(RM) $(sources:%.tex=%.log)
	$(RM) $(sources:%.tex=%.out)
	$(RM) $(sources:%.tex=%.toc)
	$(RM) $(sources:%.tex=%.ttt)
	$(RM) $(sources:%.tex=%.lot)

realclean: clean
	$(RM) $(sources:%.tex=%.pdf)

$(pdf): %.pdf: %.dvi
	$(DVIPDFM) $(@:%.pdf=%)

$(sources:%.tex=%.dvi): %.dvi: %.tex
	$(LATEX) $(@:%.dvi=%)
	$(LATEX) $(@:%.dvi=%)
	$(LATEX) $(@:%.dvi=%)
	$(MAKEINDEX) $(@:%.dvi=%)
	$(LATEX) $(@:%.dvi=%)
	$(LATEX) $(@:%.dvi=%)
	$(LATEX) $(@:%.dvi=%)
