LATEX    = latex
DVIPDFM  = dvipdfm

tex = eigen-cg.tex \
      twisted.tex

incs = defs.tex

g.dvi = $(tex:%.tex=%.dvi)
g.pdf = $(tex:%.tex=%.pdf)

.PHONY: all clean realclean 

# SNS: many hosts don't have LaTeX
#all: pdf sources
all:
	echo "Run make pdf to generate the docs"

clean:
	$(RM) $(tex:%.tex=%.aux) $(tex:%.tex=%.dvi) $(tex:%.tex=%.log) \
              $(tex:%.tex=%.nwi) $(tex:%.tex=%.out) $(tex:%.tex=%.toc) \
              $(tex:%.tex=%.loa)

realclean: clean
	$(RM) $(g.pdf)

sources: $(sources)

pdf: $(g.pdf)

###
$(g.pdf): %.pdf: %.dvi
	$(DVIPDFM) -o $@ $<

$(g.dvi): %.dvi: %.tex
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<

$(tex:%.tex=%.dvi): $(incs)
