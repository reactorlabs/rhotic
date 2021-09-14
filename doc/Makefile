SHELL    := /bin/bash

LATEXMK  := $(shell command -v latexmk 2> /dev/null)
LATEX    := pdflatex
BIBTEX   := bibtex

TEXFILE  := notes

all: $(TEXFILE).pdf

$(TEXFILE).pdf: $(TEXFILE).tex
ifdef LATEXMK
	latexmk -pdf $<
else
	$(LATEX) $<
	$(BIBTEX) $(subst .tex,.aux,$<)
	$(LATEX) $<
	$(LATEX) $<
endif

auto: $(TEXFILE).tex
	latexmk -pdf -pvc $<

halfclean:
	rm -rf *.log *.aux *.blg *~ *.out *.fdb_latexmk *.fls *.vtc

clean: halfclean
	rm -rf $(TEXFILE).pdf

.PHONY: all auto halfclean clean
