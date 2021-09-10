SHELL := /bin/bash

LATEXMK := $(shell command -v latexmk 2> /dev/null)
LATEX   := pdflatex
TEXFILE := notes

all: $(TEXFILE).tex
ifndef LATEXMK
	$(LATEX) $<
else
	latexmk -pdf $<
endif

auto: $(TEXFILE).tex
	latexmk -pdf -pvc $<

halfclean:
	rm -rf *.log *.aux *.blg *~ *.out *.fdb_latexmk *.fls *.vtc

clean: halfclean
	rm -rf $(TEXFILE).pdf

.PHONY: all auto halfclean clean
