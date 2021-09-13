SHELL    := /bin/bash

LATEXMK  := $(shell command -v latexmk 2> /dev/null)
LATEX    := pdflatex
BIBTEX   := bibtex
OTT      := $(shell command -v ott 2> /dev/null)

OTTFLAGS := -tex_colour true -tex_wrap false -signal_parse_errors true

OTTFILE := mini_rhotic
TEXFILE := notes2

all: $(TEXFILE).pdf

ifdef OTT
$(OTTFILE)_.tex: $(OTTFILE).ott
	$(OTT) $(OTTFLAGS) -i $< -o $@
endif

$(TEXFILE).pdf: $(TEXFILE).tex $(OTTFILE)_.tex
ifdef LATEXMK
	$(LATEXMK) -pdf $<
else
	$(LATEX) $<
	$(BIBTEX) $(subst .tex,.aux,$<)
	$(LATEX) $<
	$(LATEX) $<
endif

auto: $(TEXFILE).tex $(OTTFILE).ott $(OTTFILE)_.tex
	while true; do inotifywait -e modify $^; make all; done
	#latexmk -pdf -pvc $<

halfclean:
	rm -rf *.log *.aux *.blg *~ *.out *.fdb_latexmk *.fls *.vtc

clean: halfclean
	rm -rf $(TEXFILE).pdf

.PHONY: all auto halfclean clean
