SHELL    := /bin/bash

LATEXMK  := $(shell command -v latexmk 2> /dev/null)
LATEX    := pdflatex
BIBTEX   := bibtex
OTT      := $(shell command -v ott 2> /dev/null)

OTTFLAGS := -tex_colour true -tex_wrap false -signal_parse_errors true

OTTFILE := mini_rhotic
TEXFILE := notes

all: $(TEXFILE).pdf

ott: $(OTTFILE)_.tex

ifdef OTT
$(OTTFILE)_.tex: $(OTTFILE).ott
	$(OTT) $(OTTFLAGS) -i $< -o $@

ottdebug: $(OTTFILE)_.tex
	$(OTT) $(OTTFLAGS) -show_sort true -show_defns true -i $(OTTFILE).ott
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

cleanall: clean
	rm -rf $(OTTFILE)_.tex

.PHONY: all ott ottdebug auto halfclean clean cleanall
