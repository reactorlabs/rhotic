SHELL    := /bin/bash

EXEC     := rhotic

BUILDDIR := _build/default
TESTDIR  := $(BUILDDIR)/test
SRCDIR   := $(BUILDDIR)/src

all: exe

run: exe
	./$(EXEC)

utop:
	dune utop lib

debug: bc
	ocamldebug $(SRCDIR)/main.bc

exe:
	dune build src/main.exe
	cp $(SRCDIR)/main.exe $(EXEC)
	@chmod 755 $(EXEC)

bc:
	dune build src/main.bc

fmt:
	@# force this command to always return true
	dune build @fmt --auto-promote || true

deps:
	opam install . --deps-only --locked

clean:
	dune clean
	rm -rf _coverage
	rm -f $(EXEC)

.PHONY: all run utop debug exe bc fmt deps clean
