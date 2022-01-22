SHELL    := /bin/bash

EXEC     := rhotic

BUILDDIR := _build/default
TESTDIR  := $(BUILDDIR)/test
SRCDIR   := $(BUILDDIR)/src

all: exe

run: exe
	./$(EXEC)

test: test-eval

test-eval:
	dune build test/eval_suite.exe
	cd $(TESTDIR) && ./eval_suite.exe --compact

test-oracle:
	dune build test/eval_suite.exe
	cd $(TESTDIR) && DUMP=suite.R ./eval_suite.exe test --compact "dummy"
	R -f $(TESTDIR)/suite.R

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
	@# output a linebreak
	@echo

coverage: clean
	dune runtest --instrument-with bisect_ppx --force
	@# Run the deparser
	cd $(TESTDIR) && DUMP=suite.R ./eval_suite.exe test --compact "dummy"
	bisect-ppx-report html
	bisect-ppx-report summary

deps:
	opam install . --deps-only --locked

clean:
	dune clean
	rm -rf _coverage
	rm -f $(EXEC)

.PHONY: all run test test-eval test-oracle utop debug exe bc fmt coverage deps clean
