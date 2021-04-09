rhotic
======

Yet another semantics for R. A work in progress.

These semantics model a tiny subset of R, particularly its operations. The
semantics are intended to be used in a program analysis, rather than to
illustrate specific features of R.

As such, these semantics do not cover the ["core" of
R](http://janvitek.org/pubs/ecoop12.pdf) (e.g., the semantics of functions,
variable scoping and binding, copy semantics, etc.), the [call-by-need nature of
R](http://janvitek.org/pubs/oopsla19a.pdf), or [complex indexing
operations](https://github.com/reactorlabs/RhoVec/).


Getting Started
---------------

Install [opam](https://opam.ocaml.org/). Then run the following:

```
opam switch create ocaml.4.10.0
git clone https://github.com/reactorlabs/rhotic.git
cd rhotic
make deps # install dependencies
make
```
