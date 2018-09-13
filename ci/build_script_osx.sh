#!/bin/sh

eval `opam config env`

OPAMYES=1 opam pin add ppx_bs_css . -n
OPAMYES=1 opam install ppx_bs_css --deps-only
