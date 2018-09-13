#!/bin/sh

chmod -R a+w .
docker run --rm -v `pwd`:/home/opam/workspace -it ocaml/opam:alpine_ocaml-4.02.3 sh -c "\
    sudo apk add m4 && \
    (cd opam-repository && git pull --quiet) && \
    OPAMYES=1 opam update && \
    (cd workspace && \
        OPAMYES=1 opam pin add ppx_bs_css . -n && \
        OPAMYES=1 opam install ppx_bs_css --deps-only && \
        make \
    )"
