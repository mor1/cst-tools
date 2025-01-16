# SPDX-FileCopyrightText: 2024 Richard Mortier <mort@cantab.net>
#
# SPDX-License-Identifier: ISC

_default:
    @just --list

PWD := env("PWD")
DOCDIR := "_build/default/_doc/_html"
BUILDDIR := "_build/install/default/bin"
TARGET := "breakdown"

# build targets
build:
    dune build @all

# cleanup
clean:
    dune clean

# install targets
install: build
    dune build @install
    ln -sf {{PWD}}/{{BUILDDIR}}/{{TARGET}} ~/.local/bin/

# uninstall targets
uninstall:
    dune uninstall

# run any tests
test:
    dune runtest

# format sources
format:
    dune fmt

# lint everything
lint:
    dune build @lint
    dune-release lint

# build docs
doc:
    dune build @doc
    dune build @doc-private

# open the docs for reading
read: doc
    handlr open {{DOCDIR}}/index.html || open {{DOCDIR}}

# tag and create a release
release:
    dune-release tag
    dune-release -vv

# install dependencies
depends:
    opam install --yes dune-release odoc
    opam install --yes . --deps-only
