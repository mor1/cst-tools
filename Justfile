_default:
    @just --list

PWD := env("PWD")
DOCDIR := "_build/default/_doc/_html/"

# build `ocal`
build:
    dune build @all

# clean droppings
clean:
    dune clean

# install `ocal`, including local install
install:
    dune build @install
    ln -sf {{PWD}}/_build/install/default/bin/breakdown ~/.local/bin/

# uninstall `ocal`
uninstall:
    dune uninstall

# format sources
format:
    dune fmt

# lint sources
lint:
    dune build @lint
    dune-release lint

# build docs
doc:
    dune build @doc
    dune build @doc-private

# open docs
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
