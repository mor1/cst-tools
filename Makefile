.PHONY: build
build:
	dune build @all

.PHONY: depend depends
depend depends:
	opam install \
	  containers \
	  csv \
	  notty

.PHONY: clean
clean:
	$(RM) -r _build
