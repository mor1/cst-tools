.PHONY: build
build:
	dune build extract.exe

.PHONY: clean
clean:
	$(RM) -r _build
