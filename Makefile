.PHONY: build
build:
	jbuilder build extract.exe

.PHONY: clean
clean:
	$(RM) -r _build
