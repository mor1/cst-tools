.PHONY: build clean

build:
	jbuilder build extract.exe

clean:
	$(RM) -r _build
