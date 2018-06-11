.PHONY: build clean test doc

build:
	esy build

test:
	esy jbuilder runtest

install:
	esy jbuilder install

uninstall:
	esy jbuilder uninstall

clean:
	rm -rf _build

