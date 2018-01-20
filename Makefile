.PHONY: build clean test doc

build:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build

doc:
	jbuilder build @doc

