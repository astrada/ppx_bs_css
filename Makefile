.PHONY: build test test_bs

build:
	esy build

test:
	esy build dune build test/test_suite.exe
	esy _build/default/test/test_suite.exe

test_bs:
	cd test_bs
	yarn build
	cd ..

