.PHONY: build test

build:
	esy

test:
	cd test_bs && yarn build && cd ..

