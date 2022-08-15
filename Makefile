.PHONY: all test

all:
	stack build --fast --test --no-run-tests

test:
	stack test --fast
