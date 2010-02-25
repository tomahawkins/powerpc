.PHONY: all
all: test

.PHONY: test
test: Test.hs Language/PowerPC/*.hs
	runhaskell -W Test.hs

