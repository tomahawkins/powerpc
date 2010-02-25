.PHONY: all
all: test

.PHONY: instrs
instrs:
	runhaskell -W SetupInstrs.hs

.PHONY: test
test: instrs Test.hs Language/PowerPC/*.hs Language/PowerPC/Instructions/*.hs
	runhaskell -W Test.hs

