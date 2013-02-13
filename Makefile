HC = ghc
RUNHS = runhaskell

TESTAPPS = \
tests/test-lexer \
tests/test-parser \
tests/test-printer

TESTDEPS = \
Language/Go/Parser/Lexer.hs \
Language/Go/Parser/Tokens.hs \
Language/Go/Parser/Operators.hs \
Language/Go/Parser/Parser.hs \
Language/Go/Pretty.hs \
Language/Go/Syntax/AST.hs

.PHONY: all
all: dist
	$(RUNHS) Setup build

dist:
	$(RUNHS) Setup configure

.PHONY: clean
clean:
	$(RUNHS) Setup clean
	rm -f tests/{*.hi,*.o}

install: all
	sudo $(RUNHS) Setup install

check: $(TESTAPPS)
	bash ./tests.sh

tests/test-%: tests/test-%.hs $(TESTDEPS)
	$(HC) --make $<

Language/Go/Parser/Lexer.hs: Language/Go/Parser/Lexer.x
	alex Language/Go/Parser/Lexer.x


