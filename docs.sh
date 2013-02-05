#!/bin/bash

FILES="\
Language/Go/Parser.hs \
Language/Go/Parser/Parser.hs \
Language/Go/Parser/Tokens.hs \
Language/Go/Syntax.hs \
Language/Go/Syntax/AST.hs"

mkdir -p docs
haddock -odocs -h $FILES
#for X in $FILES ; do
#done
