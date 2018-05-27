#!/bin/bash

#DIR="tests"
#DIR=$GOROOT/src/cmd
DIR=$GOROOT/src/log
#DIR=$GOROOT/test

for TEST in tests/*.go $(find ${DIR} -name '*\.go' "!" -path "*/testdata/*") ; do
    if tests/test-lexer "$TEST" 2>&1 | grep "lexical error"; then
        echo "FAIL: $TEST"
        continue
    elif tests/test-parser "$TEST" 2>&1 | grep -v "lexical error" | grep -A 2 '^ERROR'; then
        echo "FAIL: $TEST"
        continue
    fi
    tests/test-printer $TEST False || echo "ERROR: $TEST"
    tests/test-printer $TEST True > /tmp/test-hs-go2 || echo "ERROR: $TEST"
    gofmt -tabs=false -tabwidth=4 -comments=false $TEST > /tmp/test-hs-go1
    #colordiff -BEuw /tmp/test-hs-go1 /tmp/test-hs-go2
    git diff --word-diff=color -w --ignore-space-at-eol /tmp/test-hs-go1 /tmp/test-hs-go2 | grep $'\e'
done

exit 0
