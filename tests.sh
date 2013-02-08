#!/bin/bash

#DIR="tests"
DIR=$GOROOT/src/pkg

for TEST in tests/*.go $(find ${DIR} -name '*\.go' "!" -path "*/testdata/*") ; do
    if tests/test-lexer "$TEST" 2>&1 | grep "lexical error"; then
        echo "FAIL: $TEST"
        continue
    elif tests/test-parser "$TEST" | grep -A 2 '^ERROR'; then
        echo "FAIL: $TEST"
        continue
    fi
    #echo "PASS: $TEST"
done

exit 0
