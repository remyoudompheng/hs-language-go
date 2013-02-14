#!/bin/bash

#DIR="tests"
DIR=$GOROOT/src/pkg

for TEST in tests/*.go $(find ${DIR} -name '*\.go' "!" -path "*/testdata/*") ; do
    if tests/test-lexer "$TEST" 2>&1 | grep "lexical error"; then
        echo "FAIL: $TEST"
        continue
    elif tests/test-parser "$TEST" 2>&1 | grep -v "lexical error" | grep -A 2 '^ERROR'; then
        echo "FAIL: $TEST"
        continue
    fi
    tests/test-printer $TEST False
done

exit 0
