#!/bin/bash

#DIR="tests"
DIR=$GOROOT/src/pkg/go

function dotest {
tests/test-lexer  "$1"
tests/test-parser "$1"
}

#for TEST in ${DIR}/test*.go ; do
#    if dotest "$TEST" | tee "${TEST%.go}".log | grep ERROR ; then
#        echo "FAIL: $TEST"
#    else
#        echo "PASS: $TEST"
#    fi
#done

for TEST in tests/*.go $(find ${DIR} -name '*\.go' "!" -path "*/testdata/*") ; do
    if dotest "$TEST" | grep -A 2 ^ERROR ; then
        echo "FAIL: $TEST"
    else
        echo "PASS: $TEST"
    fi
done

exit 0
