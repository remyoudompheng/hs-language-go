#!/bin/bash

#DIR="tests"
#DIR="tests/go/src/pkg/go"
DIR="tests/go/src/cmd"

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

for TEST in $(find ${DIR} -name '*\.go') ; do
    if dotest "$TEST" | tee "${TEST%.go}".log | grep ERROR ; then
        echo "FAIL: $TEST"
    else
        echo "PASS: $TEST"
    fi
done

exit 0