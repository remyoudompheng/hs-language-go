-- |
-- Module      : Language.Go.Tests.Pretty
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
-- 
-- This module provides tests for the pretty-printer.
--

module Language.Go.Tests.Pretty (testsPretty) where

import Test.HUnit
import Text.PrettyPrint (render)
import Text.Parsec.Combinator

import Language.Go.Parser.Tokens
import Language.Go.Parser.Parser
import Language.Go.Pretty

testRoundTrip :: (Show a, Pretty a, Eq a) => String -> GoParser a -> String -> Test
testRoundTrip desc parser s = TestLabel desc $ TestCase $ assertEqual desc ast1 ast2
  where parse = goParseTestWith (do { p <- parser; optional goTokSemicolon; eof; return p })
        ast1 = stringify $ parse s
        ast2 = case ast1 of
                 Left err -> Left "cannot parse"
                 Right a  -> stringify $ parse (render $ pretty a)
        stringify s = case s of { Left e -> Left (show e); Right a -> Right a }

testMethod = testRoundTrip "method expression" goExpression "(*T).Method"

testStructTag = testRoundTrip "struct tag" goType "struct { Field T `tag` }"

testConversion = testRoundTrip "conversion to pointer" goExpression "(*[]T)(p)"
testConversionFunc = testRoundTrip "conversion to function" goExpression "(func())(x)"
testConversionChan = testRoundTrip "conversion to recv chan" goExpression "(<-chan int)(x)"

testTypeSwitch = testRoundTrip "type switch" goStatement "switch nerr := <-c; err := nerr.(type) {}"

testSignature = testRoundTrip "function signature" goType "func() (x)"

testsPretty :: [Test]
testsPretty =
  [ testMethod
  , testStructTag
  , testConversion
  , testConversionFunc
  , testTypeSwitch
  , testSignature ]
