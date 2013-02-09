module Language.Go.Tests.Lexer (testsLexer) where

import Test.HUnit

import Language.Go.Parser.Lexer
import Language.Go.Parser.Parser (goTokenize)
import Language.Go.Parser.Tokens

testLex :: String -> String -> [GoToken] -> Test
testLex desc text ref = TestLabel desc $ TestCase $ assertEqual desc toks ref
  where toks = map strip $ goTokenize text
        strip (GoTokenPos _ tok) = tok

testRawString1 = testLex "raw string"
  "`hello`"
  [ GoTokStr (Just "`hello`") "hello"
  , GoTokSemicolon]

testRawString2 = testLex "raw multiline string"
  "`hello\n\tworld`"
  [ GoTokStr (Just "`hello\n\tworld`") "hello\n\tworld"
  , GoTokSemicolon]

testCharLit1 = testLex "rune literal for backslash"
  "'\\\\'"
  [ GoTokChar (Just "'\\\\'") '\\'
  , GoTokSemicolon]

testsLexer =
  [ testRawString1
  , testRawString2
  , testCharLit1
  ]
