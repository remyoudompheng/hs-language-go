module Language.Go.Tests.Lexer (testsLexer) where

import Test.HUnit

import Language.Go.Parser.Lexer
import Language.Go.Parser.Parser (goTokenize)
import Language.Go.Parser.Tokens

testLex :: String -> String -> [GoToken] -> Test
testLex desc text ref = TestLabel desc $ TestCase $ assertEqual desc ref toks
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

testCharLit2 = testLex "rune literal for newline"
  "'\\n'"
  [ GoTokChar (Just "'\\n'") '\n'
  , GoTokSemicolon]

testCharLit3 = testLex "rune literal for e-acute"
  "'é'"
  [ GoTokChar (Just "'é'") 'é'
  , GoTokSemicolon]

testString1 = testLex "string with backslash"
  "\"\\\\\""
  [ GoTokStr (Just "\"\\\\\"") "\\"
  , GoTokSemicolon]

testString2 = testLex "long string with backslash"
  "{\"\\\\\", \"a\", false, ErrBadPattern},"
  [ GoTokLBrace
  , GoTokStr (Just "\"\\\\\"") "\\"
  , GoTokComma,GoTokStr (Just "\"a\"") "a"
  , GoTokComma
  , GoTokId "false"
  , GoTokComma
  , GoTokId "ErrBadPattern"
  , GoTokRBrace
  , GoTokComma
  ]

testsLexer =
  [ testRawString1
  , testRawString2
  , testCharLit1
  , testCharLit2
  , testCharLit3
  , testString1
  , testString2
  ]
