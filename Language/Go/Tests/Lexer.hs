-- |
-- Module      : Language.Go.Tests.Lexer
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
-- 
-- This module provides tests for the lexer.

module Language.Go.Tests.Lexer (testsLexer) where

import Test.HUnit

import Language.Go.Parser.Lexer
import Language.Go.Parser.Tokens

testLex :: String -> String -> [GoToken] -> Test
testLex desc text ref = TestLabel desc $ TestCase $ assertEqual desc ref toks
  where toks = map strip $ insertSemi $ alexScanTokens text
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

testCharLit4 = testLex "rune literal with octal escaping"
  "'\\377'"
  [ GoTokChar (Just "'\\377'") '\xff'
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

testString3 = testLex "string with tab"
  "\"\t\""
  [ GoTokStr (Just "\"\t\"") "\t"
  , GoTokSemicolon]

testString4 = testLex "string literal with octal escaping"
  "\"\\377\""
  [ GoTokStr (Just "\"\\377\"") "\xff"
  , GoTokSemicolon]

testFloat1 = testLex "floating point"
  "11."
  [ GoTokReal (Just "11.") 11
  , GoTokSemicolon]

testFloat2 = testLex "floating point"
  "11.e+3"
  [ GoTokReal (Just "11.e+3") 11e+3
  , GoTokSemicolon]

testFloat3 = testLex "floating point"
  ".5"
  [ GoTokReal (Just ".5") 0.5
  , GoTokSemicolon]

testId1 = testLex "non-ASCII identifier"
  "α := 2"
  [ GoTokId "α"
  , GoTokColonEq
  , GoTokInt (Just "2") 2
  , GoTokSemicolon
  ]

testComment1 = testLex "comment with non-ASCII characters"
  "/* αβ */"
  [ GoTokComment True " αβ " ]

testComment2 = testLex "comment with non-ASCII characters"
  "/*\n\tαβ\n*/"
  [ GoTokComment True "\n\tαβ\n" ]

testsLexer =
  [ testRawString1
  , testRawString2
  , testCharLit1
  , testCharLit2
  , testCharLit3
  , testCharLit4
  , testString1
  , testString2
  , testString3
  , testString4
  , testFloat1
  , testFloat2
  , testFloat3
  , testId1
  , testComment1
  , testComment2
  ]
