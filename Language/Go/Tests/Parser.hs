module Language.Go.Tests.Parser where

import Test.HUnit

import Text.Parsec.Error

import Language.Go.Parser.Parser
import Language.Go.Parser.Tokens
import Language.Go.Syntax.AST

strerror :: Either ParseError a -> Either String a
strerror (Left err) = Left (show err)
strerror (Right x) = Right x

testParse :: (Show a, Eq a) => String -> GoParser a -> String -> a -> Test
testParse desc parser text ref = TestLabel desc $ TestCase $ assertEqual desc want got
    where got = strerror $ goParseTestWith parser text
          want = Right ref

testBuiltin1 = testParse "test builtin make"
    goBuiltinCall "make([]int, 4)" $
    (GoMake (GoSliceType (GoTypeName [] (GoId "int"))) [GoPrim (GoLiteral (GoLitInt "4" 4))])

testBuiltin2 = testParse "test builtin make as expr"
    goExpression "make([]int, 4)" $
    GoPrim (GoMake (GoSliceType (GoTypeName [] (GoId "int"))) [GoPrim (GoLiteral (GoLitInt "4" 4))])

testSwitch1 = testParse "test switch with empty case"
    goStatement "switch x { case 1: case 2: default: return; }" $
    GoStmtSwitch
      (GoCond Nothing (Just (GoPrim (GoQual [] (GoId "x")))))
      [ GoCase [GoPrim (GoLiteral (GoLitInt "1" 1))] []
      , GoCase [GoPrim (GoLiteral (GoLitInt "2" 2))] [],
        GoDefault [GoStmtReturn []]
      ]

testsParser = [
  testBuiltin1, testBuiltin2,
  testSwitch1
  ]
