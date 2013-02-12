-- |
-- Module      : Language.Go.Tests.Parser
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
-- 
-- This module provides tests for parsing of expressions and top-level
-- constructs.

module Language.Go.Tests.Parser (testsParser) where

import Language.Go.Parser.Parser
import Language.Go.Syntax.AST

import Language.Go.Tests.Common

-- begin debug
-- import Debug.Trace
-- import Text.Parsec.Prim
-- import Control.Monad
-- import Language.Go.Parser.Operators

testImport1 = testParse "dot import"
    goImportDecl "import . \"os\"" $
    GoImportDecl [GoImpSpec GoImpDot "os"]

testConst1 = testParse "const decl on one line"
    goStatement "const ( A = 1 )" $
    GoStmtDecl $ GoConst [GoCVSpec [GoId "A"] Nothing [GoPrim $ GoLiteral $ GoLitInt "1" 1]]

testConst2 = testParse "const decl on one line"
    goStatement "const A = 1 " $
    GoStmtDecl $ GoConst [GoCVSpec [GoId "A"] Nothing [GoPrim $ GoLiteral $ GoLitInt "1" 1]]

testBuiltin1 = testParse "test builtin make"
    goExpression "make([]int, 4)" $
    GoPrim $ GoMake (GoSliceType (namedType "int")) [GoPrim $ GoLiteral $ GoLitInt "4" 4]

testConversion1 = testParse "byte slice conversion"
    goExpression "[]byte(\"hello world\")" $
    GoPrim $ GoCast (GoSliceType (namedType "byte")) (GoPrim $ GoLiteral $ GoLitStr "\"hello world\"" "hello world")

testConversion2 = testParse "conversion to pointer"
    goExpression "*(*unsafe.Pointer)(unsafe.Pointer(&fn))" $
    Go1Op (GoOp "*") (GoPrim $ GoCall (GoParen $ Go1Op (GoOp "*") $ GoPrim unsafeptr) [GoPrim (GoCall unsafeptr [Go1Op (GoOp "&") (ident "fn")] False)] False)
  where unsafeptr = GoQual (Just $ GoId "unsafe") (GoId "Pointer")

testLiteral1 = testParse "empty composite literal"
    goExpression "T{}" $
    GoPrim $ GoLiteral $ GoLitComp (namedType "T") (GoComp [])

testLiteral2 = testParse "non-empty composite literal as expression"
    goExpression "T{Field: value}" $
    GoPrim (GoLiteral (GoLitComp
      (namedType "T")
      (GoComp [GoElement (GoKeyField (GoId "Field")) (GoValueExpr (GoPrim (GoQual Nothing (GoId "value"))))])
    ))

testLiteral3 = testParse "composite literal in statement"
    goStatement "a := T{Field: value}" $
    GoStmtSimple $ GoSimpVar
      [GoId "a"]
      [GoPrim (GoLiteral (GoLitComp
        (namedType "T")
        (GoComp [GoElement (GoKeyField (GoId "Field")) (GoValueExpr (GoPrim (GoQual Nothing (GoId "value"))))])
      ))]

testLiteral4 = testParse "map literal with composite keys"
    goExpression "map[T]U{T{1, 2}: \"hello\"}" $
    GoPrim $ GoLiteral $ GoLitComp
      (GoMapType (namedType "T") (namedType "U"))
      (GoComp [
        GoElement
         (GoKeyIndex $ GoPrim $ GoLiteral $ GoLitComp
           (namedType "T")
           (GoComp [GoElement GoKeyNone (lit "1" 1),
                    GoElement GoKeyNone (lit "2" 2)])
         )
         (GoValueExpr $ GoPrim $ GoLiteral $ GoLitStr "\"hello\"" "hello")]
      )
  where lit s n = GoValueExpr (GoPrim (GoLiteral (GoLitInt s n)))

testLiteral5 = testParse "array literal with abridged syntax"
    goExpression "[]T{{a, b}, {c, d},}" $
    GoPrim $ GoLiteral $ GoLitComp (GoSliceType $ namedType "T")
      (GoComp [ GoElement GoKeyNone (GoValueComp (GoComp [elem "a", elem "b"]))
              , GoElement GoKeyNone (GoValueComp (GoComp [elem "c", elem "d"])) ])
  where elem t = GoElement GoKeyNone $ GoValueExpr $ ident t

testOp1 = testParse "expression with operator"
    goExpression "!*p" $
    Go1Op (GoOp "!") $ Go1Op (GoOp "*") $ ident "p"

testOp2 = testParse "receive operator"
    goExpression "<-c" $
    Go1Op (GoOp "<-") $ ident "c"

testCall1 = testParse "call with trailing comma after args"
    goExpression "f(a,b,c,)" $
    GoPrim $ GoCall (GoQual Nothing $ GoId "f") (map ident ["a", "b", "c"]) False

testCall2 = testParse "call with comment (used to insert semicolon)"
   goExpression "f(a, b, c /* comment */)" $
   GoPrim $ GoCall (GoQual Nothing $ GoId "f") (map ident ["a", "b", "c"]) False

testCall3 = testParseFail "call with multiple trailing commas"
    goExpression "f(a,b,c,,)"

testCall4 = testParse "call variadic with trailing comma after args"
    goExpression "f(a,b,c...,)" $
    GoPrim $ GoCall (GoQual Nothing $ GoId "f") (map ident ["a", "b", "c"]) True

testMethod1 = testParse "method call"
    goExpression "time.Now()" $
    GoPrim $ GoCall (GoQual (Just $ GoId "time") (GoId "Now")) [] False

testMethod2 = testParse "method signature with anonymous receiver"
    goTopLevelDecl "func (T) Method ()" $
    GoMeth $ GoMethDecl
      (GoRec False Nothing $ GoTypeName Nothing (GoId "T"))
      (GoId "Method")
      (GoSig [] [])
      GoNoBlock

testSelector1 = testParse "selector on index expression"
    goExpression "a[i].field" $
    GoPrim (GoSelect (GoIndex (GoQual Nothing (GoId "a")) (GoPrim (GoQual Nothing (GoId "i")))) (GoId "field"))

testTypeAssert1 = testParse "type assertion"
    goExpression "v.(T)" $
    GoPrim (GoTA (GoQual Nothing (GoId "v")) (namedType "T"))

testStructDecl1 = testParse "struct decl with embedded field"
    goType "struct { Field T; U }" $
    GoStructType
      [ GoFieldType {getFieldTag = "", getFieldId = [GoId "Field"], getFieldType = namedType "T"}
      , GoFieldAnon {getFieldTag = "", getFieldPtr = False, getFieldType = namedType "U"} 
      ]

testIfaceDecl1 = testParse "interface decl with embedded qualified interface"
    goType "interface { io.Reader }" $
    GoInterfaceType [GoIfaceName (Just (GoId "io")) (GoId "Reader")]

testsParser =
  [ testImport1
  , testConst1
  , testConst2
  , testBuiltin1
  , testConversion1
  , testConversion2
  , testLiteral1
  , testLiteral2
  , testLiteral3
  , testLiteral4
  , testLiteral5
  , testOp1
  , testOp2
  , testCall1
  , testCall2
  , testCall3
  , testCall4
  , testMethod1
  , testMethod2
  , testSelector1
  , testTypeAssert1
  , testStructDecl1
  , testIfaceDecl1
  ]
