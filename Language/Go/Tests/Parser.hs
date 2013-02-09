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

testBuiltin1 = testParse "test builtin make"
    goBuiltinCall "make([]int, 4)" $
    GoMake (GoSliceType (namedType "int")) [GoPrim $ GoLiteral $ GoLitInt "4" 4]

testBuiltin2 = testParse "test builtin make as expr"
    goExpression "make([]int, 4)" $
    GoPrim (GoMake (GoSliceType $ namedType "int") [GoPrim $ GoLiteral $ GoLitInt "4" 4])

testConversion1 = testParse "byte slice conversion"
    goExpression "[]byte(\"hello world\")" $
    GoPrim $ GoCast (GoSliceType (namedType "byte")) (GoPrim $ GoLiteral $ GoLitStr "\"hello world\"" "hello world")

testConst1 = testParse "raw string constant"
    goExpression "`hello`" $
    GoPrim $ GoLiteral $ GoLitStr "`hello`" "hello"

testConst2 = testParse "rune literal of backquote"
    goExpression "'`'" $
    GoPrim $ GoLiteral $ GoLitChar "'`'" '`'

-- testConst3 = testParse "rune literal of single quote"
--     goExpression "'\\''" $
--     GoPrim $ GoLiteral $ GoLitChar "'\\''" '\''

testConst4 = testParse "rune literal of backslash"
    goExpression "'\\\\'" $
    GoPrim $ GoLiteral $ GoLitChar "'\\\\'" '\\'

testLiteral1 = testParse "empty composite literal"
    goCompositeLit "T{}" $
    GoLitComp (namedType "T") (GoComp [])

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
    goCompositeLit "map[T]U{T{1, 2}: \"hello\"}" $
    GoLitComp
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

testMethod1 = testParse "method call"
    goExpression "time.Now()" $
    GoPrim $ GoCall (GoQual (Just $ GoId "time") (GoId "Now")) [] False

testMethod2 = testParse "method signature with anonymous receiver"
    goMethodDecl "func (T) Method ()" $
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
  , testBuiltin1
  , testBuiltin2
  , testConversion1
  , testConst1
  , testConst2
  -- , testConst3
  , testLiteral1
  , testLiteral2
  , testLiteral3
  , testLiteral4
  , testOp1
  , testOp2
  -- , testCall1
  , testCall2
  , testMethod1
  , testMethod2
  , testSelector1
  , testTypeAssert1
  , testStructDecl1
  , testIfaceDecl1
  ]
