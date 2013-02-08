module Language.Go.Tests.Parser where

import Test.HUnit

import Text.Parsec.Combinator (eof, optional)
import Text.Parsec.Error

import Language.Go.Parser.Parser
import Language.Go.Parser.Tokens
import Language.Go.Syntax.AST

strerror :: Either ParseError a -> Either String a
strerror (Left err) = Left (show err)
strerror (Right x) = Right x

testParse :: (Show a, Eq a) => String -> GoParser a -> String -> a -> Test
testParse desc parser text ref = TestLabel desc $ TestCase $ assertEqual desc want got
    where got = strerror $ goParseTestWith (do { p <- parser; optional goTokSemicolon; eof; return p }) text
          want = Right ref

namedType :: String -> GoType
namedType = GoTypeName Nothing . GoId

ident :: String -> GoExpr
ident s = GoPrim $ GoQual Nothing (GoId s)

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

testSwitch1 = testParse "test switch with empty case"
    goStatement "switch x { case 1: case 2: default: return; }" $
    GoStmtSwitch
      (GoCond Nothing (Just (GoPrim (GoQual Nothing (GoId "x")))))
      [ GoCase [GoPrim (GoLiteral (GoLitInt "1" 1))] []
      , GoCase [GoPrim (GoLiteral (GoLitInt "2" 2))] [],
        GoDefault [GoStmtReturn []]
      ]

testSelect1 = testParse "test empty select"
    goStatement "select {}" $
    GoStmtSelect []

testSelect2 = testParse "test select with empty case"
    goStatement "select { case <-ch: }" $
    GoStmtSelect [GoCase [GoChanRecv Nothing (Go1Op (GoOp "<-") (ident "ch"))] []]

testSelect3 = testParse "test select with parentheses"
    goStatement "select { case (<-ch): }" $
    GoStmtSelect [GoCase [GoChanRecv Nothing (Go1Op (GoOp "<-") (ident "ch"))] []]

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

testRecv1 = testParse "receive operator"
    goExpression "<-c" $
    Go1Op (GoOp "<-") (GoPrim (GoQual Nothing (GoId "c")))

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

testLabel1 = testParse "labelled statement"
    goStatement "label: return" $
    GoStmtLabeled (GoId "label") (GoStmtReturn [])

testFor1 = testParse "while true"
    goStatement "for {}" $
    GoStmtFor (GoForWhile Nothing) (GoBlock [])

testIf1 = testParse "if statement with init"
    goStatement "if v, ok := F(); ok {}" $
    GoStmtIf
      (GoCond (Just stmt) (Just $ ident "ok"))
      (GoBlock [])
      Nothing
  where
    stmt = GoSimpVar [GoId "v", GoId "ok"] [GoPrim (GoCall (GoQual Nothing (GoId "F")) [] False)]

testIf2 = testParse "if statement with complex terms"
    goStatement "if F() {}" $
    GoStmtIf
      (GoCond Nothing $ Just expr)
      (GoBlock [])
      Nothing
  where
    expr = GoPrim (GoCall (GoQual Nothing (GoId "F")) [] False)

testsParser =
  [ testImport1
  , testBuiltin1
  , testBuiltin2
  , testConversion1
  , testSwitch1
  , testSelect1
  , testSelect2
  , testSelect3
  , testLiteral1
  , testLiteral2
  , testLiteral3
  , testLiteral4
  , testRecv1
  , testMethod1
  , testMethod2
  , testSelector1
  , testTypeAssert1
  , testStructDecl1
  , testLabel1
  , testFor1
  , testIf1
  , testIf2
  ]
