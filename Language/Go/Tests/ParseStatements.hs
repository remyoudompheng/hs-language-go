module Language.Go.Tests.ParseStatements (testsParseStmts) where

import Language.Go.Parser.Parser
import Language.Go.Syntax.AST

import Language.Go.Tests.Common

testSwitch1 = testParse "test switch with empty case"
    goStatement "switch x { case 1: case 2: default: return; }" $
    GoStmtSwitch
      (GoCond Nothing (Just (GoPrim (GoQual Nothing (GoId "x")))))
      [ GoCase [GoPrim (GoLiteral (GoLitInt "1" 1))] []
      , GoCase [GoPrim (GoLiteral (GoLitInt "2" 2))] [],
        GoDefault [GoStmtReturn []]
      ]

testSwitch2 = testParse "test type switch"
    goStatement "switch v.(type) {}" $
    GoStmtTypeSwitch (GoCond Nothing (Just $ ident "v")) [] Nothing

testSwitch3 = testParse "test empty switch"
    goStatement "switch {}" $
    GoStmtSwitch (GoCond Nothing Nothing) []

testSwitch4 = testParse "test empty switch 2"
    goStatement "switch (v) {}" $
    GoStmtSwitch (GoCond Nothing (Just (GoPrim (GoParen $ ident "v")))) []

testSwitch5 = testParse "test empty switch parsing ambiguity"
    goExprSwitchStmt "switch v {}" $
    GoStmtSwitch (GoCond Nothing (Just $ ident "v")) []

testSwitch6 = testParse "test switch on call"
    goStatement "switch v.Kind() {}" $
    GoStmtSwitch (GoCond Nothing (Just $ GoPrim $ GoCall func [] False)) []
  where func = GoQual (Just $ GoId "v") (GoId "Kind")

testSelect1 = testParse "test empty select"
    goStatement "select {}" $
    GoStmtSelect []

testSelect2 = testParse "test select with empty case"
    goStatement "select { case <-ch: }" $
    GoStmtSelect [GoCase [GoChanRecv Nothing (Go1Op (GoOp "<-") (ident "ch"))] []]

testSelect3 = testParse "test select with parentheses"
    goStatement "select { case (<-ch): }" $
    GoStmtSelect [GoCase [GoChanRecv Nothing (Go1Op (GoOp "<-") (ident "ch"))] []]

testPostfix1 = testParse "test increment"
    goIncDecStmt "*p++" $
    GoSimpInc $ Go1Op (GoOp "*") $ ident "p"

testLabel1 = testParse "labelled statement"
    goStatement "label: return" $
    GoStmtLabeled (GoId "label") (GoStmtReturn [])

testFor1 = testParse "while true"
    goStatement "for {}" $
    GoStmtFor (GoForWhile Nothing) (GoBlock [])

-- testFor2 = testParse "for with parsing ambiguity"
--     goStatement "for a = a.prev; a.level > level; a = a.prev {}" $
--     GoStmtFor (GoForWhile Nothing) (GoBlock [])

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

testIf3 = testParseFail "if statement without condition"
    goStatement "if x := 0; {}"

testsParseStmts =
  [ testSwitch1
  , testSwitch2
  , testSwitch3
  , testSwitch4
  , testSwitch5
  , testSwitch6
  , testSelect1
  , testSelect2
  , testSelect3
  , testPostfix1
  , testLabel1
  , testFor1
  -- , testFor2
  , testIf1
  , testIf2
  , testIf3
  ]
