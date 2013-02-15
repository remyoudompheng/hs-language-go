-- |
-- Module      : Language.Go.Parser.Parser
-- Copyright   : (c) 2011 Andrew Robbins
-- License     : GPLv3 (see COPYING)
--
-- This module provides parsers for the various Go language elements.

{- LANGUAGE CPP -}
module Language.Go.Parser.Parser (
  -- Tokenizer and top-level parser.
  goTokenize,
  goParse,
  goParseTokens,
  goParseFileWith,
  goParseTestWith,

  goSource,
  goImportDecl,
  goTopLevelDecl,
  goType,
  goBlock,
  goExpression,
  goStatement,
) where
import Language.Go.Parser.Operators
import Language.Go.Parser.Tokens
import Language.Go.Parser.Lexer (alexScanTokens)
import Language.Go.Syntax.AST

import Control.Monad
import Data.Maybe (isJust, catMaybes)

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator


-- | Tokenize Go language source code
--
-- This is where semicolons are inserted into the token stream.
-- We also filter out comments here, so any comment processing
-- must occur before this stage.
--
-- TODO: Unicode identifiers
--
-- See also: 4.3. Semicolons
goTokenize :: String -> [GoTokenPos]
goTokenize = insertSemi . stripComments . alexScanTokens

-- | Parse Go Language source code into AST
goParse :: String -> String -> Either ParseError GoSource
goParse filename s = goParseTokens filename $ goTokenize s

-- | Parse Go Language token list into AST
goParseTokens :: String -> [GoTokenPos] -> Either ParseError GoSource
goParseTokens filename toks = runGoParser goSource filename toks

goParseFileWith :: GoParser a -> String -> String -> Either ParseError a
goParseFileWith start filename s = runGoParser start filename (goTokenize s)

goParseTestWith :: GoParser a -> String -> Either ParseError a
goParseTestWith start s = runGoParser start "" (goTokenize s)

--
--  Begin specification grammar
--

-- | Standard @Type@
--
-- See also: SS. 6. Types
goType :: GoParser GoType
goType =  goTypeName
      <|> goTypeLit
      <|> goParen goType

-- | Standard @TypeName@
--
-- See also: SS. 6. Types
goTypeName :: GoParser GoType
goTypeName = do
  (GoQual q n) <- goQualifiedIdent
  return $ GoTypeName q n

-- | Standard @TypeLit@
--
-- See also: SS. 6. Types
goTypeLit :: GoParser GoType
goTypeLit =  (try goSliceType)
         <|> goArrayType
         <|> goStructType
         <|> goPointerType
         <|> goFunctionType
         <|> goInterfaceType
         <|> goMapType
         <|> goChannelType

-- See also: SS. 6.1. Boolean types
-- See also: SS. 6.2. Numeric types
-- See also: SS. 6.3. String types

-- | Standard @ArrayType@
--
-- See also: SS. 6.4. Array types
goArrayType :: GoParser GoType
goArrayType = do
  l <- goBracket goExpression -- Go @ArrayLength@
  t <- goType                 -- Go @ElementType@
  return $ GoArrayType l t

-- | Standard @SliceType@
--
-- See also: SS. 6.5. Slice types
goSliceType :: GoParser GoType
goSliceType = do
  goTokLBracket
  goTokRBracket
  liftM GoSliceType goType

-- | Standard @StructType@
--
-- See also: SS. 6.6. Struct types
goStructType :: GoParser GoType
goStructType = do
  goTokStruct
  liftM GoStructType $ goBlockish goFieldDecl

-- | Standard @FieldDecl@
--
-- See also: SS. 6.6. Struct types
goFieldDecl :: GoParser GoFieldType
goFieldDecl = (try field) <|> anonymous where

    parseTag = do
      let strlit = token (GoTokStr Nothing "")
      GoTokStr (Just lit) s <- try strlit
      return $ GoLitStr lit s

    field = do -- Go @FieldDecl@
      ids <- goIdentifierList
      t <- goType
      tag <- optionMaybe parseTag
      return $ GoFieldType tag ids t

    anonymous = do -- Go @AnonymousField@
      a <- optionMaybe goTokAsterisk -- "*"
      t <- goTypeName -- TypeName
      tag <- optionMaybe parseTag
      return $ GoFieldAnon tag (isJust a) t

-- | Standard @PointerType@
--
-- See also: SS. 6.7. Pointer types
goPointerType :: GoParser GoType
goPointerType = do
  goTokAsterisk
  liftM GoPointerType goType -- Go @BaseType@

-- | Standard @FunctionType@
--
-- See also: SS. 6.8. Function types
goFunctionType :: GoParser GoType
goFunctionType = do
  goTokFunc
  liftM GoFunctionType goSignature

-- | Standard @Signature@
--
-- See also: SS. 6.8. Function types
goSignature :: GoParser GoSig
goSignature = do
  par <- goParameters
  res <- option [] goResult
  return $ GoSig par res

-- | Standard @Result@
--
-- See also: SS. 6.8. Function types
goResult :: GoParser [GoParam]
goResult =  goParameters 
        <|> do ty <- goType ; return [GoParam [] ty]

-- | Standard @Parameters@
--
-- See also: SS. 6.8. Function types
goParameters :: GoParser [GoParam]
goParameters = do
  goTokLParen
  params <- option [] $ do
    ps <- goParameterList
    optional goTokComma
    return ps
  goTokRParen
  return params

-- | Standard @ParameterList@
--
-- See also: SS. 6.8. Function types
goParameterList :: GoParser [GoParam]
goParameterList = sepBy goParameterDecl goTokComma

-- | Standard @ParameterDecl@
--
-- See also: SS. 6.8. Function types
goParameterDecl :: GoParser GoParam
goParameterDecl = (try goParameterDecl') <|> goParameterDecl'' where

    goParameterDecl' :: GoParser GoParam
    goParameterDecl' = do
      is <- option [] goIdentifierList
      optional goTokEllipsis
      t <- goType
      return $ GoParam is t
    --  return $ flip map is (\i -> GoParam (Just i) t)
    
    goParameterDecl'' :: GoParser GoParam
    goParameterDecl'' = do
      t <- goType
      return $ GoParam [] t

-- | Standard @InterfaceType@
--
-- See also: SS. 6.9. Interface types
goInterfaceType :: GoParser GoType
goInterfaceType = do
  goTokInterface
  xs <- goBlockish goMethodSpec
  return $ GoInterfaceType xs

-- | Standard @MethodSpec@
--
-- See also: SS. 6.9. Interface types
goMethodSpec :: GoParser GoMethSpec
goMethodSpec = try goMethodSpec' <|> goMethodSpec'' where

    goMethodSpec'' = do
      GoQual pkg id <- goQualifiedIdent -- Go @InterfaceTypeName@
      return $ GoIfaceName pkg id

    goMethodSpec' = do
      n <- goIdentifier -- Go @MethodName@
      s <- goSignature
      return $ GoMethSpec n s

-- | Standard @MapType@
--
-- See also: SS. 6.10. Map types
goMapType :: GoParser GoType
goMapType = do
  goTokMap
  kt <- goBracket goType -- Go @KeyType@
  et <- goType -- Go @ElementType@
  return $ GoMapType kt et

-- | Standard @ChannelType@
--
-- See also: SS. 6.11. Channel types
goChannelType :: GoParser GoType
goChannelType = do
  qi <- goChannelQuip
  ty <- goType
  return $ GoChannelType qi ty

-- | Nonstandard
goChannelQuip :: GoParser GoChanKind
goChannelQuip =  do goTokArrow ; goTokChan ; return GoIChan         -- 1=RecvDir
             <|> (try $ do goTokChan ; goTokArrow ; return GoOChan) -- 2=SendDir
             <|> (try $ do goTokChan ; return GoIOChan)             -- 3=BothDir

-- See also: SS. 7.1. Type identity
-- See also: SS. 7.2. Assignability

-- | Standard @Block@
--
-- See also: SS. 8. Blocks
goBlock :: GoParser GoBlock
goBlock = do liftM GoBlock $ goBlockish goAnyStatement

-- | Nonstandard
goBlockish :: GoParser a -> GoParser [a]
goBlockish p = goBrace $ do
  lines <- sepEndBy (optionMaybe p) goTokSemicolon
  return $ catMaybes lines

-- | Standard @Declaration@
--
-- See also: SS. 9. Declarations and scope
goDeclaration :: GoParser GoDecl
goDeclaration =  goConstDecl
             <|> goTypeDecl
             <|> goVarDecl
--             <?> "declaration"

-- | Standard @TopLevelDecl@
--
-- See also: SS. 9. Declarations and scope
goTopLevelDecl :: GoParser GoDecl
goTopLevelDecl =  goDeclaration
              <|> try goFunctionDecl
              <|> try goMethodDecl
--              <?> "top-level declaration"

-- | Standard @ConstDecl@
--
-- See also: SS. 9.5. Constant declarations
goConstDecl :: GoParser GoDecl
goConstDecl = goTokConst >> liftM GoConst (goParenish $ try goConstSpec)

-- | Standard @ConstSpec@
goConstSpec :: GoParser GoCVSpec
goConstSpec = do
  id <- goIdentifierList
  option (GoCVSpec id Nothing []) (try (goConstSpec' id) <|> goConstSpec'' id) where

    goConstSpec' :: [GoId] -> GoParser GoCVSpec
    goConstSpec' ids = do
      goTokEqual
      exs <- goExpressionList
      return $ GoCVSpec ids Nothing exs

    goConstSpec'' :: [GoId] -> GoParser GoCVSpec
    goConstSpec'' ids = do
      typ <- goType
      goTokEqual
      exs <- goExpressionList
      return $ GoCVSpec ids (Just typ) exs

-- | Standard @IdentifierList@
--
-- See also: SS. 9.5. Constant declarations
goIdentifierList :: GoParser [GoId]
goIdentifierList = sepBy1 goIdentifier goTokComma

-- | Standard @ExpressionList@
--
-- See also: SS. 9.5. Constant declarations
goExpressionList :: GoParser [GoExpr]
goExpressionList = try exprs <|> return []
  where exprs = do
         -- custom implementation of sepBy that doesn't
         -- fail on [x0 comma ... xn comma y]
         x <- goExpression
         xs <- many $ try (goTokComma >> goExpression)
         return (x:xs)

-- | Nonstandard, includes @ConstSpec@, @VarSpec@
--
-- See also: SS. 9.5. Constant declarations
--goCVSpecs :: GoParser [GoCVSpec]
--goCVSpecs = try goCVSpecs' <|> goCVSpecs'' where
--
--    goCVSpecs' = liftM (replicate 1) goCVSpec
--    goCVSpecs'' = goParen $ many $ goSemi goCVSpec
--
--    goCVSpec :: GoParser GoCVSpec
--    goCVSpec = do
--      ids <- goIdentifierList
--      try (goCVSpec' ids) <|> goCVSpec'' ids where

-- See also: SS. 9.6. Iota

-- | Standard @TypeDecl@
--
-- See also: SS. 9.7. Type declarations
goTypeDecl :: GoParser GoDecl
goTypeDecl = goTokType >> liftM GoType (goParenish goTypeSpec)

-- | Standard @TypeSpec@
--
-- See also: SS. 9.7. Type declarations
goTypeSpec :: GoParser GoTypeSpec
goTypeSpec = do
  id <- goIdentifier
  ty <- goType
  return $ GoTypeSpec id ty

-- | Standard @VarDecl@
--
-- See also: SS. 9.8. Variable declarations
goVarDecl :: GoParser GoDecl
goVarDecl = goTokVar >> liftM GoVar (goParenish goVarSpec)

goVarSpec :: GoParser GoCVSpec
goVarSpec = do
  id <- goIdentifierList
  (try (goVarSpec' id) <|> try (goVarSpec'' id) <|> goVarSpec''' id) where

    goVarSpec' :: [GoId] -> GoParser GoCVSpec
    goVarSpec' ids = do
      goTokEqual
      exs <- goExpressionList
      return $ GoCVSpec ids Nothing exs

    goVarSpec'' :: [GoId] -> GoParser GoCVSpec
    goVarSpec'' ids = do
      typ <- goType
      goTokEqual
      exs <- goExpressionList
      return $ GoCVSpec ids (Just typ) exs

    goVarSpec''' :: [GoId] -> GoParser GoCVSpec
    goVarSpec''' ids = do
      typ <- goType
      return $ GoCVSpec ids (Just typ) []

-- | Standard @ShortVarDecl@
--
-- See also: SS. 9.9. Short variable declarations
goShortVarDecl :: GoParser GoSimp
goShortVarDecl = do
  ids <- goIdentifierList
  goTokColonEq
  exs <- optionMaybe goExpressionList
  case exs of
    Just ex -> return (GoSimpVar ids ex)
    Nothing -> fail "short var"
--  return (GoSimpVar ids exs)
--             <?> "short variable declaration"

-- | Standard @FunctionDecl@
--
-- See also: SS. 9.10. Function declarations
goFunctionDecl :: GoParser GoDecl
goFunctionDecl = do
  goTokFunc
  id <- goIdentifier
  sg <- goSignature
  bk <- option GoNoBlock goBlock -- Go @Body@
  return $ GoFunc $ GoFuncDecl id sg bk

-- | Standard @MethodDecl@
--
-- See also: SS. 9.11. Method declarations
goMethodDecl :: GoParser GoDecl
goMethodDecl = do
  goTokFunc
  rc <- goReceiver
  id <- goIdentifier
  sg <- goSignature
  bk <- option GoNoBlock goBlock
  return $ GoMeth $ GoMethDecl rc id sg bk

-- | Standard @Receiver@
--
-- See also: SS. 9.11. Method declarations
goReceiver :: GoParser GoRec
goReceiver = between goTokLParen goTokRParen (try namedrec <|> anonrec)
    where namedrec = do
            -- Named receiver
            id <- goIdentifier
            pt <- optionMaybe goTokAsterisk
            ty <- goTypeName -- Go @BaseTypeName@
            return $ GoRec (isJust pt) (Just id) ty
          anonrec = do
            pt <- optionMaybe goTokAsterisk
            ty <- goTypeName -- Go @BaseTypeName@
            return $ GoRec (isJust pt) Nothing ty

-- | Standard @Operand@
--
-- See also: SS. 10.1. Operands
goOperand :: GoParser GoPrim
goOperand =  (try $ liftM GoLiteral goLiteral)
         <|> try goQualifiedIdent
         <|> try goMethodExpr
         <|> liftM GoParen (goParen goExpression)

-- | Standard @Literal@
--
-- See also: SS. 10.1. Operands
goLiteral :: GoParser GoLit
goLiteral =  goBasicLit
         <|> goCompositeLit
         <|> goFunctionLit
         <?> "literal"

-- | Standard @BasicLit@
--
-- See also: SS. 10.1. Operands
goBasicLit :: GoParser GoLit
goBasicLit = try $ do
  (GoTokenPos _ tok) <- lookAhead anyToken
  case tok of
        (GoTokInt  (Just s) t) -> do anyToken; return $ GoLitInt  s t
        (GoTokReal (Just s) t) -> do anyToken; return $ GoLitReal s t
        (GoTokImag (Just s) t) -> do anyToken; return $ GoLitImag s t
        (GoTokChar (Just s) t) -> do anyToken; return $ GoLitChar s t
        (GoTokStr  (Just s) t) -> do anyToken; return $ GoLitStr  s t
        x -> fail (show x)

-- | Standard @QualifiedIdent@
--
-- See also: SS. 10.2. Qualified identifiers
goQualifiedIdent :: GoParser GoPrim
goQualifiedIdent = try qualified <|> liftM (GoQual Nothing) goIdentifier
  where qualified = do
          qual <- goIdentifier
          goTokFullStop
          name <- goIdentifier
          return $ GoQual (Just qual) name

-- | Standard @CompositeLit@
--
-- See also: SS. 10.3. Composite literals
goCompositeLit :: GoParser GoLit
goCompositeLit = do
  st <- getState
  ty <- goLiteralType
  -- FIXME: allow T{} when parenthesized.
  case ty of
    GoTypeName _ _ -> if noComposite st && parenDepth st == 0 then fail "no T{} in condition" else return ()
    _ -> return ()
  va <- goLiteralValue
  return $ GoLitComp ty va

-- | Nonstandard
--
-- This production represents the third part of the @LiteralType@ production.
--
-- See also: SS. 10.3. Composite literals
goArrayEllipsisType :: GoParser GoType
goArrayEllipsisType = do
  goBracket goTokEllipsis
  t <- goType
  return $ GoEllipsisType t

-- | Standard @LiteralType@
--
-- See also: SS. 10.3. Composite literals
goLiteralType :: GoParser GoType
goLiteralType =  (try goArrayType)
             <|> (try goArrayEllipsisType)
             <|> goSliceType
             <|> goStructType
             <|> goMapType
             <|> goTypeName

-- | Standard @LiteralValue@
--   Standard @ElementList@
--
-- See also: SS. 10.3. Composite literals
goLiteralValue :: GoParser GoComp
goLiteralValue = do
  goTokLBrace
  elements <- sepEndBy (try goElement) goTokComma
  goTokRBrace
  return $ GoComp elements

-- | Standard @Element@
--
-- See also: SS. 10.3. Composite literals
goElement :: GoParser GoElement
goElement = do
  key <- option GoKeyNone goKey
  val <- goValue
  return $ GoElement key val

-- | Standard @Key@
-- followed by colon.
--
-- See also: SS. 10.3. Composite literals
goKey :: GoParser GoKey
goKey =  try fieldcolon -- Go @FieldName@
     <|> try keycolon   -- Go @ElementIndex@
     <?> "literal key"
  where fieldcolon = do { key <- goIdentifier; goTokColon; return $ GoKeyField key }
        keycolon = do { expr <- goExpression; goTokColon; return $ GoKeyIndex expr }

-- | Standard @Value@
--
-- See also: SS. 10.3. Composite literals
goValue :: GoParser GoValue
goValue =  liftM GoValueExpr goExpression
       <|> liftM GoValueComp goLiteralValue
       <?> "literal value"

-- | Standard @FunctionLit@
--
-- See also: SS. 10.4. Function literals
goFunctionLit :: GoParser GoLit
goFunctionLit = liftM GoLitFunc goFuncLambda

-- | Nonstandard function literals (self-contained)
goFuncLambda :: GoParser GoFuncExpr
goFuncLambda = do
  goTokFunc
  sg <- goSignature
  bk <- goBlock
  return $ GoFuncExpr sg bk

-- | Standard @PrimaryExpr@
--
-- @PrimaryExpr@ is occurs in many places:
--
-- * @Expression@,
--
-- * @TypeSwitchGuard@,
--
-- * and in its own definition.
--
-- Therefore, it is useful to have a separate datatype for it,
-- since otherwise we would have to repeat ourselves. This is
-- the responsibility @goPrimary@ below. The only thing we do
-- here is convert the AST one level, so it's an expression.
--
-- See also: SS. 10.5. Primary expressions

-- | Nonstandard primary expressions (self-contained)
goPrimaryExpr :: GoParser GoExpr
goPrimaryExpr = do
    -- Try builtin call first because the prefix looks like an
    -- identifier.
    -- goConversion only matches for unnamed types (names match
    -- goOperand).
    ex <- (try goBuiltinCall) <|> (try goOperand) <|> (try goConversion)
    let complex prefix = (try $ goIndex prefix)
                  <|> (try $ goSlice prefix)
                  <|> try (goTypeAssertion prefix)
                  <|> goCall prefix
                  <|> goSelector prefix
    let veryComplex prefix = try (do
        vex <- complex prefix
        veryComplex vex) <|> return prefix
    pr <- veryComplex ex
    return $ GoPrim pr

-- | Standard @Selector@
--
-- See also: SS. 10.5. Primary expressions, 10.6. Selectors
goSelector :: GoPrim -> GoParser GoPrim
goSelector ex = do
  goTokFullStop
  id <- goIdentifier
  return $ GoSelect ex id

-- | Standard @Index@
--
-- See also: SS. 10.5. Primary expressions, 10.7. Indexes
goIndex :: GoPrim -> GoParser GoPrim
goIndex ex = do
  goTokLBracket
  enterParen
  ix <- goExpression
  exitParen
  goTokRBracket
  return $ GoIndex ex ix

-- | Standard @Slice@
--
-- See also: SS. 10.5. Primary expressions, 10.8. Slices
goSlice :: GoPrim -> GoParser GoPrim
goSlice ex = do
  goTokLBracket
  x <- optionMaybe goExpression
  goTokColon
  y <- optionMaybe goExpression
  goTokRBracket
  return $ GoSlice ex x y

-- | Standard @TypeAssertion@
--
-- See also: SS. 10.5. Primary expressions, 10.9. Type assertions
goTypeAssertion :: GoPrim -> GoParser GoPrim
goTypeAssertion ex = do
  goTokFullStop
  goTokLParen
  ty <- goType
  goTokRParen
  return $ GoTA ex ty

-- | Standard @Call@
--
-- See also: SS. 10.5. Primary expressions, 10.10. Calls
goCall :: GoPrim -> GoParser GoPrim
goCall ex = goParen $ do
  ar <- goExpressionList
  rs <- optionMaybe goTokEllipsis
  optional goTokComma
  return $ GoCall ex ar (isJust rs)

-- | Standard @Expression@
--
-- Technically, the Go spec says
--
-- * @Expression@ = @UnaryExpr@ | @Expression@ @binary_op@ @UnaryExpr@ .
--
-- * @UnaryExpr@ = @PrimaryExpr@ | @unary_op@ @UnaryExpr@ .
--
-- but we combine these into one production here.
--
-- See also: SS. 10.12. Operators
goExpression :: GoParser GoExpr
goExpression = goOpExpr goUnaryExpr
            <?> "expression"

goUnaryExpr :: GoParser GoExpr
goUnaryExpr = unaryExpr
  where unaryExpr = try unaryOpExpr <|> goPrimaryExpr
        unaryOpExpr = do
          op <- goUnaryOp
          expr <- unaryExpr
          return $ Go1Op op expr

-- | Standard @MethodExpr@
--
-- See also: SS. 10.13. Method expressions
goMethodExpr :: GoParser GoPrim
goMethodExpr = do
  rc <- goReceiverType
  goTokFullStop
  id <- goMethodName
  return $ GoMethod rc id

-- | Nonstandard
goMethodName = goIdentifier

-- | Standard @ReceiverType@
--
-- See also: SS. 10.13. Method expressions
goReceiverType :: GoParser GoRec
goReceiverType =  try goReceiverType' <|> goReceiverType'' where

    goReceiverType'' = do
      ty <- goParen (goTokAsterisk >> goTypeName)
      return $ GoRec True Nothing ty

    goReceiverType' = do
      ty <- goTypeName
      return $ GoRec False Nothing ty

-- | Standard @Conversion@
--
-- See also: SS. 10.14. Conversions
goConversion :: GoParser GoPrim
goConversion = do
  ty <- goType
  ex <- goParen goExpression
  return $ GoCast ty ex

-- | Standard @Statement@
--
-- See also: SS. 11. Statements
goStatement :: GoParser GoStmt
goStatement =  (liftM GoStmtDecl goDeclaration)   -- 'Statement/Declaration'
           <|> try goLabeledStmt                  -- label and identifier are the same token
           <|> (liftM GoStmtSimple goSimple)      -- 'Statement/SimpleStmt'
           <|> goGoStmt
           <|> goReturnStmt
           <|> goBreakStmt
           <|> goContinueStmt
           <|> goGotoStmt
           <|> goFallthroughStmt
           <|> liftM GoStmtBlock goBlock
           <|> goIfStmt
           <|> goSwitchStmt
           <|> goSelectStmt
           <|> goForStmt
           <|> goDeferStmt
           <?> "statement"

-- | Nonstandard, TODO: remove this
goAnyStatement :: GoParser GoStmt
goAnyStatement =  goStatement
              <?> "statement within a block"

-- | Nonstandard simple statements (self-contained)
--
-- This is to wrap simple statements in a self-contained datatype.
goSimple :: GoParser GoSimp
goSimple =  (try goSendStmt)       -- 'SimpleStmt/SendStmt'
        <|> (try goIncDecStmt)     -- 'SimpleStmt/IncDecStmt'
        <|> (try goShortVarDecl)   -- 'SimpleStmt/ShortVarDecl'
        <|> (try goAssignment)     -- 'SimpleStmt/Assignment'
        <|> (liftM GoSimpExpr goExpression) -- 'SimpleStmt/ExpressionStmt'

-- | Standard @LabeledStmt@
--
-- See also: SS. 11.2. Labeled statements
goLabeledStmt :: GoParser GoStmt
goLabeledStmt = do
  id <- goIdentifier -- Go @Label@
  goTokColon
  st <- option (GoStmtSimple GoSimpEmpty) goStatement
  return $ GoStmtLabeled id st

-- | Standard @SendStmt@
--
-- See also: SS. 11.4. Send statements
goSendStmt :: GoParser GoSimp
goSendStmt = do
  ch <- goExpression -- Go @Channel@
  goTokArrow
  ex <- goExpression
  return $ GoSimpSend ch ex

-- | Standard @IncDecStmt@
--
-- See also: SS. 11.5. IncDec statements
goIncDecStmt :: GoParser GoSimp
goIncDecStmt = try $ do
  ex <- goUnaryExpr -- goExpression
  (GoTokenPos _ op) <- anyToken
  case op of
    GoTokInc -> return $ GoSimpInc ex
    GoTokDec -> return $ GoSimpDec ex
    _ -> fail "IncDecStmt What?"

-- | Standard @Assignment@
--
-- See also: SS. 11.6. Assignments
goAssignment :: GoParser GoSimp
goAssignment = do
  lv <- goExpressionList
  op <- goAssignOp
  rv <- goExpressionList
  return $ GoSimpAsn lv op rv

goNoComposite :: GoParser a -> GoParser a
goNoComposite p = do
  st <- getState -- save state
  putState $ GoParserState { noComposite = True, parenDepth = 0 }
  x <- p
  putState st -- restore state
  return x

goCond :: GoParser GoCond
goCond = goNoComposite $ do
  st <- optionMaybe (goSemi goSimple)
  ex <- optionMaybe goExpression
  return $ GoCond st ex

-- | Standard @IfStmt@
--
-- See also: SS. 11.7. If statements
goIfStmt :: GoParser GoStmt
goIfStmt = do
  goTokIf
  cond <- goCond
  case cond of
     GoCond _ Nothing -> fail "missing condition in if"
     _ -> return ()
  b <- goBlock
  e <- optionMaybe (goTokElse >> goStatement)
  return $ GoStmtIf cond b e

-- | Standard @IfStmt@
--
-- See also: SS. 11.8. Switch statements
goSwitchStmt :: GoParser GoStmt
goSwitchStmt = try goExprSwitchStmt
           <|> goTypeSwitchStmt

-- | Standard @ExprSwitchStmt@
--
-- See also: SS. 11.8. Switch statements
goExprSwitchStmt :: GoParser GoStmt
goExprSwitchStmt = do
  goTokSwitch
  cond <- goCond
  cl <- goBrace $ many goExprCaseClause
  return $ GoStmtSwitch cond cl

-- | Standard @ExprCaseClause@
--
-- See also: SS. 11.8. Switch statements
goExprCaseClause :: GoParser (GoCase GoExpr)
goExprCaseClause = do
  fn <- goAfter goTokColon goExprSwitchCase
  st <- many $ goSemi goStatement
  return $ fn st

-- | Standard @ExprSwitchCase@
--
-- See also: SS. 11.8. Switch statements
goExprSwitchCase :: GoParser ([GoStmt] -> GoCase GoExpr)
goExprSwitchCase = goExprSwitchCase' <|> goExprSwitchCase''
  where goExprSwitchCase' = do goTokCase; el <- goExpressionList; return $ GoCase el
        goExprSwitchCase'' = do goTokDefault; return GoDefault

-- | Standard @TypeSwitchStmt@
goTypeSwitchStmt :: GoParser GoStmt
goTypeSwitchStmt = do
  goTokSwitch
  st <- optionMaybe $ goSemi goSimple
  id <- optionMaybe $ goAfter goTokColonEq goIdentifier
  ga <- goTypeSwitchGuard st
  cl <- goBrace $ many goTypeCaseClause
  return $ GoStmtTypeSwitch ga cl id

-- | Standard @TypeSwitchGuard@
goTypeSwitchGuard :: (Maybe GoSimp) -> GoParser GoCond
goTypeSwitchGuard st = do
  ex <- goExpression
  goTokFullStop
  goParen goTokType
  return $ GoCond st (Just ex)

-- | Standard @TypeSwitchCase@
goTypeCaseClause :: GoParser (GoCase GoType)
goTypeCaseClause = do
  fn <- goAfter goTokColon goTypeSwitchCase
  st <- many $ goSemi goStatement
  return $ fn st

-- | Standard @TypeSwitchCase@
goTypeSwitchCase :: GoParser ([GoStmt] -> GoCase GoType)
goTypeSwitchCase = goTypeSwitchCase' <|> goTypeSwitchCase''
  where goTypeSwitchCase' = do goTokCase; tl <- goTypeList; return $ GoCase tl
        goTypeSwitchCase'' = do goTokDefault; return GoDefault

-- | Standard @TypeList@
goTypeList :: GoParser [GoType]
goTypeList = sepBy1 goType goTokComma

-- | Standard @ForStmt@
--
-- See also: SS. 11.9. For statements
goForStmt :: GoParser GoStmt
goForStmt = do
  goTokFor
  h <- goNoComposite (try goForClause <|> try goRangeClause <|> goCondition)
  b <- goBlock
  return $ GoStmtFor h b

-- | Standard @Condition@
goCondition :: GoParser GoForClause
goCondition = liftM GoForWhile (optionMaybe goExpression)

-- | Standard @ForClause@
goForClause :: GoParser GoForClause
goForClause = do
  i <- option GoSimpEmpty goSimple -- Go @InitStmt@
  goTokSemicolon
  c <- optionMaybe goExpression
  goTokSemicolon
  p <- option GoSimpEmpty goSimple -- Go @PostStmt@
  return $ GoForThree i c p

-- | Standard @RangeClause@
goRangeClause :: GoParser GoForClause
goRangeClause = do
  k <- goExpression
  v <- optionMaybe (goTokComma >> goUnaryExpr)
  p <- goAnyEqual
  goTokRange
  e <- goExpression
  let lhs = case v of { Nothing -> [k]; Just v -> [k,v] }
  return $ GoForRange lhs e (p == GoOp ":=")

-- Nonstandard
goAnyEqual :: GoParser GoOp
goAnyEqual =  do goTokEqual;   return $ GoOp "="
          <|> do goTokColonEq; return $ GoOp ":="

-- | Standard @GoStmt@
--
-- See also: SS. 11.10. Go statements
goGoStmt :: GoParser GoStmt
goGoStmt = do goTokGo; liftM GoStmtGo goExpression

-- | Standard @SelectStmt@
--
-- See also: SS. 11.11. Select statements
goSelectStmt :: GoParser GoStmt
goSelectStmt = do
  goTokSelect
  cl <- goBrace $ many goCommClause
  return $ GoStmtSelect cl

-- | Standard @CommClause@
--
-- See also: SS. 11.11. Select statements
goCommClause :: GoParser (GoCase GoChan)
goCommClause = do
  fn <- goAfter goTokColon goCommCase
  st <- many $ goSemi goStatement
  return $ fn st

-- | Standard @CommCase@
--
-- See also: SS. 11.11. Select statements
goCommCase :: GoParser ([GoStmt] -> GoCase GoChan)
goCommCase = goCommCase' <|> goCommCase''
  where goCommCase' = do goTokCase; ch <- goChanStmt; return $ GoCase [ch]
        goCommCase'' = do goTokDefault; return GoDefault

-- | Nonstandard
goChanStmt :: GoParser GoChan
goChanStmt = try (goSendStmt >>= convert) <|> goRecvStmt
    where convert (GoSimpSend x y) = return (GoChanSend x y)

-- | Standard @RecvStmt@
--
-- See also: SS. 11.11. Select statements
goRecvStmt :: GoParser GoChan
goRecvStmt = do
  as <- optionMaybe $ try (do
               ex <- goExpression
               ex2 <- optionMaybe (goTokComma >> goExpression)
               op <- goAnyEqual
               return (ex, ex2, op))
  re <- goRecvExpr
  return $ GoChanRecv as re

-- | Standard @RecvExpr@
--
-- See also: SS. 11.11. Select statements
goRecvExpr :: GoParser GoExpr
goRecvExpr =  try goRecvExpr'
          <|> goParen goRecvExpr where

    goRecvExpr' = do
      goTokArrow
      ex <- goExpression
      return $ Go1Op (GoOp "<-") ex

-- | Standard @ReturnStmt@
--
-- See also: SS. 11.12. Return statements
goReturnStmt :: GoParser GoStmt
goReturnStmt = do goTokReturn; liftM GoStmtReturn $ option [] goExpressionList

-- | Standard @BreakStmt@
--
-- See also: SS. 11.13. Break statements
goBreakStmt :: GoParser GoStmt
goBreakStmt = do goTokBreak; liftM GoStmtBreak $ optionMaybe goIdentifier

-- | Standard @ContinueStmt@
--
-- See also: SS. 11.14. Continue statementss
goContinueStmt :: GoParser GoStmt
goContinueStmt = do goTokContinue; liftM GoStmtContinue $ optionMaybe goIdentifier

-- | Standard @GotoStmt@
--
-- See also: SS. 11.15. Goto statements
goGotoStmt :: GoParser GoStmt
goGotoStmt = do goTokGoto; liftM GoStmtGoto $ goIdentifier

-- | Standard @FallthroughStmt@
--
-- See also: SS. 11.16. Fallthrough statements
goFallthroughStmt :: GoParser GoStmt
goFallthroughStmt = goTokFallthrough >> return GoStmtFallthrough

-- | Standard @DeferStmt@
--
-- See also: SS. 11.17. Defer statements
goDeferStmt :: GoParser GoStmt
goDeferStmt = goTokDefer >> liftM GoStmtDefer goExpression

-- | Standard @BuiltinCall@
--
-- See also: SS. 12. Built-in functions, 12.3. Allocation
goBuiltinCall :: GoParser GoPrim
goBuiltinCall = do
  id <- goIdentifier
  goTokLParen
  tj <- optionMaybe goType
  ar <- option [] goBuiltinArgs
  goTokRParen
  case (id, tj) of
    (GoId "new",  Just ty) -> return $ GoNew  ty
    (GoId "make", Just ty) -> return $ GoMake ty ar
    _ -> fail "BuiltinCall what?"

-- | Standard @BuiltinArgs@
--
-- See also: SS. 12. Built-in functions
goBuiltinArgs :: GoParser [GoExpr]
goBuiltinArgs = goTokComma >> goExpressionList

-- | Standard @SourceFile@
--
-- See also: SS. 13.1. Source file organization
goSource :: GoParser GoSource
goSource = do
  pkg <- goSemi goPackageClause
  imp <- many $ goSemi goImportDecl
  top <- many $ goSemi goTopLevelDecl
  eof
  return $ GoSource pkg imp top

-- | Standard @PackageClase@
--
-- See also: SS. 13.2. Package clause
goPackageClause :: GoParser GoId
goPackageClause = do
  goTokPackage
  goIdentifier

-- | Standard @ImportDecl@
--
-- See also: SS. 13.3. Import declarations
goImportDecl :: GoParser GoPrel
goImportDecl = goTokImport >> liftM GoImportDecl goImportSpec

-- | List of specs separated by semicolons, in parentheses.
-- See also goBlockish.
goParenish :: GoParser a -> GoParser [a]
goParenish x = goParen xs <|> one
  where one = do { t <- x; return [t] }
        xs = do
          lines <- sepEndBy (optionMaybe x) goTokSemicolon
          return $ catMaybes lines

-- | Standard @ImportSpec@
--
-- See also: SS. 13.3. Import declarations
goImportSpec :: GoParser [GoImpSpec]
goImportSpec = goImpSpecs'' <|> goImpSpecs' where

    goImpSpecs' = liftM (replicate 1) goImpSpec
    goImpSpecs'' = goParen $ many $ goSemi goImpSpec

    goImpSpec :: GoParser GoImpSpec
    goImpSpec = do
      ty <- try goImpType
      ip <- goString -- Go @ImportPath@
      return $ GoImpSpec ty ip

    goImpType :: GoParser GoImpType
    goImpType = try dot <|> try qual <|> return GoImp
      where dot = do { goTokFullStop; return GoImpDot }
            qual = do { id <- goIdentifier; return $ GoImpQual id }

--
--  End specification grammar
--

-- combinators

goAfter :: GoParser b -> GoParser a -> GoParser a
goAfter y x = try (do z <- x ; y ; return z)

goSemi :: GoParser a -> GoParser a
goSemi = goAfter goTokSemicolon

-- literals

goString :: GoParser String
goString = do
  GoTokStr rep uni <- token (GoTokStr Nothing "")
  return uni

goParen :: GoParser a -> GoParser a
goParen p = do
  goTokLParen
  enterParen
  x <- p
  exitParen
  goTokRParen
  return x

goBrace :: GoParser a -> GoParser a
goBrace = between goTokLBrace goTokRBrace

goBracket :: GoParser a -> GoParser a
goBracket = between goTokLBracket goTokRBracket

-- parsers
-- only in TypeLit:
-- Pointer
-- InterfaceType
-- ChannelType
-- FunctionType (available as FunctionLit)
-- only in LiteralType:
-- goArrayEllipsisType
