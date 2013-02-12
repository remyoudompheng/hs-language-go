module Language.Go.Parser.Operators (
  goOpExpr,
  goUnaryOp
) where

import Text.Parsec.Expr
import Text.Parsec.Prim (try, token)
import Text.Parsec.Combinator (anyToken)

import Language.Go.Parser.Tokens (GoParser,GoToken(..),GoTokenPos(..))
import Language.Go.Syntax.AST

-- | @goOpExpr p@ returns a parser for expressions with 
-- binary operators whose terms are parsed by @p@.
goOpExpr :: GoParser GoExpr -> GoParser GoExpr
goOpExpr p = buildExpressionParser goOpTable p

-- Unary operators cannot be stacked when using
-- buildExpressionParser so we parse them separately
-- in goUnaryExpr.
goOpTable =
  [ [ Infix (goBinaryOp mul_op) AssocLeft ]
  , [ Infix (goBinaryOp add_op) AssocLeft ]
  , [ Infix (goBinaryOp rel_op) AssocLeft ]
  , [ Infix (goBinaryOp and_op) AssocLeft ]
  , [ Infix (goBinaryOp or_op) AssocLeft ]
  ]

-- | @goUnaryOp@ parse a unary (prefix) operator.
--
goUnaryOp :: GoParser GoOp
goUnaryOp = do
  -- unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-"
  GoTokenPos _ tok <- anyToken
  case tok of
    GoTokPlus    -> return $ GoOp "+"
    GoTokMinus   -> return $ GoOp "-"
    GoTokExclaim -> return $ GoOp "!"
    GoTokXOR     -> return $ GoOp "^"
    GoTokAsterisk-> return $ GoOp "*"
    GoTokAND     -> return $ GoOp "&"
    GoTokArrow   -> return $ GoOp "<-"
    _ -> fail "not a unary operator"

goBinaryOp :: (GoToken -> Maybe String) -> GoParser (GoExpr -> GoExpr -> GoExpr)
goBinaryOp want = try $ do
  -- binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
  -- rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
  -- add_op     = "+" | "-" | "|" | "^" .
  -- mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
  let s (GoTokenPos _ t) = show t
      pos (GoTokenPos pos _) = pos
      match (GoTokenPos _ t) = want t
  op <- token s pos match
  return (Go2Op $ GoOp op)

mul_op :: GoToken -> Maybe String
mul_op op = case op of
  GoTokAsterisk -> Just "*"
  GoTokSolidus  -> Just "/"
  GoTokPercent  -> Just "%"
  GoTokSHL      -> Just "<<"
  GoTokSHR      -> Just ">>"
  GoTokAND      -> Just "&"
  GoTokBUT      -> Just "&^"
  _ -> Nothing
 
add_op :: GoToken -> Maybe String
add_op op = case op of
  GoTokPlus  -> Just "+"
  GoTokMinus -> Just "-"
  GoTokIOR   -> Just "|"
  GoTokXOR   -> Just "^"
  _ -> Nothing

rel_op :: GoToken -> Maybe String
rel_op op = case op of
  GoTokEQ -> Just "=="
  GoTokNE -> Just "!="
  GoTokLT -> Just "<" 
  GoTokLE -> Just "<="
  GoTokGT -> Just ">" 
  GoTokGE -> Just ">="
  _ -> Nothing

or_op  op = if op == GoTokLOR  then Just "||" else Nothing 
and_op op = if op == GoTokLAND then Just "&&" else Nothing 


