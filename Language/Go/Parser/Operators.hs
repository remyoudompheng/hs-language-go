module Language.Go.Parser.Operators (
  goOpExpr,
  goUnaryOp
) where

import Text.Parsec.Expr
import Text.Parsec.Combinator (anyToken)

import Language.Go.Parser.Tokens
import Language.Go.Syntax.AST

-- | @goOpExpr p@ returns a parser for expressions with 
-- binary operators whose terms are parsed by @p@.
goOpExpr :: GoParser GoExpr -> GoParser GoExpr
goOpExpr p = buildExpressionParser goOpTable p

-- Unary operators cannot be stacked when using
-- buildExpressionParser so we parse them separately
-- in goUnaryExpr.
goOpTable =
  [ [ Infix goTokStar    AssocLeft
    , Infix goTokSolidus AssocLeft
    , Infix goTokPercent AssocLeft
    , Infix goTokSHL     AssocLeft
    , Infix goTokSHR     AssocLeft
    , Infix goTokAND     AssocLeft
    , Infix goTokBUT     AssocLeft ]
  , [ Infix goTokPlus    AssocLeft
    , Infix goTokMinus   AssocLeft
    , Infix goTokIOR     AssocLeft
    , Infix goTokXOR     AssocLeft ]
  , [ Infix goTokEQ      AssocLeft
    , Infix goTokNE      AssocLeft
    , Infix goTokLT      AssocLeft
    , Infix goTokLE      AssocLeft
    , Infix goTokGT      AssocLeft
    , Infix goTokGE      AssocLeft ]
  , [ Infix goTokLAND    AssocLeft ]
  , [ Infix goTokLOR     AssocLeft ]
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

-- BEGIN operators
goTokLOR      = do token GoTokLOR      ; return$Go2Op$GoOp "||" -- '||'
goTokLAND     = do token GoTokLAND     ; return$Go2Op$GoOp "&&" -- '&&'
goTokEQ       = do token GoTokEQ       ; return$Go2Op$GoOp "==" -- '=='
goTokNE       = do token GoTokNE       ; return$Go2Op$GoOp "!=" -- '!='
goTokLT       = do token GoTokLT       ; return$Go2Op$GoOp "<"  -- '<'
goTokLE       = do token GoTokLE       ; return$Go2Op$GoOp "<=" -- '<='
goTokGT       = do token GoTokGT       ; return$Go2Op$GoOp ">"  -- '>'
goTokGE       = do token GoTokGE       ; return$Go2Op$GoOp ">=" -- '>='
goTokPlus     = do token GoTokPlus     ; return$Go2Op$GoOp "+"  -- '+'
goTokMinus    = do token GoTokMinus    ; return$Go2Op$GoOp "-"  -- '-'
goTokIOR      = do token GoTokIOR      ; return$Go2Op$GoOp "|"  -- '|'
goTokXOR      = do token GoTokXOR      ; return$Go2Op$GoOp "^"  -- '^'
goTokStar     = do token GoTokAsterisk ; return$Go2Op$GoOp "*"  -- '*'
goTokSolidus  = do token GoTokSolidus  ; return$Go2Op$GoOp "/"  -- '/'
goTokPercent  = do token GoTokPercent  ; return$Go2Op$GoOp "%"  -- '%'
goTokSHL      = do token GoTokSHL      ; return$Go2Op$GoOp "<<" -- '<<'
goTokSHR      = do token GoTokSHR      ; return$Go2Op$GoOp ">>" -- '>>'
goTokAND      = do token GoTokAND      ; return$Go2Op$GoOp "&"  -- '&'
goTokBUT      = do token GoTokBUT      ; return$Go2Op$GoOp "&^" -- '&^'
-- END operators


