module Language.Go.Parser.Operators (goOpExpr) where

import Text.Parsec.Expr

import Language.Go.Parser.Tokens
import Language.Go.Syntax.AST

-- | @goOpExpr p@ returns a parser for expressions with operators
-- whose terms are parsed by @p@.
goOpExpr :: GoParser GoExpr -> GoParser GoExpr
goOpExpr p = buildExpressionParser goOpTable p

goOpTable =
  [ [ Prefix (goTokPlus  (Go1Op))
    , Prefix (goTokMinus (Go1Op))
    , Prefix (goTokExclaim      )
    , Prefix (goTokAND   (Go1Op))
    , Prefix (goTokXOR   (Go1Op))
    , Prefix (goTokStar  (Go1Op))
    , Prefix (goTokRecv         ) ]
  , [ Infix  (goTokStar  (Go2Op)) AssocLeft
    , Infix  (goTokSolidus      ) AssocLeft
    , Infix  (goTokPercent      ) AssocLeft
    , Infix  (goTokSHL          ) AssocLeft
    , Infix  (goTokSHR          ) AssocLeft
    , Infix  (goTokAND   (Go2Op)) AssocLeft
    , Infix  (goTokBUT          ) AssocLeft ]
  , [ Infix  (goTokPlus  (Go2Op)) AssocLeft
    , Infix  (goTokMinus (Go2Op)) AssocLeft
    , Infix  (goTokIOR          ) AssocLeft
    , Infix  (goTokXOR   (Go2Op)) AssocLeft ]
  , [ Infix  (goTokEQ           ) AssocLeft
    , Infix  (goTokNE           ) AssocLeft
    , Infix  (goTokLT           ) AssocLeft
    , Infix  (goTokLE           ) AssocLeft
    , Infix  (goTokGT           ) AssocLeft
    , Infix  (goTokGE           ) AssocLeft ]
  , [ Infix  (goTokLAND         ) AssocLeft ]
  , [ Infix  (goTokLOR          ) AssocLeft ]
  ]

-- BEGIN operators
goTokLOR      = do token GoTokLOR      ; return$Go2Op$GoOp "||" -- '||'
goTokLAND     = do token GoTokLAND     ; return$Go2Op$GoOp "&&" -- '&&'
goTokEQ       = do token GoTokEQ       ; return$Go2Op$GoOp "==" -- '=='
goTokNE       = do token GoTokNE       ; return$Go2Op$GoOp "!=" -- '!='
goTokLT       = do token GoTokLT       ; return$Go2Op$GoOp "<"  -- '<'
goTokLE       = do token GoTokLE       ; return$Go2Op$GoOp "<=" -- '<='
goTokGT       = do token GoTokGT       ; return$Go2Op$GoOp ">"  -- '>'
goTokGE       = do token GoTokGE       ; return$Go2Op$GoOp ">=" -- '>='
goTokPlus   f = do token GoTokPlus     ; return$  f  $GoOp "+"  -- '+'
goTokMinus  f = do token GoTokMinus    ; return$  f  $GoOp "-"  -- '-'
goTokIOR      = do token GoTokIOR      ; return$Go2Op$GoOp "|"  -- '|'
goTokXOR    f = do token GoTokXOR      ; return$  f  $GoOp "^"  -- '^'
goTokStar   f = do token GoTokAsterisk ; return$  f  $GoOp "*"  -- '*'
goTokSolidus  = do token GoTokSolidus  ; return$Go2Op$GoOp "/"  -- '/'
goTokPercent  = do token GoTokPercent  ; return$Go2Op$GoOp "%"  -- '%'
goTokSHL      = do token GoTokSHL      ; return$Go2Op$GoOp "<<" -- '<<'
goTokSHR      = do token GoTokSHR      ; return$Go2Op$GoOp ">>" -- '>>'
goTokAND    f = do token GoTokAND      ; return$  f  $GoOp "&"  -- '&'
goTokBUT      = do token GoTokBUT      ; return$Go2Op$GoOp "&^" -- '&^'
goTokExclaim  = do token GoTokExclaim  ; return$Go1Op$GoOp "!"  -- '!'
goTokRecv     = do token GoTokArrow    ; return$Go1Op$GoOp "<-" -- '<-'
-- END operators


