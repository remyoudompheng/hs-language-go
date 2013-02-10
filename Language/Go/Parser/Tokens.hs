-- |
-- Module      : Language.Go.Parser.Tokens
-- Copyright   : (c) 2011 Andrew Robbins
-- License     : GPLv3 (see COPYING)
--
-- x
module Language.Go.Parser.Tokens where

import Numeric (readHex)
import Data.Maybe (mapMaybe)
import Data.Char (chr)

import Language.Go.Syntax.AST
import Text.Parsec.String
import Text.Parsec.Prim hiding (token)
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Pos (SourcePos, SourceName)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator

-- | GoTokener is the type used for all tokenizers
-- type GoTokener = GenParser Char () [GoToken]

-- | GoParser is the type used for all parsers
type GoParser a = GenParser GoTokenPos GoParserState a

data GoParserState = GoParserState { noComposite :: Bool, parenDepth :: Int }

runGoParser :: GoParser a -> SourceName -> [GoTokenPos] -> Either ParseError a
runGoParser p = runP p $ GoParserState { noComposite = False, parenDepth = 0 }

enterParen :: GoParser ()
enterParen = do
  st <- getState
  putState $ st { parenDepth = (parenDepth st) + 1 }

exitParen :: GoParser ()
exitParen = do
  st <- getState
  let newst = st { parenDepth = (parenDepth st) - 1 }
  if parenDepth newst < 0 then fail "negative paren depth" else return ()
  putState newst

-- | GoTokenPos encodes tokens and source positions
data GoTokenPos = GoTokenPos !SourcePos !GoToken
                  deriving (Eq, Show)

-- | GoToken encodes tokens
data GoToken = GoTokNone
             | GoTokComment Bool String -- False=singleline True=multiline
-- BEGIN literals
             | GoTokInt  (Maybe String) Integer
             | GoTokReal (Maybe String) Float
             | GoTokImag (Maybe String) Float
             | GoTokChar (Maybe String) Char
             | GoTokStr  (Maybe String) String
-- END literals
-- BEGIN wraps
             | GoTokLParen   -- '('
             | GoTokRParen   -- ')'
             | GoTokLBrace   -- '{'
             | GoTokRBrace   -- '}'
             | GoTokLBracket -- '['
             | GoTokRBracket -- ']'
-- END wraps
-- BEGIN keywords
             | GoTokBreak
             | GoTokCase
             | GoTokChan
             | GoTokConst
             | GoTokContinue
             | GoTokDefault
             | GoTokDefer
             | GoTokElse
             | GoTokFallthrough
             | GoTokFor
             | GoTokFunc
             | GoTokGo
             | GoTokGoto
             | GoTokIf
             | GoTokImport
             | GoTokInterface
             | GoTokMap
             | GoTokPackage
             | GoTokRange
             | GoTokReturn
             | GoTokSelect
             | GoTokStruct
             | GoTokSwitch
             | GoTokType
             | GoTokVar
-- END keywords
             | GoTokSemicolonAuto
             | GoTokSemicolon -- ';'
             | GoTokColon     -- ':'
             | GoTokColonEq   -- ':='
             | GoTokEqual     -- '='
             | GoTokComma     -- ','
             | GoTokFullStop  -- '.'
             | GoTokEllipsis   -- '...'
-- BEGIN operators
             | GoTokLOR       -- '||'
             | GoTokLAND      -- '&&'
             | GoTokEQ        -- '=='
             | GoTokNE        -- '!='
             | GoTokLT        -- '<'
             | GoTokLE        -- '<='
             | GoTokGT        -- '>'
             | GoTokGE        -- '>='
             | GoTokPlus      -- '+'
             | GoTokMinus     -- '-'
             | GoTokIOR       -- '|'
             | GoTokXOR       -- '^'
             | GoTokAsterisk  -- '*'
             | GoTokSolidus   -- '/'
             | GoTokPercent   -- '%'
             | GoTokSHL       -- '<<'
             | GoTokSHR       -- '>>'
             | GoTokAND       -- '&'
             | GoTokBUT       -- '&^'
             | GoTokExclaim   -- '!'
             | GoTokArrow     -- '<-'
             | GoTokDec       -- '--'
             | GoTokInc       -- '++'
-- END operators
-- BEGIN names
             | GoTokId String
             | GoTokOp String -- future extensions
-- END names
             | GoTokInvalid String
               deriving (Eq, Read, Show)
-- Data, Typeable

-- False=singleline True=multiline
tokenFromComment :: Bool -> String -> GoToken
tokenFromComment False s = GoTokComment False $ drop 2 $ init s -- strip // and \n
tokenFromComment True  s = GoTokComment True  $ drop 2 $ init $ init s -- strip /* and */

tokenFromInt :: String -> GoToken
tokenFromInt s = GoTokInt (Just s) $ ((read s) :: Integer)

tokenFromReal :: String -> GoToken
tokenFromReal s = GoTokReal (Just s) $ (read s)

tokenFromImag :: String -> GoToken
tokenFromImag s = GoTokImag (Just s) $ (read $ init s)

tokenFromRawStr :: String -> GoToken
tokenFromRawStr s = GoTokStr (Just s) (init $ tail s)

tokenFromString :: String -> GoToken
tokenFromString s = case unquoteString $ init $ tail s of
                      Just q -> GoTokStr (Just s) q
                      Nothing -> GoTokInvalid s

-- | @tokenFromChar c@ unquotes the Go representation of a single
-- character literal, including the single quotes.
tokenFromChar :: String -> GoToken
tokenFromChar s =
  case c of
    Just c  -> GoTokChar (Just s) c
    Nothing -> GoTokInvalid s
  where c = unquoteChar $ init $ tail s

unquoteChar :: String -> Maybe Char
unquoteChar ['\\', c] = case c of
  'a'  -> Just '\a'
  'b'  -> Just '\b'
  'f'  -> Just '\f'
  'n'  -> Just '\n'
  'r'  -> Just '\r'
  't'  -> Just '\t'
  'v'  -> Just '\v'
  '\'' -> Just '\''
  '\"' -> Just '\"'
  '\\' -> Just '\\'
  _    -> Nothing
unquoteChar s = case s of
  ['\\','x', a, b] -> hex [a, b]
  ['\\', 'u', a, b, c, d] -> hex [a,b,c,d]
  ['\\', 'U', a, b, c, d, e, f, g, h] -> hex [a,b,c,d,e,f,g,h]
  [c] -> Just c
  _ -> Nothing
 where hex s = case readHex s of
                 ((n, _):ns) -> Just $ chr n
                 _ -> Nothing

unquoteString :: String -> Maybe String
unquoteString s = fmap reverse v
  where (_, v) = unquote s (Just "")
        unquote :: String -> Maybe String -> (String, Maybe String)
        unquote "" accum = ("", accum)
        unquote s Nothing = (s, Nothing)
        unquote s (Just accum) = case c of Just c -> unquote s' $ Just (c:accum); Nothing -> (s, Nothing)
         where (c, s') = case s of
                 ('\\':'x':_) -> (unquoteChar $ take 4 s, drop 4 s)
                 ('\\':'u':_) -> (unquoteChar $ take 6 s, drop 6 s)
                 ('\\':'U':_) -> (unquoteChar $ take 10 s, drop 10 s)
                 ('\\':_)     -> (unquoteChar $ take 2 s, drop 2 s)
                 (c:ss)        -> (Just c, ss)

tokenEq :: GoToken -> GoToken -> Bool
tokenEq (GoTokComment _ _) (GoTokComment _ _) = True
tokenEq (GoTokInt _ _)     (GoTokInt _ _) = True
tokenEq (GoTokReal   _ _)  (GoTokReal   _ _) = True
tokenEq (GoTokImag  _ _)   (GoTokImag  _ _) = True
tokenEq (GoTokId _) (GoTokId _) = True
tokenEq (GoTokOp _) (GoTokOp _) = True
tokenEq a b = a == b

token :: GoToken -> GoParser GoToken
token tok = Prim.token showTok posnTok testTok
    where showTok (GoTokenPos pos t) = show t
          posnTok (GoTokenPos pos t) = pos
          testTok (GoTokenPos pos t) = if tokenEq tok t
                                       then Just t
                                       else Nothing

stripComments :: [GoTokenPos] -> [GoTokenPos]
stripComments tokens = mapMaybe nocomm tokens where
    nocomm tok = case tok of
        -- single line comment: restore newline
        GoTokenPos pos (GoTokComment False _) -> Just $ GoTokenPos pos GoTokSemicolonAuto
        GoTokenPos _ (GoTokComment True _) -> Nothing
        _ -> Just tok

stripNone :: [GoTokenPos] -> [GoTokenPos]
stripNone tokens = filter nonull tokens where
    nonull (GoTokenPos _ x) = (x /= GoTokNone)

stripAuto :: [GoTokenPos] -> [GoTokenPos]
stripAuto tokens = filter nonull tokens where
    nonull (GoTokenPos _ x) = (x /= GoTokSemicolonAuto)

needSemi :: GoToken -> Bool
needSemi token = case token of
                     GoTokId _        -> True
                     GoTokInt  _ _    -> True
                     GoTokReal _ _    -> True
                     GoTokImag _ _    -> True
                     GoTokChar _ _    -> True
                     GoTokStr  _ _    -> True
                     GoTokBreak       -> True
                     GoTokContinue    -> True
                     GoTokFallthrough -> True
                     GoTokReturn      -> True
                     GoTokDec         -> True
                     GoTokInc         -> True
                     GoTokRParen      -> True
                     GoTokRBrace      -> True
                     GoTokRBracket    -> True
                     _ -> False

appendSemi :: [GoTokenPos] -> [GoTokenPos]
appendSemi tokens = tokens ++ semi where
    semi = [GoTokenPos (lastpos $ last tokens) GoTokSemicolonAuto]
    lastpos (GoTokenPos pos _) = pos

insertSemi :: [GoTokenPos] -> [GoTokenPos]
insertSemi = stripAuto . stripNone . 
             insertAfter . stripNone . appendSemi 

--insertSemi = insertAfter . stripNone . insertBefore . appendSemi

insertAfter :: [GoTokenPos] -> [GoTokenPos]
insertAfter [] = []
insertAfter (xt:[]) = xt:[]
insertAfter ((xt@(GoTokenPos _ x)):(yt@(GoTokenPos yp y)):zs) = xt:(insertAfter ((repl y):zs))
    where cond = if needSemi x then GoTokSemicolon else GoTokNone
          repl GoTokSemicolonAuto = GoTokenPos yp cond
          repl _ = yt

-- token parsers

goTokLParen   = token $ GoTokLParen
goTokRParen   = token $ GoTokRParen
goTokLBrace   = token $ GoTokLBrace
goTokRBrace   = token $ GoTokRBrace
goTokLBracket = token $ GoTokLBracket
goTokRBracket = token $ GoTokRBracket

goTokSemicolon= token $ GoTokSemicolon -- ';'
goTokColon    = token $ GoTokColon     -- ':'
goTokColonEq  = token $ GoTokColonEq   -- ':='
goTokEqual    = token $ GoTokEqual     -- '='
goTokComma    = token $ GoTokComma     -- ','
goTokFullStop = token $ GoTokFullStop  -- '.'
goTokEllipsis = token $ GoTokEllipsis  -- '...'

goTokAsterisk = token $ GoTokAsterisk
goTokArrow    = token $ GoTokArrow

-- BEGIN keywords
goTokBreak    = token $ GoTokBreak
goTokCase     = token $ GoTokCase
goTokChan     = token $ GoTokChan
goTokConst    = token $ GoTokConst
goTokContinue = token $ GoTokContinue
goTokDefault  = token $ GoTokDefault
goTokDefer    = token $ GoTokDefer
goTokElse     = token $ GoTokElse
goTokFallthrough = token $ GoTokFallthrough
goTokFor      = token $ GoTokFor
goTokFunc     = token $ GoTokFunc
goTokGo       = token $ GoTokGo
goTokGoto     = token $ GoTokGoto
goTokIf       = token $ GoTokIf
goTokImport   = token $ GoTokImport
goTokInterface= token $ GoTokInterface
goTokMap      = token $ GoTokMap
goTokPackage  = token $ GoTokPackage
goTokRange    = token $ GoTokRange
goTokReturn   = token $ GoTokReturn
goTokSelect   = token $ GoTokSelect
goTokStruct   = token $ GoTokStruct
goTokSwitch   = token $ GoTokSwitch
goTokType     = token $ GoTokType
goTokVar      = token $ GoTokVar
-- END keywords

goIdentifier :: GoParser GoId
goIdentifier = do
  GoTokId name <- token $ GoTokId ""
  return $ GoId name

goOperator :: GoParser GoOp
goOperator = do
  GoTokOp name <- token $ GoTokOp ""
  return $ GoOp name

-- | Standard @assign_op@
--
-- See also: SS. 11.6. Assignments
goAssignOp :: GoParser GoOp
goAssignOp = try $ do
  (GoTokenPos _ op) <- lookAhead anyToken
  case op of
    GoTokOp opname -> if last opname == '='
                      then goOperator
                      else fail "Assignment What?"
    GoTokEqual     -> do anyToken; return (GoOp "=")
    x -> unexpected (show x)
  
