module Main where

import System.Environment
import Language.Go.Parser.Lexer
import Language.Go.Parser.Parser (goTokenize)
import Language.Go.Parser.Tokens
import Data.List

showToken :: GoTokenPos -> String
showToken (GoTokenPos _ x) = (show x)

main = do
  [filename] <- getArgs
  source <- readFile filename
  let tokens = goTokenize source
  putStrLn "["
  mapM (putStrLn . showToken) tokens
  putStrLn "]"
  return ()