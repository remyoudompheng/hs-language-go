module Main where

import Data.List
import System.Environment

import Language.Go.Parser.Lexer
import Language.Go.Parser.Parser
import Language.Go.Parser.Tokens
import Language.Go.Pretty

import Text.PrettyPrint (render)

main = do
  [filename] <- getArgs
  source <- readFile filename
  let ast = goParse filename source
  case ast of
    Left x -> putStrLn $ "ERROR:" ++ filename ++ ":" ++ (show x)
    Right x -> putStr $ render $ pretty x

