module Main where

import System.Environment
import Language.Go.Parser.Parser (goParse)

main = do
  [filename] <- getArgs
  source <- readFile filename
  let ast = goParse filename source
  case ast of
    Left x -> putStrLn $ "ERROR:" ++ filename ++ ":" ++ (show x)
    Right x -> do
        putStrLn "["
        putStr $ show x
        putStrLn ""
        putStrLn "]"
