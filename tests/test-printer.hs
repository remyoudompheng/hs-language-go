module Main where

import System.Environment
import System.Exit

import Language.Go.Parser.Parser
import Language.Go.Pretty
import Language.Go.Syntax.AST

import Text.PrettyPrint (render)

main = do
  [filename, wantPrint] <- getArgs
  source <- readFile filename
  let ast = goParse filename source
  case ast of
    Left x -> (putStrLn $ "ERROR:" ++ filename ++ ":" ++ (show x)) >> exitFailure
    Right x -> check filename x (read wantPrint :: Bool)

check :: String -> GoSource -> Bool -> IO ()
check filename x printit = do
      let printed = render (pretty x)
      if printit then
          putStr printed
      else do
          let ast2 = goParse filename printed
          case ast2 of
              Left x -> (putStrLn $ "ERROR: cannot parse prettied " ++ filename ++ ":" ++ (show x)) >> exitFailure
              Right y ->
                 if not (x == y) then
                     putStrLn ("ERROR:" ++ filename ++ ": pretty printed does not match") >> exitFailure
                 else
                     return ()


