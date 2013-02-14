module Main where

import System.Environment
import System.Exit

import Language.Go.Parser.Parser
import Language.Go.Pretty
import Language.Go.Syntax.AST

import Text.PrettyPrint (render)

main = do
  [filename] <- getArgs
  source <- readFile filename
  let ast = goParse filename source
  case ast of
    Left x -> (putStrLn $ "ERROR:" ++ filename ++ ":" ++ (show x)) >> exitFailure
    Right x -> check filename x

check :: String -> GoSource -> IO ()
check filename x = do
      let printed = render (pretty x)
          ast2 = goParse filename printed
      case ast2 of
          Left x -> (putStrLn $ "ERROR:" ++ filename ++ ":" ++ (show x)) >> exitFailure
          Right y ->
            if not (x == y) then
                putStrLn ("ERROR:" ++ filename ++ ": pretty printed does not match") >> exitFailure
            else
                return ()
      -- putStr printed

