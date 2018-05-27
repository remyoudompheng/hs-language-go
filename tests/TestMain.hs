import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>))

import Tests.Lexer
import Tests.Parser
import Tests.ParseStatements
import Tests.Pretty
import Tests.Types

import Language.Go.Parser.Parser (goTokenize, goParse)

main :: IO ()
main = defaultMain $ testGroup "all"
  [ testsLexer
  , testsParser
  , testsParseStmts
  , testsPretty
  , testsTypes
  , testsLexSamples
  , testsParseSamples
  ]

testsLexSamples :: TestTree
testsLexSamples = testGroup "lex samples"
                $ map testLexFile sampleFiles

testsParseSamples :: TestTree
testsParseSamples = testGroup "parse samples"
                $ map testParseFile sampleFiles

sampleFiles :: [FilePath]
sampleFiles = map ("tests" </>)
  [ "test01.go"
  , "test02.go"
  , "test02.go"
  , "test04.go"
  , "test05.go"
  , "test06.go"
  , "test07.go"
  , "test08.go"
  , "test09.go"
  , "test10.go"
  , "test11.go"
  , "test12.go"
  , "test13.go"
  , "test14.go"
  , "test15.go"
  , "test16.go"
  , "test17.go"
  , "test18.go"
  , "test19.go"
  , "test20.go"
  , "test-Statement-01.go"
  , "test-Statement-02.go"
  , "test-Statement-03.go"
  , "test-Statement-04.go"
  ]

testLexFile :: String -> TestTree
testLexFile fname = testCase ("lex "++fname) a
  where a = do
          source <- readFile fname
          let tokens = goTokenize source
          mapM_ return tokens

testParseFile :: String -> TestTree
testParseFile fname = testCase ("parse "++fname) a
  where a = do
          source <- readFile fname
          case goParse fname source of
            Left err -> assertFailure ("cannot parse: " ++ show err)
            Right _ -> return ()
