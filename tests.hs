import Test.HUnit
import Language.Go.Tests.Lexer
import Language.Go.Tests.Parser
import Language.Go.Tests.ParseStatements
import Language.Go.Tests.Pretty
import Language.Go.Tests.Types

main :: IO Counts
main = runTestTT $ TestList (testsLexer
                          ++ testsParser
                          ++ testsParseStmts
                          ++ testsPretty
                          ++ testsTypes )
