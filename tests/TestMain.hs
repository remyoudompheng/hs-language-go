import Test.HUnit
import Tests.Lexer
import Tests.Parser
import Tests.ParseStatements
import Tests.Pretty
import Tests.Types

main :: IO Counts
main = runTestTT $ TestList (testsLexer
                          ++ testsParser
                          ++ testsParseStmts
                          ++ testsPretty
                          ++ testsTypes )
