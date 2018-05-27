import Test.Tasty
import Tests.Lexer
import Tests.Parser
import Tests.ParseStatements
import Tests.Pretty
import Tests.Types

main :: IO ()
main = defaultMain $ testGroup "all"
  [ testsLexer
  , testsParser
  , testsParseStmts
  , testsPretty
  , testsTypes
  ]
