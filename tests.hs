import Test.HUnit
import Language.Go.Tests.Parser
import Language.Go.Tests.ParseStatements

main :: IO Counts
main = runTestTT $ TestList (testsParser ++ testsParseStmts)
