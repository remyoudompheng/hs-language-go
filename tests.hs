import Test.HUnit
import Language.Go.Tests.Parser

main :: IO Counts
main = runTestTT $ TestList $ testsParser
