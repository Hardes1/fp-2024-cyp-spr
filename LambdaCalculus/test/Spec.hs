import Test.Tasty (TestTree, testGroup, defaultMain)
import ParserTest ( testParser )
import PrinterTest (testPrinter)

tests :: TestTree
tests = testGroup "Lambda calculus utils tests" [testParser, testPrinter]

main :: IO ()
main = defaultMain tests