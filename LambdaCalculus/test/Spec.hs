import Test.Tasty (TestTree, testGroup, defaultMain)
import ParserTest ( testParser )

tests :: TestTree
tests = testGroup "Lambda calculus utils tests" [testParser]

main :: IO ()
main = defaultMain tests