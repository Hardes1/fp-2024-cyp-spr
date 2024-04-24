module PrinterTest(testPrinter) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Printer (printTerm)
import Term (Term(..))


testVariable :: TestTree
testVariable = testGroup "Print variable" [
    testCase "Variable 'x' -> x" $ printTerm (Variable 'x') @?= "x"
  ]

testLambda :: TestTree
testLambda = testGroup "Print lambda" [
  testCase "Lambda 'x' (Variable 'x') -> \\x.x" $ printTerm (Lambda 'x' (Variable 'x')) @?= "\\x.x",
  testCase "Lambda 'x' (Lambda 'y' (Variable 'x')) -> \\x.\\y.x" $ printTerm (Lambda 'x' (Lambda 'y' (Variable 'x'))) @?= "\\x.\\y.x",
  testCase "Lambda 'x' (Application (Variable 'x') (Variable 'y')) -> \\x.x y" $ printTerm (Lambda 'x' (Application (Variable 'x') (Variable 'y'))) @?= "\\x.x y",
  testCase "Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Variable 'z')) -> \\x.x y z" $ printTerm (Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Variable 'z'))) @?= "\\x.x y z",
  testCase "Lambda 'x' (Application (Variable 'z') (Application (Variable 'x') (Variable 'y')))  -> \\x.z (x y)" $ printTerm (Lambda 'x' (Application (Variable 'z') (Application (Variable 'x') (Variable 'y')))) @?= "\\x.z (x y)",
  testCase "Lambda 'x' (Application (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))) (Variable 'w'))  -> \\x.z (x y) w" $ printTerm (Lambda 'x' (Application (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))) (Variable 'w'))) @?= "\\x.z (x y) w",
  testCase "Lambda 'x' (Application (Variable 'x') (Lambda 'x' (Variable 'x')) -> x (\\x.x))" $ printTerm (Lambda 'x' (Application (Variable 'x') (Lambda 'x' (Variable 'x')))) @?= "\\x.x (\\x.x)",
  testCase "Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Lambda 'y' (Variable 'y'))) -> \\x.(\\x.x) (\\y.y)" $ printTerm (Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Lambda 'y' (Variable 'y')))) @?= "\\x.(\\x.x) (\\y.y)",
  testCase "Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Lambda 'y' (Variable 'y'))) -> \\x.x y (\\y.y)" $ printTerm (Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Lambda 'y' (Variable 'y')))) @?= "\\x.x y (\\y.y)",
  testCase "Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Variable 'x')) -> \\x.(\\x.x) x" $ printTerm (Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Variable 'x'))) @?= "\\x.(\\x.x) x",
  testCase "Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Application (Variable 'x') (Variable 'x'))) -> \\x.(\\x.x) x" $ printTerm (Lambda 'x' (Application (Lambda 'x' (Variable 'x')) (Application (Variable 'x') (Variable 'x')))) @?= "\\x.(\\x.x) x x",
  testCase "Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Application (Variable 'z') (Variable 'w'))) -> \\x.x y (z w)" $ printTerm (Lambda 'x' (Application (Application (Variable 'x') (Variable 'y')) (Application (Variable 'z') (Variable 'w')))) @?= "\\x.x y (z w)"
  ]

testApplication :: TestTree
testApplication = testGroup "Print Application" [
  testCase "Application (Variable 'x') (Variable 'y') -> x y" $ printTerm (Application (Variable 'x') (Variable 'y')) @?= "x y",
  testCase "Application (Application (Variable 'x') (Variable 'y')) (Variable 'z') -> x y z" $ printTerm (Application (Application (Variable 'x') (Variable 'y')) (Variable 'z')) @?= "x y z",
  testCase "Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))  -> z (x y)" $ printTerm (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))) @?= "z (x y)",
  testCase "Application (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))) (Variable 'w')  -> z (x y) w" $ printTerm (Application (Application (Variable 'z') (Application (Variable 'x') (Variable 'y'))) (Variable 'w')) @?= "z (x y) w",
  testCase "Application (Variable 'x') (Lambda 'x' (Variable 'x')) -> x (\\x.x)" $ printTerm (Application (Variable 'x') (Lambda 'x' (Variable 'x'))) @?= "x (\\x.x)",
  testCase "Application (Lambda 'x' (Variable 'x')) (Lambda 'y' (Variable 'y')) -> (\\x.x) (\\y.y)" $ printTerm (Application (Lambda 'x' (Variable 'x')) (Lambda 'y' (Variable 'y'))) @?= "(\\x.x) (\\y.y)",
  testCase "Application (Application (Variable 'x') (Variable 'y')) (Lambda 'y' (Variable 'y')) -> x y (\\y.y)" $ printTerm (Application (Application (Variable 'x') (Variable 'y')) (Lambda 'y' (Variable 'y'))) @?= "x y (\\y.y)",
  testCase "Application (Lambda 'x' (Variable 'x')) (Variable 'x') -> (\\x.x) x" $ printTerm (Application (Lambda 'x' (Variable 'x')) (Variable 'x')) @?= "(\\x.x) x",
  testCase "Application (Lambda 'x' (Variable 'x')) (Application (Variable 'x') (Variable 'x')) -> (\\x.x) x" $ printTerm (Application (Lambda 'x' (Variable 'x')) (Application (Variable 'x') (Variable 'x'))) @?= "(\\x.x) x x",
  testCase "Application (Application (Variable 'x') (Variable 'y')) (Application (Variable 'z') (Variable 'w')) -> x y (z w)" $ printTerm (Application (Application (Variable 'x') (Variable 'y')) (Application (Variable 'z') (Variable 'w'))) @?= "x y (z w)"
 ]

testPrinter :: TestTree
testPrinter = testGroup "Printer tests" [
    testVariable,
    testLambda,
    testApplication
  ]