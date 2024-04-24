module ParserTest(testParser) where
import Test.Tasty (TestTree, testGroup)
import Utils(parseInput, constructErrorMessage)
import Test.Tasty.HUnit (testCase, (@?=))
import Term (Term(..))

testVariables :: TestTree
testVariables = testGroup "parse variables" [
    testCase "x -> Variable x" $ parseInput "x" @?= Right (Variable 'x'),
    testCase "X -> Variable x" $ parseInput "X" @?= Right (Variable 'X'),
    testCase "(x) -> Variable x" $ parseInput "(x)" @?= Right (Variable 'x'),
    testCase "number is not a variable" $ parseInput "1" @?= Left (constructErrorMessage 1 "\"1\"" "\"\\\\\", letter or \"(\""),
    testCase "Spaces before is error" $ parseInput "  x" @?= Left (constructErrorMessage 1 "\" \"" "\"\\\\\", letter or \"(\""),
    testCase "Spaces after is error" $ parseInput "x " @?= Left (constructErrorMessage 2 "\' \'" "end of input")
    ]

testApplications :: TestTree
testApplications = testGroup "parse applications" [
    testCase "x y -> Application (Variable x) (Variable y)" $ parseInput "x y" @?= Right (Application (Variable 'x') (Variable 'y')),
    testCase "x x -> Application (Variable x) (Variable x)" $ parseInput "x x" @?= Right (Application (Variable 'x') (Variable 'x')),
    testCase "(x x) -> Application (Variable x) (Variable x)" $ parseInput "(x x)" @?= Right (Application (Variable 'x') (Variable 'x')),
    testCase "(x) (x) -> Application (Variable x) (Variable x)" $ parseInput "(x) (x)" @?= Right (Application (Variable 'x') (Variable 'x')),
    testCase "Respects left associativity: x y z -> Right (Application (Application (Variable 'x') (Variable 'y')) (Variable 'z'))" $ parseInput "x y z" @?= Right (Application (Application (Variable 'x') (Variable 'y')) (Variable 'z')),
    testCase "Respects right associativity with braces associativity: x (y z) -> Right (Application (Variable 'x') (Application (Variable 'y') (Variable 'z'))" $ parseInput "x (y z)" @?= Right (Application (Variable 'x') (Application (Variable 'y') (Variable 'z'))),
    testCase "No spaces between is error" $ parseInput "xy" @?= Left (constructErrorMessage 2 "\'y\'" "end of input"),
    testCase "Spaces before is error" $ parseInput " x y" @?= Left (constructErrorMessage 1 "\" \"" "\"\\\\\", letter or \"(\""),
    testCase "Spaces after is error" $ parseInput "x y " @?= Left (constructErrorMessage 2 "\' \'" "end of input")
   ]

testLambda :: TestTree
testLambda = testGroup "parse lambda" [
    testCase "\\x.x -> Lambda 'x' (Variable 'x')" $ parseInput "\\x.x" @?= Right (Lambda 'x' (Variable 'x')),
    testCase "(\\x.x) -> Lambda 'x' (Variable 'x')" $ parseInput "(\\x.x)" @?= Right (Lambda 'x' (Variable 'x')),
    testCase "\\x.\\y.x -> Lambda 'x' (Lambda 'y' (Variable 'x'))" $ parseInput "\\x.\\y.x" @?= Right ( Lambda 'x' (Lambda 'y' (Variable 'x'))),
    testCase "Body in braces: \\x.(\\y.x) -> Lambda 'x' (Lambda 'y' (Variable 'x'))" $ parseInput "\\x.\\y.x" @?= Right ( Lambda 'x' (Lambda 'y' (Variable 'x'))),
    testCase "Braces everywhere: (\\x.(\\y.(x)) -> Lambda 'x' (Lambda 'y' (Variable 'x'))" $ parseInput "\\x.\\y.x" @?= Right ( Lambda 'x' (Lambda 'y' (Variable 'x'))),
    testCase "Space before dot is error" $ parseInput "\\x .x" @?= Left (constructErrorMessage 3 "\" \"" "\".\""),
    testCase "Space after dot is error" $ parseInput "\\x. x" @?= Left (constructErrorMessage 4 "\" \"" "\"\\\\\", letter or \"(\""),
    testCase "Braces for bound variable is error" $ parseInput "\\(x).x" @?= Left (constructErrorMessage 2 "\"(\"" "letter"),
    testCase "Spaces before is error" $ parseInput " \\x.x" @?= Left (constructErrorMessage 1 "\" \"" "\"\\\\\", letter or \"(\""),
    testCase "Spaces after is error" $ parseInput "\\x.x " @?= Left (constructErrorMessage 5 "\' \'" "end of input")
    ]

testMixed :: TestTree
testMixed = testGroup "Parse mixed" [
    testCase "\\x.x \\y.y -> Lambda 'x' (Application (Lambda 'y' (Variable 'y')))" $ parseInput "\\x.x \\y.y" @?= Right (Lambda 'x' (Application (Variable 'x') (Lambda 'y' (Variable 'y')))),
    testCase "(\\x.x) \\x.x -> Application (Lambda 'x' (Variable 'x')) (Lambda 'x' (Variable 'x'))" $ parseInput "(\\x.x) \\x.x" @?= Right (Application (Lambda 'x' (Variable 'x')) (Lambda 'x' (Variable 'x'))),
    testCase "x \\x.x -> Application (Variable 'x') (Lambda 'x' (Variable 'x'))" $ parseInput "x \\x.x" @?= Right (Application (Variable 'x') (Lambda 'x' (Variable 'x'))),
    testCase "(\\x.x) x -> Application (Lambda 'x' (Variable 'x')) (Variable 'x')" $ parseInput "(\\x.x) x" @?= Right (Application (Lambda 'x' (Variable 'x')) (Variable 'x')),
    testCase "x (\\x.x) x -> Application (Application (Variable 'x') (Lambda 'x' (Variable 'x'))) (Variable 'x')" $ parseInput "x (\\x.x) x" @?= Right (Application (Application (Variable 'x') (Lambda 'x' (Variable 'x'))) (Variable 'x'))
    ]

testParser :: TestTree
testParser =
    testGroup "Lambda term parser test" [
        testVariables,
        testApplications,
        testLambda,
        testMixed
    ]