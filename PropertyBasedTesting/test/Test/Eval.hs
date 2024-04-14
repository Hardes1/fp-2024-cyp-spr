module Test.Eval(props) where

import Hedgehog
import Test.Tasty
import Expr(Error(..))
import Util.Evaluator(evalExprNoArgs)
import Util.Generator(genExprOnlyArgs, genExprOnlyNumbers)
import Test.Tasty.Hedgehog

evalOnlyVarsWithNoBindings :: Property
evalOnlyVarsWithNoBindings = property $ do
  expr <- forAll $ genExprOnlyArgs 100
  assert (evalExprNoArgs expr == Left VariableIsUndefined)

evalOnlyNumbers :: Property 
evalOnlyNumbers = property $ do
    expr <- forAll $ genExprOnlyNumbers 100
    assert (evalExprNoArgs expr /= Left VariableIsUndefined)

props :: [TestTree]
props = [
    testProperty "Expressions with vars with no bindings are not calculated" evalOnlyVarsWithNoBindings
    , testProperty "Expression without vars can't get error VariableIsUndefined" evalOnlyNumbers
    ]