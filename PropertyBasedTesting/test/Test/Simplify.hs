module Test.Simplify(props) where

import Hedgehog
import Test.Tasty
import Expr(simplify)
import Util.Generator(genExprMixed, genBindnigsToVariables)
import Util.Evaluator(evalExpr)
import Test.Tasty.Hedgehog

simplifyCompositionEval :: Property
simplifyCompositionEval = property $ do
  expr <- forAll $ genExprMixed 2
  bindings <- forAll $ genBindnigsToVariables 100
  let simplified = simplify expr
  assert (evalExpr expr bindings == evalExpr simplified bindings)

props :: [TestTree]
props =
  [ testProperty "Simplify doesn't modify the result of evaluation" simplifyCompositionEval
  ]