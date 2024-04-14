module Test.Simplify(props) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Expr(Expr(..), simplify)
import Util.Generator(genExprMixed, genBindnigsToVariables)
import Util.Evaluator(evalExpr)
import Test.Tasty.Hedgehog

simplifyCompositionEval :: Property
simplifyCompositionEval = property $ do
  expr <- forAll $ (genExprMixed 100)
  bindings <- forAll $ (genBindnigsToVariables 100)
  let simplified = simplify expr
  assert ((evalExpr expr bindings) == (evalExpr simplified bindings))

props :: [TestTree]
props =
  [ testProperty "Simplify doesn't modify the result of evaluation" simplifyCompositionEval
  ]