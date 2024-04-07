module Test.Simplify where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Expr(Expr(..), simplify)
import Util.Generator(genExpr, genBindnigsToVariables)
import Util.Evaluator(evalExpr)
import Test.Tasty.Hedgehog

simplifyCompositionEval :: Property
simplifyCompositionEval = property $ do
  expr <- forAll $ (genExpr 100)
  bindings <- forAll $ (genBindnigsToVariables 100)
  let simplified = simplify expr
  assert ((evalExpr expr bindings) == (evalExpr simplified bindings))

props :: [TestTree]
props =
  [ testProperty "Simplify doesn't modify the result of evaluation" simplifyCompositionEval
  ]