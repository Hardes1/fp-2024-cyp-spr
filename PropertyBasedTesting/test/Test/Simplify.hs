module Test.Simplify(props) where

import Hedgehog
import Test.Tasty
import Expr(simplify, Expr (..), BinaryOperator (Plus, Multiply, Minus, Divide, Power))
import Util.Generator(genExprMixed, genBindnigsToVariables, genExprOnlyNumbers, numGen)
import Util.Evaluator(evalExpr)
import Test.Tasty.Hedgehog
import Data.Either (isLeft)

simplifyCompositionEval :: Property
simplifyCompositionEval = property $ do
  expr <- forAll $ genExprMixed 1
  bindings <- forAll $ genBindnigsToVariables 100
  let simplified = simplify expr
  assert (evalExpr expr bindings == evalExpr simplified bindings)

simplifyWithOneAdditionalOp :: (Expr Double -> Expr Double) -> Property
simplifyWithOneAdditionalOp f = property $ do
  expr <- forAll $ genExprMixed 2
  bindings <- forAll $ genBindnigsToVariables 100
  let transformedExpr = f expr
  let simplifiedTransformedExpr = simplify transformedExpr
  let simplifiedInitialExpr = simplify expr
  assert (evalExpr expr bindings == evalExpr simplifiedTransformedExpr bindings)
  assert (simplifiedTransformedExpr == simplifiedInitialExpr)

simplifyWithZeroResult :: (Expr Double -> Expr Double) -> Property
simplifyWithZeroResult f = property $ do
  expr <- forAll $ genExprOnlyNumbers 2
  bindings <- forAll $ genBindnigsToVariables 100
  let transformedSimplifiedExpr = simplify $ f expr
  let result = evalExpr transformedSimplifiedExpr bindings
  assert (result == Right 0.0 || isLeft result)

simplifyPower :: Expr Double -> Property
simplifyPower e = property $ do
  expr <- forAll $ numGen 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = simplify $ BinOp Power expr e
  let result = evalExpr newExpr bindings
  case e of 
    Const 0.0 -> assert(result == Right 1.0)
    Const 1.0 -> assert(result == evalExpr expr bindings) 
    _ -> error ("Illegal input expression was passed" ++ show e)

props :: [TestTree]
props =
  [ testProperty "Simplify doesn't modify the result of evaluation" simplifyCompositionEval,
    testProperty "Should simplify addition with zero left" (simplifyWithOneAdditionalOp (BinOp Plus (Const 0.0))),
    testProperty "Should simplify addition with zero right" (simplifyWithOneAdditionalOp (\x -> BinOp Plus x (Const 0.0))),
    testProperty "Should simplify subtraction by zero" (simplifyWithOneAdditionalOp (\x -> BinOp Minus x (Const 0.0))),
    testProperty "Should simplify multiplication by one left" (simplifyWithOneAdditionalOp (BinOp Multiply (Const 1.0))),
    testProperty "Should simplify multiplication by one right" (simplifyWithOneAdditionalOp (\x -> BinOp Multiply x (Const 1.0))),
    testProperty "Should simplify multiplication by zero left" (simplifyWithZeroResult (BinOp Multiply (Const 0.0))),
    testProperty "Should simplify multiplication by zero right" (simplifyWithZeroResult (\x -> BinOp Multiply x (Const 0.0))),
    testProperty "Should simplify division by one" (simplifyWithOneAdditionalOp (\x -> BinOp Divide x (Const 1.0))),
    testProperty "Should simplify division with zero" (simplifyWithZeroResult (BinOp Divide (Const 0.0))),
    testProperty "Should simplify power with one" (simplifyPower (Const 1.0)),
    testProperty "Should simplify power with zero" (simplifyPower (Const 0.0))
  ]