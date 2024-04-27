module Test.Eval(props) where

import Hedgehog
import Test.Tasty
import Expr(Error(..), Expr (BinOp, Const), BinaryOperator (Multiply, Plus, Minus, Divide, Power))
import Util.Evaluator(evalExprNoArgs, evalExpr)
import Util.Generator(genExprOnlyArgs, genExprOnlyNumbers, genExprMixed, genBindnigsToVariables, genExprMixedOnlyPositive)
import Test.Tasty.Hedgehog
import Data.Either (isLeft)

evalOnlyVarsWithNoBindings :: Property
evalOnlyVarsWithNoBindings = property $ do
  expr <- forAll $ genExprOnlyArgs 100
  assert (evalExprNoArgs expr == Left VariableIsUndefined)

evalOnlyNumbers :: Property 
evalOnlyNumbers = property $ do
    expr <- forAll $ genExprOnlyNumbers 100
    assert (evalExprNoArgs expr /= Left VariableIsUndefined)

evalMultiplicationByOnePreservesResult :: Property
evalMultiplicationByOnePreservesResult = property $ do
  expr <- forAll $ genExprMixed 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExprLHS = BinOp Multiply (Const 1) expr
  let newExprRHS = BinOp Multiply expr (Const 1)
  assert (evalExpr newExprLHS bindings == evalExpr expr bindings)
  assert (evalExpr newExprRHS bindings == evalExpr expr bindings)

evalAdditionWithZeroPreservesResult :: Property
evalAdditionWithZeroPreservesResult = property $ do
  expr <- forAll $ genExprMixed 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExprLHS = BinOp Plus (Const 0) expr
  let newExprRHS = BinOp Plus expr (Const 0)
  assert (evalExpr newExprLHS bindings == evalExpr expr bindings)
  assert (evalExpr newExprRHS bindings == evalExpr expr bindings)

evalSubtractionWithZeroPreservesResult :: Property
evalSubtractionWithZeroPreservesResult = property $ do
  expr <- forAll $ genExprMixed 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = BinOp Minus expr (Const 0)
  assert (evalExpr newExpr bindings == evalExpr expr bindings)

evalDivisionByOnePresevesResult :: Property
evalDivisionByOnePresevesResult = property $ do
  expr <- forAll $ genExprMixed 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = BinOp Divide expr (Const 1)
  assert (evalExpr newExpr bindings == evalExpr expr bindings)

evalDivisionByZeroIsZeroOrError :: Property
evalDivisionByZeroIsZeroOrError = property $ do
  expr <- forAll $ genExprMixed 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = BinOp Divide (Const 0.0) expr
  let result = evalExpr newExpr bindings
  assert (result == Right 0.0 || isLeft result)

evalPowerInOnePreservesResult :: Property
evalPowerInOnePreservesResult = property $ do
  expr <- forAll $ genExprMixedOnlyPositive 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = BinOp Power expr (Const 1.0)
  assert (evalExpr expr bindings == evalExpr newExpr bindings)

evalPowerInZeroPreservesResult :: Property
evalPowerInZeroPreservesResult = property $ do
  expr <- forAll $ genExprMixedOnlyPositive 100
  bindings <- forAll $ genBindnigsToVariables 100
  let newExpr = BinOp Power expr (Const 0.0)
  let result = evalExpr newExpr bindings
  assert (result == Right 1.0 || isLeft result)

props :: [TestTree]
props = [
    testProperty "Expressions with vars with no bindings are not calculated" evalOnlyVarsWithNoBindings,
    testProperty "Expression without vars can't get error VariableIsUndefined" evalOnlyNumbers,
    testProperty "1 * Expression == Expression * 1 == Expression" evalMultiplicationByOnePreservesResult,
    testProperty "0 + Expression == Expression + 0 == Expression" evalAdditionWithZeroPreservesResult,
    testProperty "Expression - 0 == Expression" evalSubtractionWithZeroPreservesResult,
    testProperty "Expression / 1 == Expression" evalDivisionByOnePresevesResult,
    testProperty "0 / Expression == 0 or Error" evalDivisionByZeroIsZeroOrError,
    testProperty "Positive Expression ^ 1 == Positive Expression" evalPowerInOnePreservesResult,
    testProperty "Positive Expression ^ 0 == 1" evalPowerInZeroPreservesResult
    ]