module Util.Generator(genBindnigsToVariables, genExprMixed, genExprOnlyArgs, genExprOnlyNumbers, numGen, genExprMixedOnlyPositive) where

import Hedgehog
import Expr(Expr(..), BinaryOperator(..), UnaryOperator(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Map.Strict as Map(Map, fromList)

genBinaryOperator :: Gen BinaryOperator
genBinaryOperator = Gen.element [Plus, Minus, Multiply, Divide, Power]

genPositiveBinaryOperator :: Gen BinaryOperator
genPositiveBinaryOperator = Gen.element [Plus, Multiply, Divide, Power]

genUnaryOperator :: Gen UnaryOperator
genUnaryOperator = Gen.element [Square]

varNames :: [String]
varNames = ["x", "y", "z"]

genVariableName :: Gen String
genVariableName = Gen.element varNames


genDouble :: Int -> Gen Double
genDouble n = fromIntegral <$> Gen.int (Range.constant 0 n)

genBindnigsToVariables :: Int -> Gen (Map String Double)
genBindnigsToVariables n = do
    vars <- Gen.subsequence varNames
    values <- Gen.list (Range.singleton $ length vars) (genDouble n)
    return $ Map.fromList $ zip vars values


genExprMixedOnlyPositive :: Int -> Gen (Expr Double)
genExprMixedOnlyPositive n = genExpr [varGen, numGen n ] (positiveOpsGen (genExprMixedOnlyPositive n))

genExprMixed :: Int -> Gen (Expr Double)
genExprMixed n = genExpr [varGen, numGen n] (opsGen (genExprMixed n))

genExprOnlyNumbers :: Int -> Gen (Expr Double)
genExprOnlyNumbers n = genExpr [numGen n] (opsGen (genExprOnlyNumbers n))

genExprOnlyArgs :: Int -> Gen (Expr Double)
genExprOnlyArgs n = genExpr [varGen] (opsGen (genExprOnlyArgs n))


genExpr :: [Gen (Expr Double)] -> [Gen (Expr Double)] -> Gen (Expr Double)
genExpr = Gen.recursive Gen.choice


varGen :: Gen (Expr Double)
varGen = Expr.Var <$> genVariableName


numGen :: Int -> Gen (Expr Double)
numGen n = Const <$> genDouble n

positiveOpsGen :: Gen (Expr Double) -> [Gen (Expr Double)]
positiveOpsGen gen = [binOpGen gen genPositiveBinaryOperator, unOpGen gen genUnaryOperator]


opsGen :: Gen (Expr Double) -> [Gen (Expr Double)]
opsGen gen = [binOpGen gen genBinaryOperator, unOpGen gen genUnaryOperator]



binOpGen :: Gen (Expr Double) -> Gen BinaryOperator -> Gen (Expr Double)
binOpGen genEx genOp = do
      op <- genOp
      Gen.subterm2 genEx genEx (BinOp op)
  
unOpGen :: Gen (Expr Double) -> Gen UnaryOperator -> Gen (Expr Double)
unOpGen genEx genOp = do
    op <- genOp
    Gen.subterm genEx (UnOp op) 