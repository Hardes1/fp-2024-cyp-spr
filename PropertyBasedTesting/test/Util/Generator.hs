module Util.Generator(genBindnigsToVariables, genExprMixed, genExprOnlyArgs, genExprOnlyNumbers) where

import Hedgehog
import Expr(Expr(..), BinaryOperator(..), UnaryOperator(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Map.Strict as Map(Map, fromList)

genBinaryOperator :: Gen BinaryOperator
genBinaryOperator = Gen.element [Plus, Minus, Multiply, Divide, Power]

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
    return $ Map.fromList $ (zip vars values)


genExprMixed :: Int -> Gen (Expr Double)
genExprMixed n = genExpr [varGen, numGen n] (opsGen (genExprMixed n))

genExprOnlyNumbers :: Int -> Gen (Expr Double)
genExprOnlyNumbers n = genExpr [numGen n] (opsGen (genExprOnlyNumbers n))

genExprOnlyArgs :: Int -> Gen (Expr Double)
genExprOnlyArgs n = genExpr [varGen] (opsGen (genExprOnlyArgs n))


genExpr :: [Gen (Expr Double)] -> [Gen (Expr Double)] -> Gen (Expr Double)
genExpr term next =
  Gen.recursive Gen.choice term next


varGen :: Gen (Expr Double)
varGen = Expr.Var <$> genVariableName


numGen :: Int -> Gen (Expr Double)
numGen n = Const <$> genDouble n


opsGen :: Gen (Expr Double) -> [Gen (Expr Double)]
opsGen gen = [binOpGen gen, unOpGen gen]

binOpGen :: Gen (Expr Double) -> Gen (Expr Double)
binOpGen genEx = do
      op <- genBinaryOperator
      Gen.subterm2 genEx genEx (BinOp op)
  
unOpGen :: Gen (Expr Double) -> Gen (Expr Double)
unOpGen genEx = do
    op <- genUnaryOperator
    Gen.subterm genEx (UnOp op) 