module Util.Generator where

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

genExpr :: Int -> Gen (Expr Double)
genExpr n =
  Gen.recursive
    Gen.choice
    [ 
      numGen,
      varGen
    ]
    [ 
     binOpGen,
     unOpGen 
    ]
  where
    varGen = Expr.Var <$> genVariableName
    numGen = Const <$> genDouble n
    binOpGen = do
      op <- genBinaryOperator
      Gen.subterm2 (genExpr n) (genExpr n) (BinOp op)

    unOpGen = do
      op <- genUnaryOperator
      Gen.subterm (genExpr n) (UnOp op) 