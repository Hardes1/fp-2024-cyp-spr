module Test.Utils where

import Hedgehog
import Expr(Expr(..), BinaryOperator(..), UnaryOperator(..), Error, eval)
import StateDemo(execState)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Map.Strict as Map(empty, Map, fromList)
import Parser(runParser, parseExpression)

-- For the simplest types, the generator just picks a random value from the list. 
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

-- To generate a recursive algebraic data type, use Gen.recursive and Gen.subterm
genExpr :: Int -> Gen (Expr Double)
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen,
      varGen
    ]
    [ -- recursive generators
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

evalExpr :: (Show a, Ord a, Floating a) => Expr a -> Map String a -> Either Error a
evalExpr expr dict = execState (Expr.eval expr) dict


evalExprNoArgs :: (Show a, Ord a, Floating a) => Expr a -> Either Error a
evalExprNoArgs expr = evalExpr expr Map.empty

parseExpr :: String -> Either String (String, (Expr Double))
parseExpr s = runParser parseExpression s