module Util.Evaluator where

import Expr(Expr(..), BinaryOperator(..), UnaryOperator(..), Error, eval)
import StateDemo(execState)
import Data.Map.Strict as Map(Map, empty)

evalExpr :: (Show a, Ord a, Floating a) => Expr a -> Map String a -> Either Error a
evalExpr expr dict = execState (Expr.eval expr) dict

evalExprNoArgs :: (Show a, Ord a, Floating a) => Expr a -> Either Error a
evalExprNoArgs expr = evalExpr expr Map.empty