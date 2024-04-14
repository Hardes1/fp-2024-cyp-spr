module Util.Evaluator(evalExpr, evalExprNoArgs) where

import Expr(Expr(..), Error, eval)
import StateDemo(execState)
import Data.Map.Strict as Map(Map, empty)

evalExpr :: (Ord a, Floating a) =>Expr a -> Map String a -> Either Error a
evalExpr expr = execState (Expr.eval expr)

evalExprNoArgs :: (Ord a, Floating a) => Expr a -> Either Error a
evalExprNoArgs expr = evalExpr expr Map.empty