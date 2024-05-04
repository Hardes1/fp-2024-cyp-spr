{-# LANGUAGE InstanceSigs #-}
module Expr (BinaryOperator(..), UnaryOperator(..), Expr(..), Error(..), eval, simplify, getPrefixNotation) where
import Data.Either (fromLeft)
import Data.Map.Strict as M ( lookup, Map)
import StateDemo (State, get)
import Text.Printf (printf)

data BinaryOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)

data UnaryOperator = Square deriving Eq

data Expr a = Const a | Var String | BinOp BinaryOperator (Expr a) (Expr a) | UnOp UnaryOperator (Expr a) deriving (Eq)

instance (Show a) => Show (Expr a) where
  show :: Expr a -> String
  show (Const x) = show x
  show (BinOp Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (BinOp Minus x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (BinOp Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (BinOp Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (BinOp Power x y) = show x ++ " ^ " ++ show y
  show (UnOp Square x) = "sqrt(" ++ show x ++ ")"
  show (Var name) = name


getPrefixNotation :: (Show a, RealFrac a) => Expr a -> String
getPrefixNotation (Const x) = show (round x :: Int)
getPrefixNotation (BinOp Plus x y) = "+ " ++ getPrefixNotation x ++ " " ++ getPrefixNotation y
getPrefixNotation (BinOp Minus x y) = "- " ++ getPrefixNotation x ++ " " ++ getPrefixNotation y
getPrefixNotation (BinOp Multiply x y) = "* " ++ getPrefixNotation x ++ " " ++ getPrefixNotation y
getPrefixNotation (BinOp Divide x y) = "/ " ++ getPrefixNotation x ++ " " ++ getPrefixNotation y
getPrefixNotation (BinOp Power x y) = "^ " ++ getPrefixNotation x ++ " " ++ getPrefixNotation y
getPrefixNotation (UnOp Square x) = "sqrt " ++ getPrefixNotation x
getPrefixNotation (Var name) = name

instance (Num a) => Num (Expr a) where
  (+) :: Expr a -> Expr a -> Expr a
  (+) = BinOp Plus
  (*) :: Expr a -> Expr a -> Expr a
  (*) = BinOp Multiply
  abs :: Expr a -> Expr a
  abs = undefined
  signum :: Expr a -> Expr a
  signum = undefined
  fromInteger :: Integer -> Expr a
  fromInteger = Const . fromInteger
  negate :: Expr a -> Expr a
  negate = BinOp Multiply (Const (-1))


data Error = DivisorIsZero | SquareRootIsNegative | PowerOpException String | VariableIsUndefined String | IllegalState deriving Eq
instance Show Error where
  show :: Error -> String
  show DivisorIsZero = "Divisor is equal zero"
  show SquareRootIsNegative  = "The root can't be negative"
  show (PowerOpException s) = "Power operation exception: " ++ s
  show (VariableIsUndefined s) = printf "Variable %s is undefined" s
  show IllegalState = "Illegal state error, should not be thrown"


performOp :: (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performOp f a b = case a of
    Right first -> case b of
      Right second -> Right (f first second)
      err -> err
    err -> err


performOpWithCheck :: (Either Error a -> Either Error a -> Maybe Error) -> (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performOpWithCheck pr f a b = case pr a b of
  Nothing -> performOp f a b
  Just err -> Left err

checkDivisorIsZero :: (Eq a, Fractional a) => Either Error a -> Either Error a -> Maybe Error
checkDivisorIsZero _ b = case b of
  Right 0.0 -> Just DivisorIsZero
  Right _ -> Nothing
  Left err -> Just err

checkSquareRoot ::  (Ord a, Num a) => Error -> Either Error a -> Either Error a -> Maybe Error
checkSquareRoot err (Right x) (Right _)
  | x < 0 = Just err
  | otherwise = Nothing

checkSquareRoot _ a b = Just ( case a of
  Left err -> err
  _ -> fromLeft IllegalState b
  )



checkPowerOp :: (Ord a, Num a) => (String -> Error) -> Either Error a -> Either Error a -> Maybe Error
checkPowerOp err (Right x) (Right y)
  | x == 0 && y < 0 = Just (err "Can't power zero in negative degree")
  | x < 0 = Just (err "Power base is negative")
  | otherwise = Nothing

checkPowerOp _ a b = Just ( case a of
  Left err -> err
  _ -> fromLeft IllegalState b
  )

getVar :: String -> Maybe b -> Either Error b
getVar key value = case value of
  Just x -> Right x
  _ -> Left (VariableIsUndefined key)


eval :: (Ord b, Floating b) => Expr b -> State (Map String b) (Either Error b)
eval (Const x) = do
  return (Right x)
eval (Var v) = do
  getVar v . M.lookup v <$> get
eval (BinOp op x y) = do
  f <- eval x
  s <- eval y
  return (calcBinOp op f s)
  where
  calcBinOp op = case op of
    Plus -> performOp (+)
    Minus -> performOp (-)
    Multiply ->  performOp (*)
    Divide -> performOpWithCheck checkDivisorIsZero (/)
    Power -> performOpWithCheck (checkPowerOp PowerOpException) (**)
eval (UnOp op x) = do
  f <- eval x
  return (case op of
    Square -> performOpWithCheck (checkSquareRoot SquareRootIsNegative) (**) f (Right 0.5)
    )


simplifyAdd :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyAdd (Const 0.0) x = x
simplifyAdd x (Const 0.0) = x
simplifyAdd x y = BinOp Plus x y

simplifySubtract :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifySubtract (Const x) (Const y)
    | x == y = Const 0.0
    | y == 0.0 = Const x
    | otherwise = BinOp Minus (Const x) (Const y)
simplifySubtract x (Const 0.0) = x
simplifySubtract x y = BinOp Minus x y


chooseMultiplyByZeroSimplification :: Fractional a => Expr a -> Expr a -> Expr a
chooseMultiplyByZeroSimplification initExpr expr = case expr of
  Const _ -> Const 0.0
  _ -> initExpr

simplifyMultiply :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyMultiply (Const 0.0) x = chooseMultiplyByZeroSimplification (BinOp Multiply (Const 0.0) x) x
simplifyMultiply x (Const 0.0) = chooseMultiplyByZeroSimplification (BinOp Multiply x (Const 0.0)) x
simplifyMultiply (Const 1.0) x = x
simplifyMultiply x (Const 1.0) = x
simplifyMultiply x y = BinOp Multiply x y

simplifyDivide :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyDivide (Const 0.0) (Const x)
    | x /= 0 = Const 0.0
    | otherwise = BinOp Divide (Const 0.0) (Const x)
simplifyDivide x (Const 1.0) = x
simplifyDivide (Const x) (Const y)
    | x == y && x /= 0.0 = Const 1.0
    | otherwise = BinOp Divide (Const x) (Const y)
simplifyDivide x y = BinOp Divide x y


simplifyPower :: (Fractional a, Ord a) => Expr a -> Expr a -> Expr a
simplifyPower (Const x) (Const 1.0)
    | x >= 0.0 = Const x
    | otherwise = BinOp Power (Const x) (Const 1.0)
simplifyPower (Const x) (Const 0.0)
     | x > 0.0 = Const 1.0
     | otherwise = BinOp Power (Const x) (Const 0.0)
simplifyPower x y = BinOp Power x y

simplify :: (Ord a, Fractional a) =>  Expr a -> Expr a
simplify expr = case expr of
  BinOp Plus l r -> simplifyAdd (simplify l) (simplify r)
  BinOp Minus l r -> simplifySubtract (simplify l) (simplify r)
  BinOp Multiply l r -> simplifyMultiply (simplify l) (simplify r)
  BinOp Divide l r -> simplifyDivide (simplify l) (simplify r)
  BinOp Power l r -> simplifyPower (simplify l) (simplify r)
  UnOp Square l -> UnOp Square (simplify l)
  _ -> expr