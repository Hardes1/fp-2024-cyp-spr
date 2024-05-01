{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Evaluate" #-}
module ExprParser(parseExpression, parseIdent) where

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Data.Map as M (fromList, member, Map, lookup)
import Expr (BinaryOperator(..), UnaryOperator(..), Expr(..))
import Control.Applicative ( Alternative((<|>), empty) )
import Parser (Parser(..), satisfy, getWord)
import GHC.Unicode (isSpace)


binaryOperator :: Parser BinaryOperator
binaryOperator = Parser $ \str -> 
  case str of
    "" -> Left "Expected Binary operator"
    h:t -> case M.lookup h ops of
      Just res -> return (t, res)
      Nothing -> Left ("Unknown binary operator: " ++ [h])
  where
    ops = fromList [('+', Plus), ('-', Minus), ('*', Multiply), ('/', Divide), ('^', Power)]



unaryOperator :: Parser UnaryOperator
unaryOperator = Parser $ \str ->
  case getWord str "" of 
    Left err -> Left err
    Right (h, t) -> case M.lookup h unaryOperators of
      Just res -> return (t, res)
      Nothing -> Left ("Unknown unary operator: " ++ h)
    
unaryOperators :: Map String UnaryOperator
unaryOperators = fromList [("sqrt", Square)]

-- Checks if the next character is a space or the end of the input
satisfyEndOrWhiteSpace :: Parser ()
satisfyEndOrWhiteSpace = Parser $ \str ->
  case str of
    "" -> Right (str, ()) -- End of the string
    (c:_) | isSpace c -> Right (str, ()) -- Next char is whitespace
    _ -> Left "Expected space or end of input"

parseIdent :: Parser String
parseIdent = do
    h <- satisfy isAlpha
    t <- go
    let ident = h : t
    if member ident unaryOperators then empty else return ident
  where
    go = (do
        x <- satisfy isAlphaNum
        y <- go
        return (x : y))
      <|>
        return []

parseBinOp :: Parser (Expr Double)
parseBinOp = do
  op <- binaryOperator
  satisfy (== ' ')
  f <- parseExpression
  satisfy (== ' ')
  BinOp (op) f <$> parseExpression


parseUnOp :: Parser (Expr Double)
parseUnOp = do
  op <- unaryOperator
  satisfy (== ' ')
  UnOp (op) <$> parseExpression

parseInt :: Parser Int
parseInt = do
  h <- satisfy isDigit
  t <- go
  return (stoi (h:t))
  where
    go = (do
        x <- satisfy isDigit
        y <- go
        return (x : y))
      <|>
        return []
    stoi = foldl1 (\a x -> a * 10 + x) . map digitToInt

parseConst :: Parser (Expr Double)
parseConst = do
  val <- parseInt
  satisfyEndOrWhiteSpace
  return (Const $ fromIntegral $ val)

parseVar :: Parser (Expr Double)
parseVar = do
  ident <- parseIdent
  satisfyEndOrWhiteSpace
  return (Var ident)

parseExpression :: Parser (Expr Double)
parseExpression = do
  parseBinOp <|> parseUnOp <|> parseVar <|> parseConst
