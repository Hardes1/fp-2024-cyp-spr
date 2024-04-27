module Util.Parser where
import Parser(runParser, parseExpression)
import Expr(Expr(..))

parseExpr :: String -> Either String (String, Expr Double)
parseExpr = runParser parseExpression