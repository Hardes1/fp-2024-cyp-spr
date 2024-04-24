{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser(parseTermStart) where

import Text.Parsec
import Term(Term(..))
import Text.Parsec.String (Parser)


parseVariable :: Parser Term
parseVariable = Variable <$> letter


parseLambda :: Parser Term
parseLambda = do
    char '\\'
    v <- letter
    char '.'
    Lambda v <$> parseTerm

application :: Parser Term
application = do
    firstTerm <- parseInApplicationTerm
    restTerms <- many1 (char ' ' >> parseInApplicationTerm)
    return $ foldl1 Application (firstTerm:restTerms)


-- this trick required in order not to go to infinity recursion, because otherwise we will try to parseApplication infinitely
parseInApplicationTerm :: Parser Term
parseInApplicationTerm = try parseLambda <|> try parseVariable <|> parens parseTerm

parseTerm :: Parser Term
parseTerm = try application <|> try parseLambda <|> try parseVariable <|> parens parseTerm

parseTermStart :: Parser Term
parseTermStart = parseTerm <* eof

parens :: Parser Term -> Parser Term
parens = between (char '(') (char ')')