module Term(Term(..)) where


data Term = Variable Char | Lambda Char Term | Application Term Term deriving (Show, Eq) 