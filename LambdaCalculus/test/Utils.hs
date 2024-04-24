module Utils(parseInput, constructErrorMessage) where
import Term (Term)
import Parser(parseTermStart)
import Text.Parsec (parse)
import Data.Either.Extra(mapLeft)
import Text.Printf

parseInput :: String -> Either String Term
parseInput input = mapLeft show $ parse parseTermStart "" input

constructErrorMessage :: Int -> String -> String -> String
constructErrorMessage = printf "(line 1, column %d):\nunexpected %s\nexpecting %s"