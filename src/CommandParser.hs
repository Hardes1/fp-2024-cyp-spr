{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module CommandParser(parseCommmand) where

import Parser(satisfy, Parser(..), getWord)
import ExprParser(parseIdent, parseExpression)
import Command (Command (..))
import Control.Applicative ((<|>))
import Text.Printf (printf)

parseKeyword :: String -> Parser ()
parseKeyword keyword = Parser $ \s ->
    case getWord s [] of
        Left err -> Left err
        Right (h, t) -> if h == keyword then return (t, ()) else Left ("Unknown keyword, expected" ++ keyword)

parseFilename :: Parser (Maybe String)
parseFilename = Parser $ \s ->
    case getWord s [] of
        Left _ -> return ([], Nothing)
        Right(h, _) -> return ([], Just h)

parseQuit :: Parser Command
parseQuit = do
    parseKeyword ":quit"
    return Quit

parseHelp :: Parser Command
parseHelp = do
    parseKeyword ":help"
    return Help

parseLet :: Parser Command
parseLet = do
    parseKeyword ":let"
    satisfy (== ' ')
    ident <- parseIdent
    satisfy (== ' ')
    Let ident <$> parseExpression

parseEval :: Parser Command
parseEval = do
    parseKeyword ":eval"
    satisfy (== ' ')
    Eval <$> parseExpression

parseEnv :: Parser Command
parseEnv = do
    parseKeyword ":env"
    name <- (satisfy (== ' ') >> parseFilename) <|> return Nothing
    return (Env name)

reportError :: Parser Command
reportError = Parser $ \s -> Left (printf "Unknown command '%s'" s)

parseCommmand :: Parser Command
parseCommmand = parseLet <|> parseEval <|> parseEnv <|> parseQuit <|> parseHelp <|> reportError