module CommandParser(parseCommmand) where

import Parser(runParser, satisfy, Parser(..), getWord)
import Command (Command (..))

parseKeyword :: String -> Parser ()
parseKeyword keyword = Parser $ \s ->
    case getWord s of 
        Left err -> Left err
        Right (h, t) -> if h == keyword then return (t, ()) else Left ("Unknown keyword, expected" ++ keyword)



parseQuit :: Parser Command
parseQuit = do
    parseKeyword ":quit"
    return Quit

parseCommmand :: Parser Command
parseCommmand = parseQuit
