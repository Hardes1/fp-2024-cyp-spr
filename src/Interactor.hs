module Interactor(runREPL) where
import Control.Monad (forever)
import Parser (Parser(runParser))
import CommandParser (parseCommmand)
import Command(Command(..))
import GHC.Base (MonadPlus(mzero))
import StateDemo(execState)
import Expr (eval)
import Data.Map.Strict as Map(empty)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans.Class (lift)

runREPL :: IO ()
runREPL = runMaybeT repl >> putStrLn "Exiting REPL..."

repl :: MaybeT IO ()
repl = forever $ do
    lift printHelp
    input <- lift getLine
    case runParser parseCommmand input of
        Left err -> lift $ print err
        Right command -> handleCommand $ snd command

handleCommand :: Command -> MaybeT IO ()
handleCommand Quit = mzero
handleCommand (Let ident expr) = lift $ putStrLn ("Setting " ++ ident ++ " to " ++ show expr)
handleCommand (Eval expr) = do
    let resultState = execState (eval expr) Map.empty
    lift $ putStrLn $ show resultState
handleCommand (Env maybeFilename) =
    case maybeFilename of
        Nothing -> lift $ putStrLn "Printing environment to stdout"
        Just filename -> lift $ putStrLn ("Printing environment to file: " ++ filename)


printHelp :: IO ()
printHelp = do
    putStrLn ":let <ident> <expr> - set value <expr> to variable <ident>"
    putStrLn ":eval <expr> - evaluate expression <expr>"
    putStrLn ":env [filename] - print current set variables to stdout or [filename]"
    putStrLn ":quit - finish the programm"
