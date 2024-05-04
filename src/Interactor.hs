module Interactor(runREPL) where
import Control.Monad (forever)
import Parser (Parser(runParser))
import CommandParser (parseCommmand)
import Command(Command(..))
import GHC.Base (MonadPlus(mzero))
import StateDemo(execState)
import Expr (eval, Expr)
import Data.Map.Strict as Map(empty, Map, insert)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.State.Lazy (evalStateT)



runREPL :: IO ()
runREPL = printHelp >> runMaybeT (evalStateT repl Map.empty) >> putStrLn "Exiting REPL..."

repl :: StateT (Map String Double) (MaybeT IO) ()
repl = forever $ do
    input <- lift . lift $ getLine
    case runParser parseCommmand input of
        Left err -> lift . lift $ print err
        Right command -> handleCommand $ snd command

handleCommand :: Command -> StateT (Map String Double) (MaybeT IO) ()
handleCommand Quit = mzero
handleCommand Help = lift . lift $ printHelp
handleCommand (Let ident expr) = do
    env <- get
    let resultState = execState (eval expr) env
    case resultState of
        Left err -> lift . lift $ print err
        Right res -> modify (insert ident res)

handleCommand (Eval expr) = do
    env <- get
    let resultState = execState (eval expr) env
    lift . lift $ print resultState
handleCommand (Env maybeFilename) = do
    env <- get
    case maybeFilename of
        Nothing -> lift . lift $ print env
        Just filename -> lift . lift $ writeFile filename (show env)


printHelp :: IO ()
printHelp = do
    putStrLn ":let <ident> <expr> - set value <expr> to variable <ident>"
    putStrLn ":eval <expr> - evaluate expression <expr>"
    putStrLn ":env [filename] - print current set variables to stdout or [filename]"
    putStrLn ":help - print help"
    putStrLn ":quit - finish the programm"
