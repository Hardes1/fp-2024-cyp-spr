module Interactor(run) where
    
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Class (MonadTrans(lift))

run :: MaybeT IO ()
run = do
    printHelp




printHelp :: MaybeT IO ()
printHelp = do
    lift $ putStrLn ":let <ident> <expr> - set value <expr> to variable <ident>"
    lift $ putStrLn ":eval <expr> - evaluate expression <expr>"
    lift $ putStrLn ":env [filename] - print current set variables to stdout or [filename]"
    lift $ putStrLn ":quit - finish the programm" 
