module Main (main) where

import Interactor(runREPL)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))

main :: IO ()
main = do
    putStrLn "Welcome to the REPL of Expressions!"
    runREPL
    return ()