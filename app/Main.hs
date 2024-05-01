module Main (main) where

import Interactor(run)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT)

main :: IO ()
main = do
    return ()