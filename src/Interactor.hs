module Interactor(runREPL) where
import Expr (Expr)

data Command = Let String (Expr Double) | Eval (Expr Double) | Env (Maybe String) | Quit


runREPL :: IO ()
runREPL = do
    printHelp
    -- value <- msum $ repeat getPassword


printHelp :: IO ()
printHelp = do
    putStrLn ":let <ident> <expr> - set value <expr> to variable <ident>"
    putStrLn ":eval <expr> - evaluate expression <expr>"
    putStrLn ":env [filename] - print current set variables to stdout or [filename]"
    putStrLn ":quit - finish the programm" 
