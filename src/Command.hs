module Command(Command(..)) where
import Expr

data Command = Let String (Expr Double) | Eval (Expr Double) | Env (Maybe String) | Quit