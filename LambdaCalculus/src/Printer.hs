module Printer(printTerm) where

import Term (Term(..))
import Text.Printf (printf)

printTerm :: Term -> String
printTerm (Variable name) = printf "%s" [name]
printTerm (Lambda name body) = printf "\\%s.%s" [name] (printTerm body)
printTerm (Application le@(Variable _) ri@(Variable _)) = printf "%s %s" (printTerm le) (printTerm ri)
printTerm (Application app@(Application _ _) var@(Variable _)) = printf "%s %s" (printTerm app) (printTerm var)
printTerm (Application lambda@(Lambda _ _) var@(Variable _)) = printf "(%s) %s" (printTerm lambda) (printTerm var)
printTerm (Application var@(Variable _) app@(Application _ _)) = printf "%s (%s)" (printTerm var) (printTerm app)
printTerm (Application lambda@(Lambda _ _) app@(Application _ _)) = printf "(%s) %s" (printTerm lambda) (printTerm app)
printTerm (Application appLe@(Application _ _) appRi@(Application _ _)) = printf "%s (%s)" (printTerm appLe) (printTerm appRi)
printTerm (Application le@(Lambda _ _) ri@(Lambda _ _)) = printf "(%s) (%s)" (printTerm le) (printTerm ri)
printTerm (Application rest lambda@(Lambda _ _)) = printf "%s (%s)" (printTerm rest) (printTerm lambda)


