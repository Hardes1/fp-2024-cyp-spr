# LambdaCalculus

## Parser
The entry point for this functionality is Parser#parseTermStart function, which accepts the String type - your input.

Examples of usage:

```
parseTerm "x" returns Variable 'x'
parseTerm "x y" returns Application (Variable 'x') (Variable 'y')
parseTerm "\\x.x" returns Lambda 'x' (Variable 'x')
```

## Pretty-Printer
The entry point for this functionality is Printer#printTerm function, which accepts the Term and returns string.
Term is (source code can be found in src/ folder):
```
data Term = Variable Char | Lambda Char Term | Application Term Term deriving (Show, Eq) 
```

Examples of usage:
```
printTerm (Variable 'x') returns "x"
printTerm (Application (Variable 'x') (Variable 'x')) returns "x x"
printTerm (Lambda 'x' (Variable 'x')) returns "\\x.x"
```