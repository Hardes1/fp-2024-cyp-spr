module Test.Parser where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Expr(Expr(..), getPrefixNotation)
import Util.Generator(genExpr)
import Util.Parser(parseExpr)
import Test.Tasty.Hedgehog

parserCompositionShow :: Property
parserCompositionShow = property $ do
  expr <- forAll $ (genExpr 100)
  let s = getPrefixNotation expr
  case parseExpr s of
    Right ("", parsedExpr) -> assert (expr == parsedExpr)
    _ -> assert False


props :: [TestTree]
props =
  [ testProperty "Result of prefix notation is parsed to the same expression" parserCompositionShow
  ]