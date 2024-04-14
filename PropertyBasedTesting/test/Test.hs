import Test.Tasty

import qualified Test.Simplify
import qualified Test.Parse
import qualified Test.Eval
main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Simplify expr" Test.Simplify.props
                , testGroup "Parse expr" Test.Parse.props
                , testGroup "Eval expr" Test.Eval.props
                ])
