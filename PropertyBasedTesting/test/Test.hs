import Test.Tasty

import qualified Test.Simplify
import qualified Test.Parser
main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Simplify expr" Test.Simplify.props
                , testGroup "Parser expr" Test.Simplify.props
                ])
