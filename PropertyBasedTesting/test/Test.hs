import Test.Tasty

import qualified Test.Simplify

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Simplify expr" Test.Simplify.props
                ])
