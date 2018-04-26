module Main (main) where

import qualified Test.Brig.Types.TURN
import qualified Test.Brig.Types.Common
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Test.Brig.Types.TURN.tests
    , Test.Brig.Types.Common.tests
    ]
