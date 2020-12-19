module Smos.ActionsSpec (spec) where

import qualified Data.Text as T
import Smos.Actions
import Smos.Default
import Smos.Types
import Test.Syd

spec :: Spec
spec =
  specify "All actions that are bound by default are in the allActions list" $
    let allActionNames = map anyActionName allActions
        configuredActions = keyMapActions defaultKeyMap
     in forM_ configuredActions $ \ca ->
          let an = anyActionName ca
           in unless (an `elem` allActionNames) $
                expectationFailure $
                  unwords
                    [ "This action was not found in the 'allActions' list:",
                      T.unpack (actionNameText an)
                    ]
