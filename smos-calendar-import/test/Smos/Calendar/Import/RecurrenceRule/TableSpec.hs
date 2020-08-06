{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.RecurrenceRule.TableSpec
  ( spec,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen
import Smos.Calendar.Import.UnresolvedTimestamp
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec =
  describe "dailyNextRecurrence" $ do
    specify "ByMonth limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule ->
        forAllValid $ \limit -> do
          let withoutRule = rrule {rRuleByMonth = []}
          let with = rruleOccurrencesUntil cur rrule limit
          print (S.size with)
          let without = rruleOccurrencesUntil cur rrule limit
          print (S.size without)
          S.size with `shouldSatisfy` (<= (S.size without))
