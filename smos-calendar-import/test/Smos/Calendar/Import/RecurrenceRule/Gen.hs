{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.RecurrenceRule.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Maybe
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.UnresolvedTimestamp.Gen
import Test.QuickCheck

instance GenValid DayOfWeek where -- Until we have it in genvalidity-time
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid Frequency where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid UntilCount where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid Interval where
  shrinkValid = shrinkValidStructurally
  genValid = Interval <$> sized (\s -> max 1 <$> choose (1, fromIntegral s)) -- no point in generating huge words

instance GenValid BySecond where
  shrinkValid = shrinkValidStructurally
  genValid = Second <$> choose (0, 60)

instance GenValid ByMinute where
  shrinkValid = shrinkValidStructurally
  genValid = Minute <$> choose (0, 59)

instance GenValid ByHour where
  shrinkValid = shrinkValidStructurally
  genValid = Hour <$> choose (0, 23)

instance GenValid ByDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ Every <$> genValid,
        Specific <$> sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)]) <*> genValid
      ]

instance GenValid RRule where
  shrinkValid = shrinkValidStructurally
  genValid = do
    rRuleFrequency <- genValid
    rRuleInterval <- genValid
    rRuleUntilCount <- genValid
    rRuleBySecond <- genValid
    rRuleByMinute <- genValid
    rRuleByHour <- genValid
    rRuleByDay <-
      genListOf
        ( case rRuleFrequency of
            Monthly -> Every <$> genValid
            Yearly -> Every <$> genValid
            _ -> genValid
        )
    rRuleByMonthDay <- case rRuleFrequency of
      Weekly -> pure []
      _ ->
        genListOf
          ( oneof
              [ choose (1, 31),
                choose (-31, - 1)
              ]
          )
    rRuleByYearDay <- case rRuleFrequency of
      Daily -> pure []
      Weekly -> pure []
      Monthly -> pure []
      _ ->
        genListOf
          ( oneof
              [ choose (1, 366),
                choose (-366, - 1)
              ]
          )
    rRuleByWeekNo <-
      genListOf
        ( oneof
            [ choose (1, 53),
              choose (-53, - 1)
            ]
        )
    rRuleByMonth <- genListOf (choose (1, 12))
    rRuleWeekStart <- genValid
    rRuleBySetPos <- genListOf (sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)]))
    pure RRule {..}
