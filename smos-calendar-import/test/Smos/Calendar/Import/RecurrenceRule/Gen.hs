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
    rRuleInterval <- sized $ \s -> max 1 <$> choose (1, fromIntegral s) -- no point in generating huge words
    rRuleUntilCount <- genValid
    rRuleBySecond <- oneof [pure Nothing, Just <$> genNonEmptyOf (choose (0, 60))]
    rRuleByMinute <- oneof [pure Nothing, Just <$> genNonEmptyOf (choose (0, 59))]
    rRuleByHour <- oneof [pure Nothing, Just <$> genNonEmptyOf (choose (0, 23))]
    rRuleByDay <-
      oneof
        [ pure Nothing,
          Just
            <$> genNonEmptyOf
              ( case rRuleFrequency of
                  Monthly -> Every <$> genValid
                  Yearly -> Every <$> genValid
                  _ -> genValid
              )
        ]
    rRuleByMonthDay <- case rRuleFrequency of
      Weekly -> pure Nothing
      _ ->
        oneof
          [ pure Nothing,
            Just
              <$> genNonEmptyOf
                ( oneof
                    [ choose (1, 31),
                      choose (-31, - 1)
                    ]
                )
          ]
    rRuleByYearDay <- case rRuleFrequency of
      Daily -> pure Nothing
      Weekly -> pure Nothing
      Monthly -> pure Nothing
      _ ->
        oneof
          [ pure Nothing,
            Just
              <$> genNonEmptyOf
                ( oneof
                    [ choose (1, 366),
                      choose (-366, - 1)
                    ]
                )
          ]
    rRuleByWeekNo <-
      oneof
        [ pure Nothing,
          Just
            <$> genNonEmptyOf
              ( oneof
                  [ choose (1, 53),
                    choose (-53, - 1)
                  ]
              )
        ]
    rRuleByMonth <- oneof [pure Nothing, Just <$> genNonEmptyOf (choose (1, 12))]
    rRuleWeekStart <- genValid
    rRuleBySetPos <- oneof [pure Nothing, Just <$> genNonEmptyOf (sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)]))]
    pure RRule {..}
