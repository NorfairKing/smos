{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.RecurrenceRule.Gen where

import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Set as S
import Smos.Calendar.Import.RecurrenceRule
import Smos.Data.Gen
import Test.QuickCheck

-- | Until we have it in time and then in genvalidity-time
instance GenValid Month where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid Frequency where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid UntilCount where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ pure Indefinitely,
        Count <$> sized (\s -> max 1 <$> choose (1, fromIntegral s)),
        Until <$> genImpreciseLocalTime
      ]

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

instance GenValid ByMonthDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    MonthDay
      <$> oneof
        [ choose (1, 31),
          choose (-31, - 1)
        ]

instance GenValid ByYearDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    YearDay
      <$> oneof
        [ choose (1, 366),
          choose (-366, - 1)
        ]

instance GenValid ByWeekNo where
  shrinkValid = shrinkValidStructurally
  genValid =
    WeekNo
      <$> oneof
        [ choose (1, 53),
          choose (-53, - 1)
        ]

instance GenValid BySetPos where
  shrinkValid = shrinkValidStructurally
  genValid = SetPos <$> sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)])

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
      genSetOf
        ( case rRuleFrequency of
            Monthly -> Every <$> genValid
            Yearly -> Every <$> genValid
            _ -> genValid
        )
    rRuleByMonthDay <- case rRuleFrequency of
      Weekly -> pure S.empty
      _ -> genValid
    rRuleByYearDay <- case rRuleFrequency of
      Daily -> pure S.empty
      Weekly -> pure S.empty
      Monthly -> pure S.empty
      _ -> genValid
    rRuleByWeekNo <- case rRuleFrequency of
      Yearly -> genValid
      _ -> pure S.empty
    rRuleByMonth <- genValid
    rRuleWeekStart <- genValid
    rRuleBySetPos <-
      let anyOtherBySpecified =
            any
              not
              [ S.null rRuleBySecond,
                S.null rRuleByMinute,
                S.null rRuleByHour,
                S.null rRuleByDay,
                S.null rRuleByMonthDay,
                S.null rRuleByYearDay,
                S.null rRuleByWeekNo,
                S.null rRuleByMonth
              ]
       in if anyOtherBySpecified
            then genValid
            else pure S.empty
    pure RRule {..}

genDailyRecurrence :: Gen RRule
genDailyRecurrence = do
  let rRuleFrequency = Daily
  rRuleInterval <- genValid
  rRuleUntilCount <- genValid
  rRuleBySecond <- genValid
  rRuleByMinute <- genValid
  rRuleByHour <- genValid
  rRuleByDay <-
    genSetOf
      ( case rRuleFrequency of
          Monthly -> Every <$> genValid
          Yearly -> Every <$> genValid
          _ -> genValid
      )
  rRuleByMonthDay <- case rRuleFrequency of
    Weekly -> pure S.empty
    _ -> genValid
  let rRuleByYearDay = S.empty
      rRuleByWeekNo = S.empty
  rRuleByMonth <- genValid
  rRuleWeekStart <- genValid
  rRuleBySetPos <-
    let anyOtherBySpecified =
          any
            not
            [ S.null rRuleBySecond,
              S.null rRuleByMinute,
              S.null rRuleByHour,
              S.null rRuleByDay,
              S.null rRuleByMonthDay,
              S.null rRuleByYearDay,
              S.null rRuleByWeekNo,
              S.null rRuleByMonth
            ]
     in if anyOtherBySpecified
          then genValid
          else pure S.empty
  pure RRule {..}
