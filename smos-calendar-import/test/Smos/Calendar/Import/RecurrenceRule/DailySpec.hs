module Smos.Calendar.Import.RecurrenceRule.DailySpec
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
    specify "No ByX's" $ forAllValid $ \tod ->
      dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) (Interval 2) [] [] [] [] [] [] []
        `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) tod)
    describe "ByMonth" $ do
      it "Every three days in September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) (Interval 3) [September] [] [] [] [] [] []
          `shouldBe` Just
            (LocalTime (fromGregorian 2020 09 01) tod)
      it "Every four days in August" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) (Interval 4) [August] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 10) tod)
    describe "ByMonthDay" $ do
      it "Every tenth day of the month" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) (Interval 1) [] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 10) tod)
      it "Every tenth of September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2019 09 10) tod) (Interval 1) [September] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 10) tod)
    describe "ByDay" $ do
      it "Every tuesday" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) (Interval 1) [] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 11 10) tod)
      it "Every tuesday in September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) (Interval 1) [September] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 01) tod)
    describe "ByHour" $ do
      it "16h every other day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 00 00)) (Interval 2) [] [] [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) (TimeOfDay 16 00 00))
    describe "ByMinute" $ do
      it "16h20 every third day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 00)) (Interval 3) [] [] [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 09) (TimeOfDay 16 20 00))
    describe "ByMinute" $ do
      it "16h20m30s every fourth day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 00)) (Interval 3) [] [] [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 10) (TimeOfDay 16 20 30))
