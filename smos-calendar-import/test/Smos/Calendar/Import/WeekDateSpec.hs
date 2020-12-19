module Smos.Calendar.Import.WeekDateSpec
  ( spec,
  )
where

import Data.Time
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.WeekDate
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  describe "firstDayOfTheWSWeekThatContainsJan1st" $ do
    it "works for a day in 2014 with a Monday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Monday 2014 `shouldBe` d 2013 12 30
    it "works for a day in 2015 with a Monday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Monday 2015 `shouldBe` d 2014 12 29
    it "works for a day in 2014 with a Sunday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Sunday 2014 `shouldBe` d 2013 12 29
    it "works for a day in 2015 with a Sunday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Sunday 2015 `shouldBe` d 2014 12 28
    it "works for a day in 2014 with a Thursday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Thursday 2014 `shouldBe` d 2013 12 26
    it "works for a day in 2015 with a Thursday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Thursday 2015 `shouldBe` d 2015 01 01
  describe "toWeekDateWithStart" $ do
    describe "examples" $ do
      it "works for 2013-12-29 in 2013 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2013 12 29) `shouldBe` (2013, 52, Sunday)
      it "works for 2014-01-01 in 2014 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 01 01) `shouldBe` (2014, 01, Wednesday)
      it "works for 2014-01-05 in 2014 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 01 05) `shouldBe` (2014, 01, Sunday)
      it "works for 2013-12-29 in 2013 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2013 12 29) `shouldBe` (2014, 01, Sunday)
      it "works for 2014-01-01 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 01 01) `shouldBe` (2014, 01, Wednesday)
      it "works for 2014-01-05 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 01 05) `shouldBe` (2014, 02, Sunday)
      it "works for 2014-12-29 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 12 29) `shouldBe` (2015, 01, Monday)
      it "works for 2015-01-01 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2015 01 01) `shouldBe` (2015, 01, Thursday)
      it "works for 2015-01-05 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2015 01 05) `shouldBe` (2015, 02, Monday)
      it "works for 2014-12-29 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 12 29) `shouldBe` (2014, 53, Monday)
      it "works for 2015-01-01 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2015 01 01) `shouldBe` (2014, 53, Thursday)
      it "works for 2015-01-05 in 2015 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2015 01 05) `shouldBe` (2015, 01, Monday)
      it "works for 1997-01-01 in 1997 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 1997 01 01) `shouldBe` (1997, 01, Wednesday)
      it "works for 1997-01-07 in 1997 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 1997 01 07) `shouldBe` (1997, 02, Tuesday)
      it "works for 1997-01-01 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 01 01) `shouldBe` (1997, 01, Wednesday)
      it "works for 1997-01-07 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 01 07) `shouldBe` (1997, 02, Tuesday)
      it "works for 1997-08-17 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 08 17) `shouldBe` (1997, 34, Sunday)
      it "works for 1997-08-17 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Monday (d 1997 08 17) `shouldBe` (1997, 33, Sunday)
    it "produces tuples of which the last value is independent of the week start" $
      forAllValid $ \ws1 ->
        forAllValid $ \ws2 ->
          forAllValid $ \day ->
            let (_, _, dow1) = toWeekDateWithStart ws1 day
                (_, _, dow2) = toWeekDateWithStart ws2 day
             in dow1 `shouldBe` dow2
    it "produces valid results" $ producesValidsOnValids2 toWeekDateWithStart
  describe "fromWeekDateWithStart" $ do
    describe "examples" $ do
      it "works for 2013-12-29 in 2013 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2013 52 Sunday `shouldBe` Just (d 2013 12 29)
      it "works for 2014-01-01 in 2014 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2014 01 Wednesday `shouldBe` Just (d 2014 01 01)
      it "works for 2014-01-05 in 2014 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2014 01 Sunday `shouldBe` Just (d 2014 01 05)
      it "works for 2013-12-29 in 2013 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 01 Sunday `shouldBe` Just (d 2013 12 29)
      it "works for 2014-01-01 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 01 Wednesday `shouldBe` Just (d 2014 01 01)
      it "works for 2014-01-05 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 02 Sunday `shouldBe` Just (d 2014 01 05)
      it "works for 2014-12-29 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 01 Monday `shouldBe` Just (d 2014 12 29)
      it "works for 2015-01-01 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 01 Thursday `shouldBe` Just (d 2015 01 01)
      it "works for 2015-01-05 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 02 Monday `shouldBe` Just (d 2015 01 05)
      it "works for 2014-12-29 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 53 Monday `shouldBe` Just (d 2014 12 29)
      it "works for 2015-01-01 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 53 Thursday `shouldBe` Just (d 2015 01 01)
      it "works for 2015-01-05 in 2015 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2015 01 Monday `shouldBe` Just (d 2015 01 05)
    it "produces valid results" $ forAllValid $ producesValidsOnValids3 . fromWeekDateWithStart
    xdescribe "This fails for very negative years for unknown reasons" $
      it "roundtrips toWeekDateWithStart for a given week start" $
        forAllValid $
          \ws ->
            forAllValid $ \day ->
              let (y, wn, dow) = toWeekDateWithStart ws day
               in fromWeekDateWithStart ws y wn dow `shouldBe` Just day
