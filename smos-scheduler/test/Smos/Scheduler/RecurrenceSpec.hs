{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.RecurrenceSpec (spec) where

import qualified Data.Map as M
import Data.Time
import Smos.Scheduler.History
import Smos.Scheduler.History.Gen ()
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Schedule.Gen ()
import qualified System.Cron as Cron
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = do
  describe "computeNextRun" $ do
    it "always activates a new item" $
      forAllValid $ \now ->
        forAllValid $ \sn ->
          forAllValid $ \si ->
            case computeNextRun now M.empty sn si of
              Left DoNotActivateHaircut -> expectationFailure "should have activated."
              Right DoNotActivateRent -> expectationFailure "should have activated."
              _ -> pure ()

    it "does not crash" $
      forAllValid $ \now ->
        forAllValid $ \rh ->
          forAllValid $ \sn ->
            forAllValid $ \si ->
              forAllValid $ \mla ->
                let rh' = maybe rh (\la -> M.insert sn la rh) mla
                 in shouldBeValid $ computeNextRun now rh' sn si

  describe "rentNextRun" $ do
    it "activates 'every day' in the next day after the last activation" $
      forAllValid $ \open ->
        forAllValid $ \mClosed ->
          let la = LatestActivation open mClosed
           in rentNextRun la Cron.daily `shouldBe` utcToLocalTime utc <$> Cron.nextMatch Cron.daily (localTimeToUTC utc open)

  describe "haircutNextRun" $ do
    it "does not active if the previous is not closed" $
      forAllValid $ \open ->
        forAllValid $ \ndt ->
          let la = LatestActivation open Nothing
           in haircutNextRun la ndt `shouldBe` Nothing

    it "activates in the next day after the last closing" $
      forAllValid $ \open ->
        forAllValid $ \closedDay ->
          let la = LatestActivation open (Just (LocalTime closedDay midnight))
           in haircutNextRun la nominalDay `shouldBe` Just (LocalTime (addDays 1 closedDay) midnight)
