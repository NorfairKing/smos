{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.PickSpec (spec) where

import Data.Time
import qualified ICal
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.RecurringEvent
import Test.Syd

spec :: Spec
spec = describe "pickedInclusion" $ do
  -- Neither of these components may be dropped while picking: one with a
  -- RECURRENCE-ID has to reach recurrence to replace the instance it names, so
  -- the decision is recorded here and acted on after reconciliation.
  let eventWith :: Maybe ICal.Status -> Maybe ICal.Description -> ICal.Event
      eventWith mStatus mDescription =
        (ICal.makeEvent (ICal.UID "test") (ICal.DateTimeStamp (ICal.DateTimeUTC (UTCTime (fromGregorian 1970 01 01) 0))))
          { ICal.eventStatus = mStatus,
            ICal.eventDescription = mDescription
          }

  it "includes an ordinary component" $
    pickedInclusion (eventWith Nothing Nothing) `shouldBe` Include

  it "excludes a cancelled component" $
    -- STATUS:CANCELLED on a component with a RECURRENCE-ID is how "delete this
    -- one instance" is written.
    pickedInclusion (eventWith (Just ICal.StatusCancelled) Nothing) `shouldBe` Exclude

  it "includes a component with some other status" $
    pickedInclusion (eventWith (Just ICal.StatusConfirmed) Nothing) `shouldBe` Include

  it "excludes a component whose description opts out" $
    pickedInclusion (eventWith Nothing (Just (ICal.makeDescription "Foo SMOS_NO_CALENDAR_IMPORT bar")))
      `shouldBe` Exclude

  it "includes a component whose description does not mention the opt-out" $
    pickedInclusion (eventWith Nothing (Just (ICal.makeDescription "Foo bar")))
      `shouldBe` Include
