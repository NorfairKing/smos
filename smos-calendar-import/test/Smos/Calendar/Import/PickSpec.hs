{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.PickSpec (spec) where

import Data.Time
import qualified ICal
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static
import Test.Syd

spec :: Spec
spec = do
  describe "pickedBusy" $ do
    let eventWith :: Maybe ICal.Status -> ICal.Transparency -> ICal.Event
        eventWith mStatus transparency =
          (ICal.makeEvent (ICal.UID "test") (ICal.DateTimeStamp (ICal.DateTimeUTC (UTCTime (fromGregorian 1970 01 01) 0))))
            { ICal.eventStatus = mStatus,
              ICal.eventTransparency = transparency
            }

    it "is Busy for an event with no TRANSP and no STATUS" $
      -- @
      -- ;Default value is OPAQUE
      -- @
      pickedBusy (eventWith Nothing ICal.TransparencyOpaque) `shouldBe` Busy

    it "is Free when TRANSP is TRANSPARENT" $
      -- [section 3.8.2.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.7)
      --
      -- @
      -- Other
      -- events, which do not take up the individual's (or resource's) time
      -- SHOULD be recorded as TRANSPARENT, making them invisible to free/
      -- busy time searches.
      -- @
      pickedBusy (eventWith Nothing ICal.TransparencyTransparent) `shouldBe` Free

    it "is Busy when the event is confirmed" $
      pickedBusy (eventWith (Just ICal.StatusConfirmed) ICal.TransparencyOpaque) `shouldBe` Busy

    it "is Free when the event is only tentative" $
      -- [section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11)
      --
      -- @
      -- statvalue-event = "TENTATIVE"    ;Indicates event is tentative.
      --                 / "CONFIRMED"    ;Indicates event is definite.
      -- @
      --
      -- TRANSP says whether the event would consume time; STATUS says whether
      -- it is happening at all.  An event that is only tentative has not been
      -- agreed to, so it does not block out time the way a definite one does.
      pickedBusy (eventWith (Just ICal.StatusTentative) ICal.TransparencyOpaque) `shouldBe` Free

    it "is Free when the event is both tentative and transparent" $
      pickedBusy (eventWith (Just ICal.StatusTentative) ICal.TransparencyTransparent) `shouldBe` Free

  describe "pickedInclusion" $ do
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
