{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.RecurrenceSpec (spec) where

import qualified Data.DirForest as DF
import qualified Data.Map as M
import Data.Time
import Data.Time.Zones
import Data.Tree
import Smos.Data
import Smos.Directory.InterestingStore
import Smos.Directory.TestUtils
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render.Gen ()
import qualified System.Cron as Cron
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = do
  modifyMaxSuccess (`div` 10) $
    describe "readReccurrenceHistory" $ do
      it "always produces valid history" $ do
        withInterestingStore $ \dc -> do
          rh <- readReccurrenceHistory dc
          shouldBeValid rh
      it "works for this very specific complex example" $
        -- We have these projects
        -- A) Two unarchived
        -- B) Two archived
        -- C) One archived, one not
        forAllValid $ \shA ->
          forAllValid $ \tA1 ->
            forAll (genValid `suchThat` (>= tA1)) $ \tA2 ->
              forAll (genValid `suchThat` (/= shA)) $ \shB ->
                forAllValid $ \tB1 ->
                  forAll (genValid `suchThat` (>= tB1)) $ \tB2 ->
                    forAll (genValid `suchThat` (\sh -> sh /= shA && sh /= shB)) $ \shC ->
                      forAllValid $ \tC1 ->
                        forAll (genValid `suchThat` (>= tC1)) $ \tC2 ->
                          let mkSF sh t = addScheduleHashMetadata sh $ makeSmosFile [Node (entryWithState emptyHeader t "TODO") []]
                              errOrWorkflowDF =
                                DF.fromMap $
                                  M.fromList
                                    [ ("a-1.smos", DF.F (mkSF shA tA1)),
                                      ("a-2.smos", DF.F (mkSF shA tA2)),
                                      ("c-2.smos", DF.F (mkSF shC tC2))
                                    ]
                              errOrArchiveDF =
                                DF.fromMap $
                                  M.fromList
                                    [ ("b-1.smos", DF.F (mkSF shB tB1)),
                                      ("b-2.smos", DF.F (mkSF shB tB2)),
                                      ("c-1.smos", DF.F (mkSF shC tC1))
                                    ]
                           in case (,) <$> errOrWorkflowDF <*> errOrArchiveDF of
                                Left err -> expectationFailure $ show err
                                Right (workflowDF, archiveDF) ->
                                  let is =
                                        emptyInterestingStore
                                          { workflowFiles = workflowDF,
                                            archiveFiles = archiveDF
                                          }
                                   in withDirectorySettings is $ \dc -> do
                                        rh <- readReccurrenceHistory dc
                                        rh
                                          `shouldBe` M.fromList
                                            [ ( shA,
                                                LatestActivation
                                                  { latestActivationActivated = tA2,
                                                    latestActivationClosed = Nothing
                                                  }
                                              ),
                                              ( shB,
                                                LatestActivation
                                                  { latestActivationActivated = tB2,
                                                    latestActivationClosed = Just tB2
                                                  }
                                              ),
                                              ( shC,
                                                LatestActivation
                                                  { latestActivationActivated = tC2,
                                                    latestActivationClosed = Nothing
                                                  }
                                              )
                                            ]

  describe "parseSmosFileSchedule" $
    it "can parse schedule hash that was added with addScheduleHashMetadata" $
      forAllValid $ \sf ->
        forAllValid $ \sih -> do
          parseSmosFileSchedule (addScheduleHashMetadata sih sf) `shouldBe` Just sih

  describe "computeNextRun" $ do
    it "always activates a new item" $
      forAllValid $ \zone ->
        forAllValid $ \now ->
          forAllValid $ \si -> do
            case computeNextRun zone now M.empty si of
              Left DoNotActivateHaircut -> expectationFailure "should have activated."
              Right DoNotActivateRent -> expectationFailure "should have activated."
              _ -> pure ()

    it "does not crash" $
      forAllValid $ \zone ->
        forAllValid $ \now ->
          forAllValid $ \rh ->
            forAllValid $ \si ->
              forAllValid $ \mla ->
                let rh' = maybe rh (\la -> M.insert (hashScheduleItem si) la rh) mla
                 in shouldBeValid $ computeNextRun zone now rh' si

  describe "rentNextRun" $ do
    it "activates 'every day' in the next day after the last activation" $
      forAllValid $ \zone ->
        forAllValid $ \open ->
          forAllValid $ \mClosed ->
            let la = LatestActivation (localTimeToUTCTZ zone open) mClosed
             in rentNextRun zone la Cron.daily `shouldBe` utcToLocalTime utc <$> Cron.nextMatch Cron.daily (localTimeToUTC utc open)

  describe "haircutNextRun" $ do
    it "does not active if the previous is not closed" $
      forAllValid $ \open ->
        forAllValid $ \ndt ->
          let la = LatestActivation open Nothing
           in haircutNextRun la ndt `shouldBe` Nothing

    it "activates in the next day after the last closing" $
      forAllValid $ \open ->
        forAllValid $ \closedDay ->
          let la = LatestActivation open (Just (UTCTime closedDay 0))
           in haircutNextRun la nominalDay `shouldBe` Just (UTCTime (addDays 1 closedDay) 0)
