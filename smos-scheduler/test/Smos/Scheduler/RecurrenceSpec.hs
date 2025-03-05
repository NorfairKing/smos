{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.RecurrenceSpec (spec) where

import qualified Data.DirForest as DF
import qualified Data.Map as M
import Data.Time
import Data.Tree
import Smos.Archive.Commands.File (archiveTimeFormat)
import Smos.Data
import Smos.Directory.InterestingStore
import Smos.Directory.TestUtils
import Smos.Scheduler.History
import Smos.Scheduler.History.Gen ()
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Schedule.Gen ()
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
                          let mkSF sh t = addScheduleMetadata t sh $ makeSmosFile [Node emptyEntry []]
                              errOrWorkflowDF =
                                DF.fromMap $
                                  M.fromList
                                    [ ("a-1.smos", DF.F (mkSF shA tA1)),
                                      ("a-2.smos", DF.F (mkSF shA tA2)),
                                      ("c-2.smos", DF.F (mkSF shC tC2))
                                    ]
                              errOrArchiveDF =
                                DF.fromMap $
                                  M.fromList $
                                    let mkTup s n t = (s <> "_" <> formatTime defaultTimeLocale archiveTimeFormat t <> ".smos", DF.F (mkSF n t))
                                     in [ mkTup "b-1" shB tB1,
                                          mkTup "b-2" shB tB2,
                                          mkTup "c-1" shC tC1
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
                                                    latestActivationClosed = Just (mkImpreciseLocalTime tB2)
                                                  }
                                              ),
                                              ( shC,
                                                LatestActivation
                                                  { latestActivationActivated = tC2,
                                                    latestActivationClosed = Nothing
                                                  }
                                              )
                                            ]

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
