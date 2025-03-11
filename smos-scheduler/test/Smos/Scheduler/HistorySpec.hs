{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.HistorySpec (spec) where

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
import Smos.Scheduler.Schedule.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = do
  modifyMaxSuccess (`div` 10) $
    describe "readReccurrenceHistory" $ do
      it "does not crash" $
        withInterestingStore $ \dc -> do
          history <- readReccurrenceHistory dc
          shouldBeValid history
      it "works for this very specific complex example" $
        -- We have these projects
        -- A) Two unarchived
        -- B) Two archived
        -- C) One archived, one not
        forAllValid $
          \shA ->
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
                                                    { latestActivationActivated = mkImpreciseLocalTime tA2,
                                                      latestActivationClosed = Nothing
                                                    }
                                                ),
                                                ( shB,
                                                  LatestActivation
                                                    { latestActivationActivated = mkImpreciseLocalTime tB2,
                                                      latestActivationClosed = Just (mkImpreciseLocalTime tB2)
                                                    }
                                                ),
                                                ( shC,
                                                  LatestActivation
                                                    { latestActivationActivated = mkImpreciseLocalTime tC2,
                                                      latestActivationClosed = Nothing
                                                    }
                                                )
                                              ]

  describe "computeLastRun" $
    it "produces valid times" $
      producesValid2 computeLastRun

  describe "addScheduleMetadata" $
    it "produces valid times" $
      producesValid3 addScheduleMetadata

  describe "parseSmosFileScheduleMetadata" $
    it "can parse schedule hash that was added with addScheduleMetadata" $
      forAllValid $ \lt ->
        forAllValid $ \sf ->
          forAllValid $ \sih -> do
            parseSmosFileScheduleMetadata (addScheduleMetadata lt sih sf) `shouldBe` Just (sih, Just (mkImpreciseLocalTime lt))
