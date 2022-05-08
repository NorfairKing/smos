{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.ProjectsSpec where

import Data.Tree
import Smos.Data
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Projects
import Smos.Report.Projects.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @ProjectsReport
  jsonSpec @ProjectsReport
  genValidSpec @ProjectEntry
  jsonSpec @ProjectEntry
  describe "getCurrentEntry" $ do
    it "produces valid entries" $
      producesValid getCurrentEntry

    it "finds nothing in an empty file" $
      getCurrentEntry emptySmosFile `shouldBe` Nothing

    it "finds a STARTING action" $
      forAllValid $ \time -> do
        case entrySetState time (Just "STARTED") (newEntry "bar") of
          Nothing -> expectationFailure "should not happen."
          Just startedEntry -> getCurrentEntry (makeSmosFile [Node startedEntry []]) `shouldBe` Just startedEntry

    it "finds the first of two NEXT entries" $
      forAllValid $ \time1 ->
        forAllValid $ \time2 -> do
          case (,) <$> entrySetState time1 (Just "STARTED") (newEntry "bar") <*> entrySetState time2 (Just "STARTED") (newEntry "bar") of
            Nothing -> expectationFailure "should not happen."
            Just (startedEntry, nextEntry) ->
              getCurrentEntry
                ( makeSmosFile
                    [ Node startedEntry [],
                      Node nextEntry []
                    ]
                )
                `shouldBe` Just startedEntry

    it "finds a NEXT action even under an entry without a state" $
      forAllValid $ \time -> do
        case entrySetState time (Just "NEXT") (newEntry "bar") of
          Nothing -> expectationFailure "should not happen."
          Just nextEntry ->
            getCurrentEntry (makeSmosFile [Node (newEntry "foo") [Node nextEntry []]])
              `shouldBe` Just nextEntry

  describe "makeProjectsReport" $
    it "produces valid reports" $ producesValid makeProjectsReport

  pending "produceProjectsreport, which doesn't exist yet, produces valid reports for interesting stores."

-- modifyMaxSuccess (`div` 10) $
--   describe "makeProjectsReport" $
--     it "produces valid reports for interesting stores" $
--       forAllValid $
--         \mFilter ->
--           forAllValid $ \ha ->
--             withInterestingStore $ \dc -> do
--               wr <- produceWaitingReport mFilter ha DontPrint dc
--               shouldBeValid wr
