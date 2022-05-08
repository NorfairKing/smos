{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.ProjectsSpec where

import Data.List
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
      forAllValid $ \h ->
        forAllValid $ \time -> do
          let startedEntry = entryWithState h time "STARTED"
          getCurrentEntry (makeSmosFile [Node startedEntry []]) `shouldBe` Just startedEntry

    it "finds the first of two NEXT entries" $
      forAllValid $ \header1 ->
        forAllValid $ \header2 ->
          forAllValid $ \time1 ->
            forAllValid $ \time2 -> do
              let startedEntry = entryWithState header1 time1 "STARTED"
                  nextEntry = entryWithState header2 time2 "NEXT"
              getCurrentEntry
                ( makeSmosFile
                    [ Node startedEntry [],
                      Node nextEntry []
                    ]
                )
                `shouldBe` Just startedEntry

    it "ignores a TODO entry if there is a NEXT entry" $
      forAllValid $ \header1 ->
        forAllValid $ \header2 ->
          forAllValid $ \time1 ->
            forAllValid $ \time2 -> do
              let todoEntry = entryWithState header1 time1 "TODO"
                  nextEntry = entryWithState header2 time2 "NEXT"
              getCurrentEntry
                ( makeSmosFile $
                    sort
                      [ Node todoEntry [],
                        Node nextEntry []
                      ]
                )
                `shouldBe` Just nextEntry

    it "finds a NEXT action even under an entry without a state" $
      forAllValid $ \h ->
        forAllValid $ \time -> do
          let nextEntry = entryWithState h time "NEXT"
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
