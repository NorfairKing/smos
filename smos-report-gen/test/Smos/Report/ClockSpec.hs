{-# LANGUAGE TypeApplications #-}

module Smos.Report.ClockSpec where

import Data.Time

import Test.Hspec
import Test.Validity

import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Clock

import Smos.Report.Clock.Gen ()

spec :: Spec
spec = do
    describe "findClockTimes" $
        it "produces valid clock times" $ producesValidsOnValids2 findClockTimes
    describe "trimClockTime" $
        it "produces valid clock times" $ producesValidsOnValids3 trimClockTime
    describe "trimLogbookEntry" $
        it "produces valid logbook entries" $
        producesValidsOnValids3 trimLogbookEntry
    describe "trimLogbookEntryTo" $
        it "produces valid logbook entries" $
        forAllValid $ \tz -> producesValidsOnValids3 $ trimLogbookEntryTo tz
    describe "divideIntoBlocks" $
        it "produces valid clock time blocks" $
        producesValidsOnValids3 divideIntoBlocks
    describe "combineBlocksByName" $
        it "produces valid clock time blocks" $
        producesValidsOnValids (combineBlocksByName @Day)
    describe "divideClockTimeIntoDailyBlocks" $
        it "produces valid clock time blocks" $
        producesValidsOnValids2 divideClockTimeIntoDailyBlocks
    describe "makeClockTable" $
        it "produces valid clock tables" $ producesValidsOnValids makeClockTable
    describe "makeClockTableBlock" $
        it "produces valid clock table blocks" $
        producesValidsOnValids makeClockTableBlock
    describe "makeClockTableEntry" $
        it "produces valid clock table entries" $
        producesValidsOnValids makeClockTableEntry
    describe "sumLogbookEntryTime" $
        it "produces valid difftimes" $
        producesValidsOnValids sumLogbookEntryTime
    describe "renderClockTable" $
        it "produces valid Text" $ producesValidsOnValids2 renderClockTable
    describe "renderNominalDiffTime" $
        it "produces valid Text" $ producesValidsOnValids2 renderNominalDiffTime
