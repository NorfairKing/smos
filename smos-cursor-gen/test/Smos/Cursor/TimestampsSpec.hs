{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TimestampsSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Timestamps
import Smos.Cursor.Timestamps.Gen ()

spec :: Spec
spec = do
    eqSpec @TimestampsCursor
    genValidSpec @TimestampsCursor
    describe "makeTimestampsCursor" $
        it "produces valid cursors" $
        producesValidsOnValids makeTimestampsCursor
    describe "rebuildTimestampsCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildTimestampsCursor
        it "is the inverse of makeTimestampsCursor" $
            inverseFunctionsOnValid makeTimestampsCursor rebuildTimestampsCursor
