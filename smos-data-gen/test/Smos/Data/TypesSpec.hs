{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Smos.Data.Gen ()
import Smos.Data.Types

spec :: Spec
spec = do
    eqSpec @Contents
    ordSpec @Contents
    genValidSpec @Contents
    jsonSpecOnValid @Contents
    eqSpec @TimestampName
    ordSpec @TimestampName
    genValidSpec @TimestampName
    jsonSpecOnValid @TimestampName
    eqSpec @Timestamp
    ordSpec @Timestamp
    genValidSpec @Timestamp
    jsonSpecOnValid @Timestamp
    eqSpec @TodoState
    ordSpec @TodoState
    genValidSpec @TodoState
    jsonSpecOnValid @TodoState
    eqSpec @StateHistory
    ordSpec @StateHistory
    genValidSpec @StateHistory
    jsonSpecOnValid @StateHistory
    eqSpec @StateHistoryEntry
    ordSpec @StateHistoryEntry
    genValidSpec @StateHistoryEntry
    jsonSpecOnValid @StateHistoryEntry
    eqSpec @Tag
    ordSpec @Tag
    genValidSpec @Tag
    jsonSpecOnValid @Tag
    eqSpec @Logbook
    ordSpec @Logbook
    genValidSpec @Logbook
    jsonSpecOnValid @Logbook
    eqSpec @LogbookEntry
    ordSpec @LogbookEntry
    genValidSpec @LogbookEntry
    jsonSpecOnValid @LogbookEntry
    eqSpec @Entry
    ordSpec @Entry
    genValidSpec @Entry
    jsonSpecOnValid @Entry
    eqSpec @(ForYaml (Tree Entry))
    genValidSpec @(ForYaml (Tree Entry))
    jsonSpecOnValid @(ForYaml (Tree Entry))
    eqSpec @(ForYaml (Forest Entry))
    genValidSpec @(ForYaml (Forest Entry))
    jsonSpecOnValid @(ForYaml (Forest Entry))
    eqSpec @SmosFile
    genValidSpec @SmosFile
    jsonSpecOnValid @SmosFile
