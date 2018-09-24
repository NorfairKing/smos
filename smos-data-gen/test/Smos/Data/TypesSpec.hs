{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
    ( spec
    ) where

import Data.Aeson as JSON
import Data.Data

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson
import Test.Validity.Utils

import Smos.Data.Gen ()
import Smos.Data.Types

spec :: Spec
spec = do
    eqSpec @Header
    ordSpec @Header
    genValidSpec @Header
    jsonSpecOnValid @Header
    textLikeJSONValid @Header
    eqSpec @Contents
    ordSpec @Contents
    genValidSpec @Contents
    jsonSpecOnValid @Contents
    textLikeJSONValid @Contents
    eqSpec @TimestampName
    ordSpec @TimestampName
    genValidSpec @TimestampName
    jsonSpecOnValid @TimestampName
    textLikeJSONValid @TimestampName
    eqSpec @Timestamp
    ordSpec @Timestamp
    genValidSpec @Timestamp
    jsonSpecOnValid @Timestamp
    eqSpec @TodoState
    ordSpec @TodoState
    genValidSpec @TodoState
    jsonSpecOnValid @TodoState
    textLikeJSONValid @TodoState
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
    textLikeJSONValid @Tag
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

textLikeJSONValid ::
       forall a. (Validity a, Show a, Typeable a, FromJSON a)
    => Spec
textLikeJSONValid =
    describe (unwords ["JSON", nameOf @a]) $
    it "parses every text value into a valid value" $
    forAllValid $ \j ->
        case fromJSON (JSON.String j) of
            Error _ -> pure ()
            Success h -> shouldBeValid (h :: a)
