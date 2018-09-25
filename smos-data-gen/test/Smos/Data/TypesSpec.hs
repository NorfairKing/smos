{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
    ( spec
    ) where

import Data.Aeson as JSON
import Data.Data
import Data.Time

import Control.Monad

import Test.Hspec
import Test.QuickCheck
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
    let genLogbookEntryJSON =
            object <$>
            sequence
                [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
                , ("end" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
                ]
    eqSpec @Logbook
    ordSpec @Logbook
    genValidSpec @Logbook
    jsonSpecOnValid @Logbook
    genJSONValid @Logbook $
        let withGen lbGen =
                sized $ \n -> do
                    l <- choose (1, n)
                    rest <- replicateM l lbGen
                    first <-
                        object <$>
                        sequence
                            [ ("start" .=) <$>
                              (toJSON <$> (genValid :: Gen UTCTime))
                            , ("end" .=) <$>
                              (toJSON <$> (genValid :: Gen (Maybe UTCTime)))
                            ]
                    pure $ toJSON $ first : rest
            genOrderedLogbookEntryJSON = do
                start <- genValid :: Gen UTCTime
                end <- genValid `suchThat` (>= start) :: Gen UTCTime
                pure $ object ["start" .= start, "end" .= end]
        in oneof
               [withGen genLogbookEntryJSON, withGen genOrderedLogbookEntryJSON]
    eqSpec @LogbookEntry
    ordSpec @LogbookEntry
    genValidSpec @LogbookEntry
    jsonSpecOnValid @LogbookEntry
    genJSONValid @LogbookEntry genLogbookEntryJSON
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
textLikeJSONValid = genJSONValid @a $ JSON.String <$> genValid

genJSONValid ::
       forall a. (Validity a, Show a, Typeable a, FromJSON a)
    => Gen JSON.Value
    -> Spec
genJSONValid gen =
    describe (unwords ["JSON", nameOf @a]) $
    it "parses every value into a valid value" $
    forAll gen $ \j ->
        case fromJSON j of
            JSON.Error _ -> pure ()
            JSON.Success h -> shouldBeValid (h :: a)
