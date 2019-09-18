{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
  ( spec
  ) where

import Control.Monad
import Data.Aeson as JSON
import Data.Data
import Data.Time
import Smos.Data.Gen ()
import Smos.Data.Types
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson
import Test.Validity.Utils

spec :: Spec
spec = do
  eqSpecOnValid @Header
  ordSpecOnValid @Header
  genValidSpec @Header
  jsonSpecOnValid @Header
  textLikeJSONValid @Header
  eqSpecOnValid @Contents
  ordSpecOnValid @Contents
  genValidSpec @Contents
  jsonSpecOnValid @Contents
  textLikeJSONValid @Contents
  eqSpecOnValid @PropertyName
  ordSpecOnValid @PropertyName
  genValidSpec @PropertyName
  jsonSpecOnValid @PropertyName
  textLikeJSONValid @PropertyName
  eqSpecOnValid @PropertyValue
  ordSpecOnValid @PropertyValue
  genValidSpec @PropertyValue
  jsonSpecOnValid @PropertyValue
  textLikeJSONValid @PropertyValue
  eqSpecOnValid @TimestampName
  ordSpecOnValid @TimestampName
  genValidSpec @TimestampName
  jsonSpecOnValid @TimestampName
  textLikeJSONValid @TimestampName
  eqSpecOnValid @Timestamp
  ordSpecOnValid @Timestamp
  genValidSpec @Timestamp
  jsonSpecOnValid @Timestamp
  textLikeJSONValid @Timestamp
  describe "parseTimestampString" $
    it "parses whatever timestampString outputs" $
    forAllValid $ \ts -> parseTimestampString (timestampString ts) `shouldBe` Just ts
  describe "parseTimestampText" $
    it "parses whatever timestampText outputs" $
    forAllValid $ \ts -> parseTimestampText (timestampText ts) `shouldBe` Just ts
  eqSpecOnValid @TodoState
  ordSpecOnValid @TodoState
  genValidSpec @TodoState
  jsonSpecOnValid @TodoState
  textLikeJSONValid @TodoState
  eqSpecOnValid @StateHistory
  ordSpecOnValid @StateHistory
  genValidSpec @StateHistory
  jsonSpecOnValid @StateHistory
  eqSpecOnValid @StateHistoryEntry
  ordSpecOnValid @StateHistoryEntry
  genValidSpec @StateHistoryEntry
  jsonSpecOnValid @StateHistoryEntry
  eqSpecOnValid @Tag
  ordSpecOnValid @Tag
  genValidSpec @Tag
  jsonSpecOnValid @Tag
  textLikeJSONValid @Tag
  let genLogbookEntryJSON =
        object <$>
        sequence
          [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
          , ("end" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
          ]
  eqSpecOnValid @Logbook
  ordSpecOnValid @Logbook
  genValidSpec @Logbook
  jsonSpecOnValid @Logbook
  describe "emptyLogbook" $ it "is valid" $ shouldBeValid emptyLogbook
  genJSONValid @Logbook $
    let withGen lbGen =
          sized $ \n -> do
            l <- choose (1, n)
            rest <- replicateM l lbGen
            first <-
              object <$>
              sequence
                [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
                , ("end" .=) <$> (toJSON <$> (genValid :: Gen (Maybe UTCTime)))
                ]
            pure $ toJSON $ first : rest
        genOrderedLogbookEntryJSON = do
          start <- genValid :: Gen UTCTime
          end <- genValid `suchThat` (>= start) :: Gen UTCTime
          pure $ object ["start" .= start, "end" .= end]
     in oneof [withGen genLogbookEntryJSON, withGen genOrderedLogbookEntryJSON]
  eqSpecOnValid @LogbookEntry
  ordSpecOnValid @LogbookEntry
  genValidSpec @LogbookEntry
  jsonSpecOnValid @LogbookEntry
  genJSONValid @LogbookEntry genLogbookEntryJSON
  eqSpecOnValid @Entry
  ordSpecOnValid @Entry
  genValidSpec @Entry
  jsonSpecOnValid @Entry
  eqSpecOnValid @(ForYaml (Tree Entry))
  genValidSpec @(ForYaml (Tree Entry))
  jsonSpecOnValid @(ForYaml (Tree Entry))
  eqSpecOnValid @(ForYaml (Forest Entry))
  genValidSpec @(ForYaml (Forest Entry))
  jsonSpecOnValid @(ForYaml (Forest Entry))
  eqSpecOnValid @SmosFile
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
