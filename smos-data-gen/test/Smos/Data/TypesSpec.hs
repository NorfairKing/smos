{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
  ( spec,
  )
where

import Control.Monad
import Data.Aeson as JSON
import Data.Data
import Data.Time
import Smos.Data.Gen ()
import Smos.Data.Types
import Test.Syd
import Test.QuickCheck
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  genValidSpec @Header
  jsonSpecOnValid @Header
  textLikeJSONValid @Header
  genValidSpec @Contents
  jsonSpecOnValid @Contents
  textLikeJSONValid @Contents
  genValidSpec @PropertyName
  jsonSpecOnValid @PropertyName
  textLikeJSONValid @PropertyName
  -- If you remove this, the tests for json for sorters, filters and projections will fail in smos-report-gen
  describe "validPropertyNameChar" $ do
    it "says that ')' is invalid" $
      validPropertyNameChar ')' `shouldBe` False
    it
      "says that '(' is invalid"
      $ validPropertyNameChar '(' `shouldBe` False
  genValidSpec @PropertyValue
  jsonSpecOnValid @PropertyValue
  textLikeJSONValid @PropertyValue
  genValidSpec @TimestampName
  jsonSpecOnValid @TimestampName
  textLikeJSONValid @TimestampName
  genValidSpec @Timestamp
  jsonSpecOnValid @Timestamp
  textLikeJSONValid @Timestamp
  describe "parseTimestampString" $
    it "parses whatever timestampString outputs" $
      forAllValid $
        \ts -> parseTimestampString (timestampString ts) `shouldBe` Just ts
  describe "parseTimestampText" $
    it "parses whatever timestampText outputs" $
      forAllValid $
        \ts -> parseTimestampText (timestampText ts) `shouldBe` Just ts
  genValidSpec @TodoState
  jsonSpecOnValid @TodoState
  textLikeJSONValid @TodoState
  ordSpecOnValid @StateHistory
  genValidSpec @StateHistory
  jsonSpecOnValid @StateHistory
  ordSpecOnValid @StateHistoryEntry
  genValidSpec @StateHistoryEntry
  jsonSpecOnValid @StateHistoryEntry
  genValidSpec @Tag
  jsonSpecOnValid @Tag
  textLikeJSONValid @Tag
  -- If you remove this, the tests for json for sorters, filters and projections will fail in smos-report-gen
  describe "validTagChar" $ do
    it "says that ')' is invalid" $
      validTagChar ')' `shouldBe` False
    it
      "says that '(' is invalid"
      $ validTagChar '(' `shouldBe` False
  let genLogbookEntryJSON =
        object
          <$> sequence
            [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime)),
              ("end" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
            ]
  genValidSpec @Logbook
  jsonSpecOnValid @Logbook
  describe "emptyLogbook" $ it "is valid" $ shouldBeValid emptyLogbook
  genJSONValid @Logbook $
    let withGen lbGen =
          sized $ \n -> do
            l <- choose (1, n)
            rest <- replicateM l lbGen
            first <-
              object
                <$> sequence
                  [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime)),
                    ("end" .=) <$> (toJSON <$> (genValid :: Gen (Maybe UTCTime)))
                  ]
            pure $ toJSON $ first : rest
        genOrderedLogbookEntryJSON = do
          start <- genValid :: Gen UTCTime
          end <- genValid `suchThat` (>= start) :: Gen UTCTime
          pure $ object ["start" .= start, "end" .= end]
     in oneof [withGen genLogbookEntryJSON, withGen genOrderedLogbookEntryJSON]
  genValidSpec @LogbookEntry
  jsonSpecOnValid @LogbookEntry
  genJSONValid @LogbookEntry genLogbookEntryJSON
  eqSpecOnValid @Entry
  ordSpecOnValid @Entry
  genValidSpec @Entry
  jsonSpecOnValid @Entry
  genValidSpec @(ForYaml (Tree Entry))
  jsonSpecOnValid @(ForYaml (Tree Entry))
  genValidSpec @(ForYaml (Forest Entry))
  jsonSpecOnValid @(ForYaml (Forest Entry))
  genValidSpec @SmosFile
  ordSpecOnValid @SmosFile
  jsonSpecOnValid @SmosFile

textLikeJSONValid ::
  forall a.
  (Validity a, Show a, Typeable a, FromJSON a) =>
  Spec
textLikeJSONValid = genJSONValid @a $ JSON.String <$> genValid

genJSONValid ::
  forall a.
  (Validity a, Show a, Typeable a, FromJSON a) =>
  Gen JSON.Value ->
  Spec
genJSONValid gen =
  describe (unwords ["JSON", nameOf @a]) $
    it "parses every value into a valid value" $
      forAll gen $
        \j ->
          case fromJSON j of
            JSON.Error _ -> pure ()
            JSON.Success h -> shouldBeValid (h :: a)
