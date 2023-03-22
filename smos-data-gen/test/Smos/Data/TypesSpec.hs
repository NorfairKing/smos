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
import Data.Either
import Data.Time
import Smos.Data
import Smos.Data.Gen
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  genValidSpec @Header
  jsonSpec @Header
  textLikeJSONValid @Header
  genValidSpec @Contents
  jsonSpec @Contents
  textLikeJSONValid @Contents
  genValidSpec @PropertyName
  jsonSpec @PropertyName
  textLikeJSONValid @PropertyName
  -- If you remove this, the tests for json for sorters, filters and projections will fail in smos-report-gen
  describe "validPropertyNameChar" $ do
    it "says that ')' is invalid" $
      validPropertyNameChar ')' `shouldBe` False
    it
      "says that '(' is invalid"
      $ validPropertyNameChar '(' `shouldBe` False
  genValidSpec @PropertyValue
  jsonSpec @PropertyValue
  textLikeJSONValid @PropertyValue
  genValidSpec @TimestampName
  jsonSpec @TimestampName
  textLikeJSONValid @TimestampName
  genValidSpec @Timestamp
  jsonSpec @Timestamp
  textLikeJSONValid @Timestamp
  describe "parseDayString" $
    it "parses whatever renderDayString outputs" $
      forAllValid $
        \d -> parseDayString (renderDayString d) `shouldBe` Right d
  describe "parseLocalTimeString" $ do
    it "parses whatever renderLocalTimeString outputs" $
      forAll genImpreciseLocalTime $ \lt ->
        parseLocalTimeString (renderLocalTimeString lt) `shouldBe` Right lt
    it "parses this version 0 timestamp" $
      parseLocalTimeString "2018-09-06T06:13:41.359194141Z" `shouldSatisfy` isRight
    it "parses this version 0 timestamp" $
      parseLocalTimeString "2018-09-06T06:13:41.359194141Z" `shouldSatisfy` isRight
    it "parses this version 1.0.0 timestamp" $
      parseLocalTimeString "2022-01-22 00:20:34.000000000000" `shouldSatisfy` isRight
    it "parses this version 2.0.0 timestamp" $
      parseLocalTimeString "2022-01-22 00:20:34" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.0" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.01" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.012" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.0123" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.01234" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.012345" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.0123456" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.01234567" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.012345678" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseLocalTimeString "2022-01-22 00:20:34.0123456789" `shouldSatisfy` isRight
  describe "parseUTCTimeString" $ do
    it "parses whatever renderUTCTimeString outputs" $
      forAllValid $
        \u -> parseUTCTimeString (renderUTCTimeString u) `shouldBe` Right u
    it "parses this version 0.0.0 timestamp" $
      parseUTCTimeString "2018-09-06T06:13:41.359194141Z" `shouldSatisfy` isRight
    it "parses this version 1.0.0 timestamp" $
      parseUTCTimeString "2022-01-22 00:20:34.000000000000" `shouldSatisfy` isRight
    it "parses this version 2.0.0 timestamp" $
      parseUTCTimeString "2022-01-22 00:20:34" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.0" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.01" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.012" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.0123" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.01234" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.012345" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.0123456" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.01234567" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.012345678" `shouldSatisfy` isRight
    it "parses this string leniently" $
      parseUTCTimeString "2022-01-22 00:20:34.0123456789" `shouldSatisfy` isRight
  describe "parseTimestampString" $
    it "parses whatever timestampString outputs" $
      forAllValid $
        \ts -> parseTimestampString (timestampString ts) `shouldBe` Right ts
  describe "parseTimestampText" $
    it "parses whatever timestampText outputs" $
      forAllValid $
        \ts -> parseTimestampText (timestampText ts) `shouldBe` Right ts
  genValidSpec @TodoState
  jsonSpec @TodoState
  textLikeJSONValid @TodoState
  ordSpec @StateHistory
  genValidSpec @StateHistory
  describe "StateHistory" $
    it "considers a state history where two entries happened at the same time valid" $
      forAllValid $ \mts1 ->
        forAllValid $ \mts2 ->
          forAllValid $ \u ->
            shouldBeValid $
              StateHistory
                [ mkStateHistoryEntry u mts1,
                  mkStateHistoryEntry u mts2
                ]
  jsonSpec @StateHistory
  ordSpec @StateHistoryEntry
  genValidSpec @StateHistoryEntry
  jsonSpec @StateHistoryEntry
  genValidSpec @Tag
  jsonSpec @Tag
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
  jsonSpec @Logbook
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
  jsonSpec @LogbookEntry
  genJSONValid @LogbookEntry genLogbookEntryJSON
  eqSpec @Entry
  ordSpec @Entry
  genValidSpec @Entry
  jsonSpec @Entry
  describe "emptyEntry" $ it "is valid" $ shouldBeValid emptyEntry
  describe "newEntry" $ it "produces valid entries" $ producesValid newEntry
  describe "entryWithState" $ do
    it "produces valid entries" $ producesValid3 entryWithState
    it "produces an entry with the given state" $
      forAllValid $ \h ->
        forAllValid $ \time ->
          forAllValid $ \state ->
            entryState (entryWithState h time state) `shouldBe` Just state
  genValidSpec @SmosFile
  ordSpec @SmosFile
  jsonSpec @SmosFile

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
