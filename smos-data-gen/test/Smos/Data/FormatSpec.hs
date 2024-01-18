{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.FormatSpec where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as SB
import Data.Maybe
import Path
import Path.IO
import Smos.Data
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  forMatchingFilesIn "test_resources/success/file" $ \tf ->
    it (fromAbsFile tf ++ " succesfully parses as .smos") $
      shouldSucceedInParsingAsSmosFile tf
  successAndFailureTests @SmosFile "file"
  successAndFailureTests @Entry "entry"
  successAndFailureTests @Header "header"
  successAndFailureTests @StateHistory "state-history"
  successAndFailureTests @TodoState "state"
  successAndFailureTests @PropertyName "property-name"
  successAndFailureTests @PropertyValue "property-value"
  successAndFailureTests @Contents "contents"
  successAndFailureTests @Tag "tag"
  successAndFailureTests @Logbook "logbook"
  successAndFailureTests @LogbookEntry "logbook-entry"
  successAndFailureTests @TimestampName "timestamp-name"
  successAndFailureTests @Timestamp "timestamp"
  describe "golden" $ do
    forMatchingFilesIn "test_resources/golden" $ \tf -> do
      ext <- runIO $ fileExtension tf
      it (fromAbsFile tf ++ " still succesfully parses as " ++ ext) $
        shouldSucceedInParsingByExtension @SmosFile tf

successAndFailureTests ::
  forall a.
  (Validity a, Show a, FromJSON a) =>
  FilePath ->
  Spec
successAndFailureTests name =
  describe name $ do
    forMatchingFilesIn ("test_resources/format/" ++ name ++ "/success") $ \tf -> do
      ext <- runIO $ fileExtension tf
      it (fromAbsFile tf ++ " succesfully parses as " ++ ext) $
        shouldSucceedInParsingByExtension @a tf
    forMatchingFilesIn ("test_resources/format/" ++ name ++ "/failure") $ \tf ->
      it (fromAbsFile tf ++ " successfully fails to parse") $ shouldFailToParse @a tf

shouldSucceedInParsingAsSmosFile ::
  Path Abs File ->
  IO ()
shouldSucceedInParsingAsSmosFile tf = do
  mErrOrSf <- readSmosFile tf
  case mErrOrSf of
    Nothing -> expectationFailure "Impossible, hopefully."
    Just (Left err) -> expectationFailure err
    Just (Right sf) -> shouldBeValid sf

shouldSucceedInParsingByExtension ::
  forall a.
  (Validity a, Show a, FromJSON a) =>
  Path Abs File ->
  IO ()
shouldSucceedInParsingByExtension tf = do
  errOrSmosFile <- readFileByExtension @a tf
  case errOrSmosFile of
    Left err -> expectationFailure err
    Right sf -> shouldBeValid sf

shouldFailToParse ::
  forall a.
  (Show a, FromJSON a) =>
  Path Abs File ->
  IO (GoldenTest String)
shouldFailToParse tf = do
  errOrSmosFile <- readFileByExtension @a tf
  errFile <- addExtension ".error" tf
  case errOrSmosFile of
    Left err -> pure $ pureGoldenStringFile (fromAbsFile errFile) err
    Right sf -> expectationFailure $ unwords ["Should have failed, but got this smos file:", ppShow sf]

readFileByExtension ::
  forall a.
  (FromJSON a) =>
  Path Abs File ->
  IO (Either String a)
readFileByExtension tf = do
  let p =
        case fileExtension tf of
          Just ".yaml" -> parseSmosDataYaml @a
          Just ".json" -> parseSmosDataJSON @a
          Just ".pretty-json" -> parseSmosDataJSON @a
          Just ".smos" -> parseSmosData @a
          _ -> parseSmosData @a
  bs <- SB.readFile (fromAbsFile tf)
  pure $ p bs

forMatchingFilesIn :: FilePath -> (Path Abs File -> Spec) -> Spec
forMatchingFilesIn d specFunc = do
  tfs <-
    runIO $ do
      trd <- resolveDir' d
      mtfs <- forgivingAbsence (snd <$> listDirRecur trd)
      let tfs = fromMaybe [] mtfs
      pure $ filter ((`elem` map Just matchingExtensions) . fileExtension) tfs
  forM_ tfs specFunc

matchingExtensions :: [String]
matchingExtensions =
  [ ".yaml",
    ".json",
    ".pretty-json",
    ".smos"
  ]
