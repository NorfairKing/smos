{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Test.Hspec
import Test.Validity
import Text.Show.Pretty

spec :: Spec
spec = do
  forFilesIn "test_resources/success/file" $ \tf ->
    it (fromAbsFile tf ++ " succesfully parses as .smos") $
    shouldSucceedInParsingAsSmosFile @SmosFile tf
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

successAndFailureTests ::
     forall a. (Validity a, Show a, FromJSON a)
  => FilePath
  -> Spec
successAndFailureTests name =
  describe name $ do
    forFilesIn ("test_resources/" ++ name ++ "/success") $ \tf -> do
      let ext = fileExtension tf
      it (fromAbsFile tf ++ " succesfully parses as " ++ ext) $
        shouldSucceedInParsingByExtension @a tf
    forFilesIn ("test_resources/" ++ name ++ "/failure") $ \tf ->
      it (fromAbsFile tf ++ " successfully fails to parse") $ shouldFailToParse @a tf

shouldSucceedInParsingAsSmosFile ::
     forall a. (Validity a, Show a, FromJSON a)
  => Path Abs File
  -> IO ()
shouldSucceedInParsingAsSmosFile tf = do
  mErrOrSf <- readSmosFile tf
  case mErrOrSf of
    Nothing -> expectationFailure "Impossible, hopefully."
    Just (Left err) -> expectationFailure err
    Just (Right sf) -> shouldBeValid sf

shouldSucceedInParsingByExtension ::
     forall a. (Validity a, Show a, FromJSON a)
  => Path Abs File
  -> IO ()
shouldSucceedInParsingByExtension tf = do
  errOrSmosFile <- readFileByExtension @a tf
  case errOrSmosFile of
    Left err -> expectationFailure err
    Right sf -> shouldBeValid sf

shouldFailToParse ::
     forall a. (Show a, FromJSON a)
  => Path Abs File
  -> IO ()
shouldFailToParse tf = do
  errOrSmosFile <- readFileByExtension @a tf
  case errOrSmosFile of
    Left _ -> pure ()
    Right sf ->
      expectationFailure $ unwords ["Should have failed, but got this smos file:", ppShow sf]

readFileByExtension ::
     forall a. (Show a, FromJSON a)
  => Path Abs File
  -> IO (Either String a)
readFileByExtension tf = do
  let ext = fileExtension tf
  let p =
        case ext of
          ".yaml" -> parseSmosDataYaml @a
          ".json" -> parseSmosDataJSON @a
          ".smos" -> parseSmosData @a
          _ -> parseSmosData @a
  bs <- SB.readFile (fromAbsFile tf)
  pure $ p bs

forFilesIn :: FilePath -> (Path Abs File -> Spec) -> Spec
forFilesIn d specFunc = do
  tfs <-
    runIO $ do
      trd <- resolveDir' d
      mtfs <- forgivingAbsence (snd <$> listDirRecur trd)
      pure $ fromMaybe [] mtfs
  forM_ tfs specFunc
