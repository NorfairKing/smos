module Smos.Data.FormatSpec where

import qualified Data.ByteString as SB
import Data.Maybe
import Text.Show.Pretty

import Control.Monad

import Path
import Path.IO

import Test.Hspec
import Test.Validity

import Smos.Data

spec :: Spec
spec = do
    forFilesIn "test_resources/success" $ \tf -> do
        let ext = fileExtension tf
        it (fromAbsFile tf ++ " succesfully parses as .smos") $ do
            mErrOrSf <- readSmosFile tf
            case mErrOrSf of
                Nothing -> expectationFailure "Impossible, hopefully."
                Just (Left err) -> expectationFailure err
                Just (Right sf) -> shouldBeValid sf
        it (fromAbsFile tf ++ " succesfully parses as " ++ ext) $ do
            errOrSmosFile <- readFileByExtension tf
            case errOrSmosFile of
                Left err -> expectationFailure err
                Right sf -> shouldBeValid sf
    forFilesIn "test_resources/failure" $ \tf -> do
        it (fromAbsFile tf ++ " successfully fails to parse") $ do
            errOrSmosFile <- readFileByExtension tf
            case errOrSmosFile of
                Left _ -> pure ()
                Right sf ->
                    expectationFailure $
                    unwords
                        [ "Should have failed, but got this smos file:"
                        , ppShow sf
                        ]

readFileByExtension :: Path Abs File -> IO (Either String SmosFile)
readFileByExtension tf = do
    let ext = fileExtension tf
    let p =
            case ext of
                ".yaml" -> parseSmosFileYaml
                ".json" -> parseSmosFileJSON
                ".smos" -> parseSmosFile
                _ -> parseSmosFile
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
