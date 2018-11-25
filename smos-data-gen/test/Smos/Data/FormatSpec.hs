module Smos.Data.FormatSpec where

import qualified Data.ByteString as SB
import Data.Maybe

import Control.Monad

import Path
import Path.IO

import Test.Hspec
import Test.Validity

import Smos.Data

spec :: Spec
spec = do
    tfs <-
        runIO $ do
            trd <- resolveDir' "test_resources"
            mtfs <- forgivingAbsence (snd <$> listDirRecur trd)
            pure $ fromMaybe [] mtfs
    forM_ tfs $ \tf -> do
        let ext = fileExtension tf
        it (fromAbsFile tf ++ " as .smos") $ do
            mErrOrSf <- readSmosFile tf
            case mErrOrSf of
                Nothing -> expectationFailure "Impossible, hopefully."
                Just (Left err) -> expectationFailure err
                Just (Right sf) -> shouldBeValid sf
        it (fromAbsFile tf ++ " as " ++ ext) $ do
            let p =
                    case ext of
                        ".yaml" -> parseSmosFileYaml
                        ".json" -> parseSmosFileJSON
                        _ -> parseSmosFile
            bs <- SB.readFile (fromAbsFile tf)
            case p bs of
                Left err -> expectationFailure err
                Right sf -> shouldBeValid sf
