{-# LANGUAGE LambdaCase #-}

module Smos.Data.TestUtils where

import Control.Monad
import qualified Data.Text.Encoding as TE
import Path.IO
import Smos.Data
import Test.Syd

pureGoldenSmosFile :: FilePath -> SmosFile -> GoldenTest SmosFile
pureGoldenSmosFile path = goldenSmosFile path . pure

goldenSmosFile :: FilePath -> IO SmosFile -> GoldenTest SmosFile
goldenSmosFile path produceSmosFile =
  GoldenTest
    { goldenTestRead = do
        absFile <- resolveFile' path
        mErrOrRes <- readSmosFile absFile
        forM mErrOrRes $ \case
          Left err -> expectationFailure err
          Right smosFile -> pure smosFile,
      goldenTestProduce = produceSmosFile,
      goldenTestWrite = \smosFile -> do
        absFile <- resolveFile' path
        writeSmosFile absFile smosFile,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then pure Nothing
          else do
            let actualBS = smosFileBS actual
                expectedBS = smosFileBS expected
            assertion <- case (,) <$> TE.decodeUtf8' actualBS <*> TE.decodeUtf8' expectedBS of
              Left _ -> bytestringsNotEqualButShouldHaveBeenEqual actualBS expectedBS
              Right (actualText, expectedText) -> textsNotEqualButShouldHaveBeenEqual actualText expectedText
            pure $
              Just $
                Context
                  assertion
                  (goldenContext path)
    }
