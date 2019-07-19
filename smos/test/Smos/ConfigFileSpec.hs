{-# LANGUAGE OverloadedStrings #-}

module Smos.ConfigFileSpec where

import TestImport

import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Yaml as Yaml

import Smos.Default
import Smos.OptParse.Types
import Smos.Report.OptParse

spec :: Spec
spec = do
  configSpecWithExt ".yaml" parseYamlConfig
  configSpecWithExt ".json" parseJSONConfig

configSpecWithExt :: String -> (Path Abs File -> IO (Either String Configuration)) -> Spec
configSpecWithExt ext parseConf = do
  rd <- runIO $ resourcesDir
  extResourcesDir <- runIO $ resourcesFormatDir rd ext
  extFiles <- runIO $ filesWithExtInDir extResourcesDir ext
  describe ext $ do
    forM_ extFiles $ \df ->
      it (fromAbsFile df) $ do
        errOrConf <- parseConf df
        case errOrConf of
          Left err -> expectationFailure err
          Right conf -> shouldBeValid conf
    describe "default config" $ do
      defaultConfigFile <- runIO $ resolveFile extResourcesDir $ "complete" <> ext
      it ("rebuilds to the the contents of " <> fromAbsFile defaultConfigFile) $ do
        let conf = backToConfiguration defaultConfig
            actual =
              case ext of
                ".yaml" -> Yaml.encode conf
                ".json" -> LB.toStrict $ JSON.encodePretty conf <> "\n"
                _ -> error "unknown format"
        expected <- SB.readFile $ fromAbsFile defaultConfigFile
        unless (actual == expected) $ do
          putStrLn $
            unlines
              [ "Actual:"
              , SB8.unpack actual
              , "differs from expected:"
              , "If this was intentional, please copy the actual to"
              , fromAbsFile defaultConfigFile
              ]
          actual `shouldBe` expected

filesWithExtInDir :: Path Abs Dir -> String -> IO [Path Abs File]
filesWithExtInDir d ext = do
  fs <- forgivingAbsence $ snd <$> listDirRecur d
  pure $ filter ((== ext) . fileExtension) $ fromMaybe [] fs

resourcesFormatDir :: Path Abs Dir -> String -> IO (Path Abs Dir)
resourcesFormatDir rd ext = resolveDir rd $ drop 1 ext

resourcesDir :: IO (Path Abs Dir)
resourcesDir = resolveDir' "test_resources/config/"
