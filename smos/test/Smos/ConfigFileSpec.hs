{-# LANGUAGE OverloadedStrings #-}

module Smos.ConfigFileSpec where

import TestImport

import Data.Aeson as JSON
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
  rd <- runIO resourcesDir
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
        let actual = backToConfiguration defaultConfig
        let encodeFunc =
              case ext of
                ".yaml" -> Yaml.encode
                ".json" -> LB.toStrict . JSON.encodePretty
                _ -> error "unknown format"
        expected <-
          do contents <- SB.readFile $ fromAbsFile defaultConfigFile
             let decodeFunc =
                   case ext of
                     ".yaml" -> Yaml.decodeThrow
                     ".json" -> JSON.decode . LB.fromStrict
                     _ -> error "unknown format"
             case decodeFunc contents of
               Nothing -> expectationFailure "Failed to decode expected result." >> undefined
               Just r -> pure r
        unless (actual == expected) $ do
          putStrLn $
            unlines
              [ "Actual:"
              , ppShow actual
              , "differs from expected:"
              , ppShow expected
              , "If this was intentional, please copy the following actual to"
              , fromAbsFile defaultConfigFile
              , SB8.unpack $ encodeFunc actual
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
