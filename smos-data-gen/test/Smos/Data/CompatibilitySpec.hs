module Smos.Data.CompatibilitySpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.SemVer as Version
import qualified Data.Set as S
import Path.IO
import Smos.Data
import System.FilePath
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let supportedVersions =
        S.fromList
          [ version 0 0 0 [] [],
            oldestParsableDataVersion,
            currentDataVersion,
            version 1 0 0 [] [],
            newestParsableDataVersion
          ]
  describe "oldestParsableDataVersion" $ it "is older than the current version" $ oldestParsableDataVersion <= currentDataVersion
  describe "newestParsableDataVersion" $ it "is newer than the current version" $ currentDataVersion <= newestParsableDataVersion
  forM_ supportedVersions versionParsesSuccesfullySpec
  versionFailsToParseNicelySpec (version 2 0 0 [] [])
  it "outputs the same error every time" $ do
    p <- resolveFile' "test_resources/version-too-new.smos"
    r <- readSmosFile p
    case r of
      Nothing -> expectationFailure "File should have existed."
      Just errOrSmosFile -> case errOrSmosFile of
        Left err -> pure $ pureGoldenStringFile "test_resources/version-too-new.error" err
        Right res ->
          expectationFailure $
            unlines
              [ "Should have failed to parse, but parsed this instead:",
                ppShow res
              ]

dirForVer :: Version -> FilePath
dirForVer v = "test_resources/compatibility/v" <> Version.toString v

parseFunction :: FilePath -> ByteString -> Either String SmosFile
parseFunction fp =
  let ext = takeExtension fp
   in case ext of
        ".smos" -> parseSmosFile
        ".yaml" -> parseSmosFileYaml
        ".json" -> parseSmosFileJSON
        ".pretty-json" -> parseSmosFileJSON
        e -> pure $ Left $ "unknown file format: " <> e

versionParsesSuccesfullySpec :: Version -> Spec
versionParsesSuccesfullySpec v =
  scenarioDir (dirForVer v) $ \fp ->
    it "parses succesfully" $ do
      bs <- SB.readFile fp
      case parseFunction fp bs of
        Left err -> expectationFailure err
        Right r -> shouldBeValid r

versionFailsToParseNicelySpec :: Version -> Spec
versionFailsToParseNicelySpec v =
  scenarioDir (dirForVer v) $ \fp ->
    it "fails to parse, nicely" $ do
      bs <- SB.readFile fp
      case parseFunction fp bs of
        Left _ -> pure ()
        Right r -> shouldBeValid r
