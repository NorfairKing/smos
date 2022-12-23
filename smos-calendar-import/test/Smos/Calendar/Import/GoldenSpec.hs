{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.GoldenSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
import qualified ICal
import qualified ICal.Conformance as ICal
import Path
import Path.IO
import Smos.Calendar.Import
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.Resolve
import Smos.Data
import Smos.Data.TestUtils
import System.Exit
import Test.Syd

spec :: Spec
spec = do
  fs <- liftIO $ do
    testResourcesDir <- resolveDir' "test_resources"
    filter ((== Just ".ics") . fileExtension) . snd <$> listDirRecur testResourcesDir
  mapM_ mkGoldenTest fs

mkGoldenTest :: Path Abs File -> Spec
mkGoldenTest cp = doNotRandomiseExecutionOrder . describe (fromAbsFile cp) $ do
  ProcessConf {..} <- liftIO $ do
    confP <- replaceExtension ".config" cp
    mpc <- readYamlConfigFile confP
    case mpc of
      Nothing ->
        die $
          unlines
            [ "No process conf for golden test:",
              fromAbsFile cp,
              "Expected one at:",
              fromAbsFile confP
            ]
      Just pc -> pure pc
  rp <- liftIO $ replaceExtension ".recurring" cp
  it "picks the correct recurring events" $
    goldenYamlValueFile (fromAbsFile rp) $ do
      cal <- liftIO $ do
        contents <- SB.readFile (fromAbsFile cp)
        let errOrCal = ICal.runConform $ ICal.parseICalendarByteString contents
        case errOrCal of
          Left err -> die $ unlines ["Failed to parse ical file: " <> fromAbsFile cp, displayException err]
          Right (cals, warns) -> do
            unless (null warns) $ mapM_ (putStrLn . displayException) warns
            case cals of
              [] -> die "Expected at least one calendar, got 0"
              [cal] -> pure cal
              _ -> die $ "Expected exactly one calendar, got " <> show (length cals)
      pure $ pickEventsFromCalendar False cal
  up <- liftIO $ replaceExtension ".unresolved" cp
  it "recurs the correct unresolved events" $ do
    goldenYamlValueFile (fromAbsFile up) $ do
      goldenRecurringEvents <- readGoldenYaml rp
      pure $ recurRecurringEvents (LocalTime processConfLimit midnight) goldenRecurringEvents
  ep <- liftIO $ replaceExtension ".events" cp
  it "resolves the correct events" $
    goldenYamlValueFile (fromAbsFile ep) $ do
      goldenUnresolvedEvents <- readGoldenYaml up
      pure $ resolveUnresolvedEvents (LocalTime processConfStart midnight) (LocalTime processConfLimit midnight) processConfTimeZone goldenUnresolvedEvents
  sfp <- liftIO $ replaceExtension ".smos" cp
  it "renders the correct smosFile" $
    goldenSmosFile (fromAbsFile sfp) $ do
      goldenEvents <- readGoldenYaml ep
      pure $ renderAllEvents goldenEvents

compareAndSuggest :: (Show a, Eq a) => (a -> ByteString) -> Path Abs File -> a -> a -> IO ()
compareAndSuggest func p actual expected = do
  let write = False -- TODO expose this somehow?
  unless (actual == expected) $ do
    if write
      then SB.writeFile (fromAbsFile p) (func actual)
      else do
        putStr $
          unlines
            [ fromAbsFile p,
              "input:",
              "actual structure:",
              ppShow actual,
              "actual serialised:",
              T.unpack (TE.decodeUtf8 (func actual)),
              "expected structure:",
              ppShow expected,
              "expected serialised:",
              T.unpack (TE.decodeUtf8 (func expected))
            ]
        actual `shouldBe` expected

readGoldenSmosFile :: Path Abs File -> SmosFile -> IO SmosFile
readGoldenSmosFile sfp actual = do
  mErrOrSmosFile <- readSmosFile sfp
  case mErrOrSmosFile of
    Nothing ->
      die $
        unlines
          [ unwords
              [ "Golden smos file result not found: ",
                fromAbsFile sfp
              ],
            "suggested:",
            T.unpack (TE.decodeUtf8 (smosFileYamlBS actual))
          ]
    Just errOrSmosFile -> case errOrSmosFile of
      Left err -> die $ unlines ["Failed to parse smos file: ", fromAbsFile sfp, err]
      Right smosFile -> pure smosFile

readGoldenYaml :: (HasCodec a) => Path Abs File -> IO a
readGoldenYaml p = do
  mF <- readYamlConfigFile p
  case mF of
    Nothing ->
      die $
        unwords
          [ "not found:",
            fromAbsFile p
          ]
    Just r -> pure r

goldenYamlValueFile :: (Show a, Eq a, FromJSON a, ToJSON a) => FilePath -> IO a -> GoldenTest a
goldenYamlValueFile fp produceActualValue =
  GoldenTest
    { goldenTestRead = do
        p <- resolveFile' fp
        mContents <- forgivingAbsence $ SB.readFile (fromAbsFile p)
        forM mContents $ \contents ->
          case Yaml.decodeEither contents of
            Left err -> expectationFailure err
            Right r -> pure r,
      goldenTestProduce = produceActualValue,
      goldenTestWrite = \v -> do
        value <- evaluate $ force $ toJSON v
        p <- resolveFile' fp
        ensureDir (parent p)
        SB.writeFile (fromAbsFile p) $ Yaml.encode value,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else Just (Context (stringsNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)) (goldenContext fp))
    }

pureGoldenYamlValueFile :: (Show a, Eq a, FromJSON a, ToJSON a) => FilePath -> a -> GoldenTest a
pureGoldenYamlValueFile fp actualValue = goldenYamlValueFile fp $ pure actualValue
