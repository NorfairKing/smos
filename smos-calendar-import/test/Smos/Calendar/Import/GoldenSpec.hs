{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.GoldenSpec
  ( spec,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Calendar.Import
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.Resolve
import Smos.Data
import System.Exit
import Test.Syd
import Text.ICalendar.Parser
import Text.ICalendar.Types
import Text.Show.Pretty
import YamlParse.Applicative

spec :: Spec
spec = do
  fs <- runIO $ do
    testResourcesDir <- resolveDir' "test_resources"
    filter ((== Just ".ics") . fileExtension) . snd <$> listDirRecur testResourcesDir
  mapM_ mkGoldenTestFor fs

mkGoldenTestFor :: Path Abs File -> Spec
mkGoldenTestFor cp = do
  cal <- runIO $ do
    errOrCal <- parseICalendarFile def $ fromAbsFile cp
    case errOrCal of
      Left err -> die $ unlines ["Failed to parse ical file: " <> fromAbsFile cp, err]
      Right (cals, warns) -> do
        unless (null warns) $ mapM_ print warns
        case cals of
          [] -> die "Expected at least one calendar, got 0"
          [cal] -> pure cal
          _ -> die $ "Expected exactly one calendar, got " <> show (length cals)
  describe (fromAbsFile cp) $ mkGoldenTest cp cal

mkGoldenTest :: Path Abs File -> VCalendar -> Spec
mkGoldenTest cp cals = do
  ProcessConf {..} <- runIO $ do
    confP <- replaceExtension ".config" cp
    mpc <- readConfigFile confP
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
  let actualRecurringEvents = pickEventsFromCalendar cals
  rp <- runIO $ replaceExtension ".recurring" cp
  expectedRecurringEvents <- runIO $ readGoldenYaml rp actualRecurringEvents
  it "picks the correct recurring events" $ compareAndSuggest Yaml.encode rp actualRecurringEvents expectedRecurringEvents
  let actualUnresolvedEvents = recurRecurringEvents (LocalTime processConfLimit midnight) actualRecurringEvents -- TODO use a config file
  up <- runIO $ replaceExtension ".unresolved" cp
  expectedUnresolvedEvents <- runIO $ readGoldenYaml up actualUnresolvedEvents
  it "recurs the correct unresolved events" $ compareAndSuggest Yaml.encode up actualUnresolvedEvents expectedUnresolvedEvents
  let actualEvents = resolveUnresolvedEvents (LocalTime processConfStart midnight) (LocalTime processConfLimit midnight) processConfTimeZone actualUnresolvedEvents
  ep <- runIO $ replaceExtension ".events" cp
  expectedEvents <- runIO $ readGoldenYaml ep actualEvents
  it "resolves the correct events" $ compareAndSuggest Yaml.encode ep actualEvents expectedEvents
  let actualSmosFile = renderAllEvents actualEvents
  sfp <- runIO $ replaceExtension ".smos" cp
  expectedSmosFile <- runIO $ readGoldenSmosFile sfp actualSmosFile
  it "renders the correct smosFile" $ compareAndSuggest smosFileYamlBS sfp actualSmosFile expectedSmosFile

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

readGoldenYaml :: (FromJSON a, ToJSON a, YamlSchema a) => Path Abs File -> a -> IO a
readGoldenYaml p actual = do
  mF <- readConfigFile p
  case mF of
    Nothing ->
      die $
        unlines
          [ unwords ["not found:"],
            fromAbsFile p,
            "suggested:",
            T.unpack (TE.decodeUtf8 (Yaml.encode actual))
          ]
    Just r -> pure r
