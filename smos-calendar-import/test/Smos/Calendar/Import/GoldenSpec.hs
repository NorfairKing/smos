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
import qualified Data.ByteString as SB
import Data.Set (Set)
import qualified Data.Yaml.Builder as Yaml
import qualified ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Conformance.TestUtils as ICal
import qualified ICal.Recurrence as ICal
import Path
import Path.IO
import Smos.Calendar.Import
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.ResolveLocal
import Smos.Calendar.Import.ResolveZones
import Smos.Calendar.Import.UTCEvent
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Data
import Smos.Data.TestUtils
import System.Exit
import Test.Syd

spec :: Spec
spec = do
  fs <- liftIO $ do
    testResourcesDir <- resolveDir' "test_resources"
    filter ((== Just ".ics") . fileExtension) . snd <$> listDirRecur testResourcesDir
  doNotRandomiseExecutionOrder $ mapM_ mkGoldenTest fs

mkGoldenTest :: Path Abs File -> Spec
mkGoldenTest cp = sequential . describe (fromAbsFile cp) $ do
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
  let parseCalHere p = do
        cts <- SB.readFile (fromAbsFile p)
        let errOrCal = ICal.runConform $ ICal.parseICalendarByteString cts
        case errOrCal of
          Left err -> die $ unlines ["Failed to parse ical file: " <> fromAbsFile cp, displayException err]
          Right (cals, warns) -> do
            unless (null warns) $ mapM_ (putStrLn . displayException) warns
            case cals of
              [] -> die "Expected at least one calendar, got 0"
              [cal] -> pure cal
              _ -> die $ "Expected exactly one calendar, got " <> show (length cals)

  cal <- liftIO $ parseCalHere cp

  pp <- liftIO $ replaceExtension ".parsed" cp
  it "parses the ical calendar correctly" $ do
    pureGoldenTextFile (fromAbsFile pp) $ ICal.renderVCalendar cal

  let timezones = pickTimeZones cal
  zp <- liftIO $ replaceExtension ".zones" cp
  it "parses the ical timezones correctly" $ do
    pureGoldenYamlValueFile (fromAbsFile zp) timezones

  let recurringEvents :: RecurringEvents
      recurringEvents = pickEvents False cal
  rp <- liftIO $ replaceExtension ".recurring" cp
  it "picks the correct recurring events" $
    pureGoldenYamlValueFile (fromAbsFile rp) recurringEvents

  (unresolvedEvents, utcEvents) <-
    liftIO $
      ICal.shouldConformLenient $
        ICal.runR processConfLimit timezones $ do
          unresolvedEvents <- recurRecurringEvents processConfLimit (recurringEvents :: RecurringEvents)
          utcEvents <- resolveUnresolvedEvents unresolvedEvents
          pure (unresolvedEvents, utcEvents)
  up <- liftIO $ replaceExtension ".unresolved" cp
  it "recurs the correct unresolved events" $
    pureGoldenYamlValueFile (fromAbsFile up) (unresolvedEvents :: UnresolvedEvents)

  uep <- liftIO $ replaceExtension ".utcevents" cp
  it "resolves the correct utc events" $
    pureGoldenYamlValueFile (fromAbsFile uep) (utcEvents :: Set UTCEvents)

  ep <- liftIO $ replaceExtension ".events" cp
  let events :: Set Events
      events = resolveUTCEventsInUTC utcEvents
  it "resolves the correct events" $
    pureGoldenYamlValueFile (fromAbsFile ep) events

  let smosFile :: SmosFile
      smosFile = renderAllEvents events
  sfp <- liftIO $ replaceExtension ".smos" cp
  it "renders the correct smosFile" $
    pureGoldenSmosFile (fromAbsFile sfp) smosFile

readGoldenYaml :: HasCodec a => Path Abs File -> IO a
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

pureGoldenYamlValueFile :: (Show a, Eq a, HasCodec a) => FilePath -> a -> GoldenTest a
pureGoldenYamlValueFile fp value = goldenYamlValueFile fp (pure value)

goldenYamlValueFile :: (Show a, Eq a, HasCodec a) => FilePath -> IO a -> GoldenTest a
goldenYamlValueFile fp produceActualValue =
  GoldenTest
    { goldenTestRead = do
        p <- resolveFile' fp
        mContents <- forgivingAbsence $ SB.readFile (fromAbsFile p)
        pure $ case mContents of
          Nothing -> Nothing
          Just cts ->
            case eitherDecodeYamlViaCodec cts of
              Left _ -> Nothing -- If decoding fails, reset the golden output.
              Right r -> Just r,
      goldenTestProduce = produceActualValue,
      goldenTestWrite = \v -> do
        contents <- evaluate $ force $ Yaml.toByteString $ toYamlViaCodec v
        p <- resolveFile' fp
        ensureDir (parent p)
        SB.writeFile (fromAbsFile p) contents,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else Just (Context (stringsNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)) (goldenContext fp))
    }
