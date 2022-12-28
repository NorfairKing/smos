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
import Path
import Path.IO
import Smos.Calendar.Import
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.Resolve
import Smos.Calendar.Import.UnresolvedEvent
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
  let parseCalHere p =
        liftIO $ do
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
  pp <- liftIO $ replaceExtension ".parsed" cp
  it "parses the ical correctly" $ do
    goldenTextFile (fromAbsFile pp) $ do
      cal <- parseCalHere cp
      pure $ ICal.renderICalendar [cal]
  rp <- liftIO $ replaceExtension ".recurring" cp
  it "picks the correct recurring events" $
    goldenYamlValueFile (fromAbsFile rp) $ do
      cal <- parseCalHere pp
      pure (pickEvents False [cal] :: Set RecurringEvents)
  up <- liftIO $ replaceExtension ".unresolved" cp
  it "recurs the correct unresolved events" $ do
    goldenYamlValueFile (fromAbsFile up) $ do
      goldenRecurringEvents <- readGoldenYaml rp
      pure (recurEvents processConfLimit (goldenRecurringEvents :: Set RecurringEvents) :: Set UnresolvedEvents)
  ep <- liftIO $ replaceExtension ".events" cp
  it "resolves the correct events" $
    goldenYamlValueFile (fromAbsFile ep) $ do
      goldenUnresolvedEvents <- readGoldenYaml up
      pure (resolveEvents processConfStart processConfLimit processConfTimeZone (goldenUnresolvedEvents :: Set UnresolvedEvents) :: Set Events)
  sfp <- liftIO $ replaceExtension ".smos" cp
  it "renders the correct smosFile" $
    goldenSmosFile (fromAbsFile sfp) $ do
      goldenEvents <- readGoldenYaml ep
      pure $ renderAllEvents (goldenEvents :: Set Events)

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
