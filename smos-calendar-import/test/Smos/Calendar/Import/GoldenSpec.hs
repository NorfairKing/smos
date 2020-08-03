module Smos.Calendar.Import.GoldenSpec
  ( spec,
  )
where

import Control.Monad
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Render
import Smos.Data
import System.Exit
import Test.Hspec
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
  cals <- runIO $ do
    errOrCal <- parseICalendarFile def $ fromAbsFile cp
    case errOrCal of
      Left err -> die $ unlines ["Failed to parse ical file: " <> fromAbsFile cp, err]
      Right (cals, warns) -> do
        unless (null warns) $ mapM_ print warns
        pure cals
  describe (fromAbsFile cp) $ mkGoldenTest cp cals

mkGoldenTest :: Path Abs File -> [VCalendar] -> Spec
mkGoldenTest cp cals = do
  let actualRecurringEvents = pickEvents cals
  recurringEvents <- runIO $ readGoldenYaml cp ".recurring" actualRecurringEvents
  it "picks the correct recurring events" $
    actualRecurringEvents `shouldBe` recurringEvents
  let actualEvents = recurEvents actualRecurringEvents
  events <- runIO $ readGoldenYaml cp ".events" actualEvents
  it "recurs the correct events" $
    actualEvents `shouldBe` events
  let actualSmosFile = renderEvents actualEvents
  smosFile <- runIO $ readGoldenSmosFile cp actualSmosFile
  it "renders the correct smosFile" $
    actualSmosFile `shouldBe` smosFile

readGoldenSmosFile :: Path Abs File -> SmosFile -> IO SmosFile
readGoldenSmosFile cp actual = do
  sfp <- replaceExtension ".smos" cp
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
      Left err -> die $ "Failed to parse smos file: " <> err
      Right smosFile -> pure smosFile

readGoldenYaml :: (FromJSON a, ToJSON a, YamlSchema a) => Path Abs File -> String -> a -> IO a
readGoldenYaml cp ext actual = do
  p <- replaceExtension ext cp
  mF <- readConfigFile p
  case mF of
    Nothing ->
      die $
        unlines
          [ unwords [ext, "not found:"],
            fromAbsFile p,
            "suggested:",
            T.unpack (TE.decodeUtf8 (Yaml.encode actual))
          ]
    Just r -> pure r
