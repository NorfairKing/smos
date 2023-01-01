{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.RenderGoldenSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Zones
import Path
import Path.IO
import Smos.Data
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Test.Syd

spec :: Spec
spec = do
  cases <- runIO $ do
    testResourcesDir <- resolveDir' "test_resources"
    files <- snd <$> listDirRecur testResourcesDir
    fmap catMaybes $
      forM files $ \rf ->
        if fileExtension rf == Just ".yaml"
          then do
            mgtc <- readYamlConfigFile rf
            pure $ (,) rf <$> mgtc
          else pure Nothing
  mapM_ (uncurry makeTestUsingCase) cases

data GoldenTestCase = GoldenTestCase
  { goldenTestTemplate :: ScheduleTemplate,
    goldenTestNow :: UTCTime,
    goldenTestResult :: SmosFile
  }

instance HasCodec GoldenTestCase where
  codec =
    object "GoldenTestCase" $
      GoldenTestCase
        <$> requiredField' "template" .= goldenTestTemplate
        <*> requiredFieldWith' "time" timeCodec .= goldenTestNow
        <*> requiredField' "result" .= goldenTestResult
    where
      timeFormat = "%F %T"
      timeCodec = bimapCodec (parseTimeEither defaultTimeLocale timeFormat) (formatTime defaultTimeLocale timeFormat) codec

makeTestUsingCase :: Path Abs File -> GoldenTestCase -> Spec
makeTestUsingCase af GoldenTestCase {..} =
  it ("passes the golden case in " <> fromAbsFile af) $ do
    let ctx = RenderContext {renderContextNow = goldenTestNow, renderContextPretendTime = utcToLocalTime utc goldenTestNow, renderContextTimeZone = utcTZ}
        actual = runReaderT (renderTemplate goldenTestTemplate) ctx
        expected = Success goldenTestResult
    unless (actual == expected) $
      expectationFailure $
        unlines
          [ "Golden test case failure: " <> fromAbsFile af,
            "Expected:",
            T.unpack $ TE.decodeUtf8 $ smosFileYamlBS goldenTestResult,
            "Actual:",
            case actual of
              Success actualSuccess -> T.unpack $ TE.decodeUtf8 $ smosFileYamlBS actualSuccess
              Failure res -> unlines $ map prettyRenderError $ NE.toList res
          ]
