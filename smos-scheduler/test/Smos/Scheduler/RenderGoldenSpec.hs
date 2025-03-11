{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.RenderGoldenSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad
import Control.Monad.Reader
import Data.Time
import Data.Time.Zones
import Path
import Smos.Data
import Smos.Scheduler.History
import Smos.Scheduler.Render
import Smos.Scheduler.Schedule
import Test.Syd

spec :: Spec
spec = do
  scenarioDir "test_resources" $ \fp -> do
    rf <- liftIO $ parseRelFile fp
    when (fileExtension rf == Just ".yaml") $ do
      it ("passes the golden case in " <> fromRelFile rf) $ goldenByteStringFile (fp <> ".result") $ do
        mgtc <- liftIO $ readYamlConfigFile rf
        GoldenTestCase {..} <- case mgtc of
          Nothing -> expectationFailure "should exist."
          Just gtc -> pure gtc

        let pretendNow = utcToLocalTime utc goldenTestNow
            ctx =
              RenderContext
                { renderContextNow = goldenTestNow,
                  renderContextPretendTime = pretendNow,
                  renderContextTimeZone = utcTZ
                }
            actualValidation = addScheduleMetadata pretendNow "test" <$> runReaderT (renderTemplate goldenTestTemplate) ctx
        case actualValidation of
          Success actual -> pure $ smosFileYamlBS actual
          Failure res -> expectationFailure $ show res

data GoldenTestCase = GoldenTestCase
  { goldenTestNow :: UTCTime,
    goldenTestTemplate :: ScheduleTemplate
  }

instance HasCodec GoldenTestCase where
  codec =
    object "GoldenTestCase" $
      GoldenTestCase
        <$> requiredFieldWith' "time" timeCodec .= goldenTestNow
        <*> requiredField' "template" .= goldenTestTemplate
    where
      timeFormat = "%F %T%Q"
      timeCodec = bimapCodec (parseTimeEither defaultTimeLocale timeFormat) (formatTime defaultTimeLocale timeFormat) codec
