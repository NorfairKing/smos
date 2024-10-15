{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.OptParse
  ( Settings (..),
    Source (..),
    Origin (..),
    getSettings,
  )
where

import Autodocodec
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI
import OptEnvConf
import Path
import Path.IO
import Paths_smos_calendar_import (version)
import Smos.CLI.Logging ()
import Smos.CLI.OptParse as CLI
import Smos.Directory.OptParse

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' calendar import tool"

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setLogLevel :: !LogLevel,
    setSources :: ![Source],
    setDebug :: Bool
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = withSmosConfig $ do
  setDirectorySettings <- settingsParser

  let sub = subConfig_ "calendar" . subEnv_ "calendar"
  setLogLevel <- sub settingsParser
  setSources <-
    sub $
      checkMapIO resolveSources $
        setting
          [ help "Calendar sources to import from",
            conf "sources"
          ]
  setDebug <-
    sub $
      setting
        [ switch True,
          long "debug",
          help "Turn on debug output",
          value False
        ]

  pure Settings {..}

data Source = Source
  { sourceName :: Maybe String,
    sourceDestinationFile :: !(Path Rel File),
    sourceOrigin :: !Origin
  }

data Origin
  = WebOrigin URI
  | FileOrigin (Path Abs File)

resolveSources :: [SourceConfiguration] -> IO (Either String [Source])
resolveSources = fmap sequence . mapM resolveSource

resolveSource :: SourceConfiguration -> IO (Either String Source)
resolveSource SourceConfiguration {..} = do
  mOriginURIString <- case sourceConfOrigin of
    Just uri -> pure $ Right uri
    Nothing -> case sourceConfOriginFile of
      Just uriFile -> Right . T.unpack . T.strip . TE.decodeUtf8 <$> SB.readFile uriFile
      Nothing -> pure $ Left "Neither source nor source-file was set."
  case mOriginURIString of
    Left e -> pure $ Left e
    Right originURIString -> do
      let sourceName = sourceConfName
      sourceDestinationFile <- parseRelFile sourceConfDestinationFile
      sourceOrigin <- case parseURI originURIString of
        Just uri -> pure $ WebOrigin uri
        Nothing -> do
          putStrLn $ "Couldn't parse into an URI, assuming it's a file: " <> originURIString
          FileOrigin <$> resolveFile' originURIString
      pure (Right Source {..})

data SourceConfiguration = SourceConfiguration
  { sourceConfName :: !(Maybe String),
    sourceConfOrigin :: !(Maybe String),
    sourceConfOriginFile :: !(Maybe FilePath),
    sourceConfDestinationFile :: !FilePath
  }

instance HasCodec SourceConfiguration where
  codec =
    object "SourceConfiguration" $
      SourceConfiguration
        <$> optionalFieldOrNull "name" "The name of the source" .= sourceConfName
        <*> optionalFieldOrNullWith
          "source"
          ( codec
              <??> [ "If you are using Google, you want to get the URL that has these labels:",
                     "\"Use this address to access this calendar from other applications without making it public.\"",
                     "\"Warning: Only share this address with those you trust to see all event details for this calendar.\"",
                     "For more info, see https://support.google.com/calendar/answer/37648?hl=en#zippy=%2Cget-your-calendar-view-only."
                   ]
          )
          "the url to fetch or file to import"
          .= sourceConfOrigin
        <*> optionalFieldOrNull
          "source-file"
          "the file that contains the url to fetch or file to import"
          .= sourceConfOriginFile
        <*> requiredField "destination" "The destination path within the workflow directory" .= sourceConfDestinationFile
