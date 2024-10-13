{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Smos.CLI.OptParse
  ( withSmosConfig,
    defaultConfigFiles,
  )
where

import Autodocodec
import qualified OptEnvConf
import Path
import Path.IO
import Servant.Client
-- For the HasParser LogLevel instance
import Smos.CLI.Logging ()

withSmosConfig :: OptEnvConf.Parser a -> OptEnvConf.Parser a
withSmosConfig =
  OptEnvConf.subEnv_ "smos"
    . OptEnvConf.withFirstYamlConfig (OptEnvConf.runIO defaultConfigFiles)

-- | Smos Config files
--
-- Tried in order:
--
-- * ~/.config/smos/config.yaml
-- * ~/.smos/config.yaml
-- * ~/.smos.yaml
defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles = do
  home <- getHomeDir
  homeConfigDir <- resolveDir home ".smos"
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|smos|])
  configInConfigDir <- resolveFile xdgConfigDir "config.yaml"
  configInHomeSmosDir <- resolveFile homeConfigDir "config.yaml"
  configInHomeDir <- resolveFile home "smos.yaml"
  pure [configInConfigDir, configInHomeSmosDir, configInHomeDir]

instance HasCodec BaseUrl where
  codec =
    bimapCodec
      ( \s -> case parseBaseUrl s of
          Left err -> Left (show err)
          Right burl -> Right burl
      )
      showBaseUrl
      codec
