{-# LANGUAGE OverloadedStrings #-}

module Smos.CLI.Password
  ( Password,
    mkPassword,
    unsafeShowPassword,
    combinePasswordSettingsWithLogLevel,
    combinePasswordSettings,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Password.Bcrypt
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit

combinePasswordSettingsWithLogLevel ::
  LogLevel ->
  Maybe Password ->
  Maybe FilePath ->
  Maybe Password ->
  Maybe FilePath ->
  Maybe Password ->
  Maybe FilePath ->
  IO (Maybe Password)
combinePasswordSettingsWithLogLevel logLevel flagPassword flagPasswordFile envPassword envPasswordFile confPassword confPasswordFile =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= logLevel) $
      combinePasswordSettings flagPassword flagPasswordFile envPassword envPasswordFile confPassword confPasswordFile

combinePasswordSettings ::
  (MonadIO m, MonadLogger m) =>
  Maybe Password ->
  Maybe FilePath ->
  Maybe Password ->
  Maybe FilePath ->
  Maybe Password ->
  Maybe FilePath ->
  m (Maybe Password)
combinePasswordSettings flagPassword flagPasswordFile envPassword envPasswordFile confPassword confPasswordFile = do
  let readPasswordFrom pf = liftIO $ do
        bytes <- SB.readFile pf
        case TE.decodeUtf8' bytes of
          Left err -> die $ unlines ["Password file was not valid UTF8:", show err]
          Right text -> pure $ mkPassword $ T.strip text
  case flagPassword of
    Just p -> do
      logWarnN "Plaintext password in flags may end up in shell history."
      pure (Just p)
    Nothing -> case flagPasswordFile of
      Just pf -> Just <$> readPasswordFrom pf
      Nothing ->
        case envPassword of
          Just p -> pure (Just p)
          Nothing -> case envPasswordFile of
            Just pf -> Just <$> readPasswordFrom pf
            Nothing ->
              case confPassword of
                Just p -> do
                  logWarnN "Plaintext password in config file."
                  pure (Just p)
                Nothing -> case confPasswordFile of
                  Just pf -> Just <$> readPasswordFrom pf
                  Nothing -> pure Nothing
