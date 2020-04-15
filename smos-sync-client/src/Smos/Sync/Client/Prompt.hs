{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.Prompt
  ( promptUntil,
    promptSecret,
    promptSecretUntil,
    prompt,
  )
where

import Control.Exception
import Control.Monad
import Data.Text
import qualified Data.Text.IO as T
import System.IO

promptUntil :: Text -> (Text -> Maybe a) -> IO a
promptUntil p = promptRawUntil p prompt

promptSecretUntil :: Text -> (Text -> Maybe a) -> IO a
promptSecretUntil p = promptRawUntil p promptSecret

promptRawUntil :: Text -> (Text -> IO Text) -> (Text -> Maybe a) -> IO a
promptRawUntil p pf func = do
  s <- pf p
  case func s of
    Nothing -> promptUntil p func
    Just a -> pure a

prompt :: Text -> IO Text
prompt = promptRaw True . (<> " > ")

promptSecret :: Text -> IO Text
promptSecret = promptRaw False . (<> " > ")

promptRaw :: Bool -> Text -> IO Text
promptRaw b p = do
  T.putStr p
  hFlush stdout
  pass <- withEcho b T.getLine
  unless b $ putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
