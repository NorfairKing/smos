{-# LANGUAGE LambdaCase #-}

module Smos.Directory.ShouldPrint where

import Control.Monad.IO.Class
import System.IO

data ShouldPrint
  = PrintError
  | PrintWarning Handle
  | DontPrint
  deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint = \case
  "error" -> Just PrintError
  "warning" -> Just (PrintWarning stderr)
  "nothing" -> Just DontPrint
  _ -> Nothing

printErrorMessage :: (MonadIO m) => ShouldPrint -> String -> m ()
printErrorMessage = \case
  DontPrint -> const $ pure ()
  PrintWarning h -> liftIO . hPutStrLn h
  PrintError -> error
