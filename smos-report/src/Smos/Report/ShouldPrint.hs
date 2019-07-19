{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ShouldPrint where

import Control.Monad.IO.Class

data ShouldPrint
  = PrintError
  | PrintWarning
  | DontPrint
  deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint "error" = Just PrintError
parseShouldPrint "warning" = Just PrintWarning
parseShouldPrint "nothing" = Just DontPrint
parseShouldPrint _ = Nothing

printErrorMessage :: MonadIO m => ShouldPrint -> String -> m ()
printErrorMessage DontPrint = const $ pure ()
printErrorMessage PrintWarning = liftIO . putStrLn
printErrorMessage PrintError = error
