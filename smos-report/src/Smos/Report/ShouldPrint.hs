{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ShouldPrint where

import Data.Configurator.Types

import qualified Data.Text as T

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

instance Configured ShouldPrint where
    convert (String t) = parseShouldPrint $ T.unpack t
    convert _ = Nothing

printErrorMessages :: ShouldPrint -> String -> IO ()
printErrorMessages DontPrint = const $ pure ()
printErrorMessages PrintWarning = putStr
printErrorMessages PrintError = error
