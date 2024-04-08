{-# LANGUAGE LambdaCase #-}

module Smos.Directory.ShouldPrint where

import Control.Monad.IO.Class
import System.IO

data ShouldPrint
  = PrintError
  | PrintWarning Handle
  | DontPrint

printErrorMessage :: (MonadIO m) => ShouldPrint -> String -> m ()
printErrorMessage = \case
  DontPrint -> const $ pure ()
  PrintWarning h -> liftIO . hPutStrLn h
  PrintError -> error
