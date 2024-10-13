module Smos.Archive.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Smos.Archive.OptParse

type A a = ReaderT Settings (LoggingT IO) a

runA :: Settings -> A a -> LoggingT IO a
runA = flip runReaderT
