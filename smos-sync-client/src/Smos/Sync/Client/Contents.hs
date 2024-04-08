{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Contents where

import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Path
import Smos.Directory.Streaming
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import Smos.Sync.Client.OptParse.Types

readFilteredSyncFiles :: IgnoreFiles -> Path Abs Dir -> IO ContentsMap
readFilteredSyncFiles igf dir =
  case igf of
    IgnoreNothing -> readSyncFiles dir
    IgnoreHiddenFiles -> ContentsMap <$> DF.readNonHidden dir (SB.readFile . fromAbsFile)

readSyncFiles :: Path Abs Dir -> IO ContentsMap
readSyncFiles dir = ContentsMap <$> DF.read dir (SB.readFile . fromAbsFile)

isHidden :: Path Rel File -> Bool
isHidden = go
  where
    go :: Path Rel t -> Bool
    go f =
      if toFilePath f == "./"
        then False
        else
          let p = parent f
           in isHiddenIn p f || go p
