{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Contents where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.UUID
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()

import Lens.Micro

import qualified System.FilePath as FP

import Control.Monad

import Path
import Path.IO

import Smos.Report.Streaming

import Smos.Sync.API

import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.ContentsMap as CM

import Smos.Sync.Client.OptParse.Types

readFilteredSyncFiles :: IgnoreFiles -> Path Abs Dir -> IO ContentsMap
readFilteredSyncFiles igf dir = filterContentsMap igf <$> readSyncFiles dir

readSyncFiles :: Path Abs Dir -> IO ContentsMap
readSyncFiles dir = do
  fs <- snd <$> listDirRecurRel dir
  fmap (ContentsMap . M.fromList) $
    forM fs $ \rp -> do
      contents <- SB.readFile (fromAbsFile $ dir </> rp)
      pure (rp, contents)

filterContentsMap :: IgnoreFiles -> ContentsMap -> ContentsMap
filterContentsMap IgnoreNothing = id
filterContentsMap IgnoreHiddenFiles =
  ContentsMap . M.filterWithKey (\p _ -> not $ isHidden p) . contentsMapFiles

makeContentsMap :: Mergeful.ClientStore UUID SyncFile -> ContentsMap
makeContentsMap Mergeful.ClientStore {..} =
  ContentsMap $
  M.fromList $
  map (\SyncFile {..} -> (syncFilePath, syncFileContents)) $
  concat
    [ M.elems clientStoreAddedItems
    , M.elems $ M.map Mergeful.timedValue clientStoreSyncedItems
    , M.elems $ M.map Mergeful.timedValue clientStoreSyncedButChangedItems
    ]

saveContentsMap :: IgnoreFiles -> Path Abs Dir -> ContentsMap -> IO ()
saveContentsMap _ dir cm = do
  tmpDir1 <- resolveDir' $ FP.dropTrailingPathSeparator (toFilePath dir) ++ "-tmp1"
  tmpDir2 <- resolveDir' $ FP.dropTrailingPathSeparator (toFilePath dir) ++ "-tmp2"
  writeAllTo tmpDir1
  renameDir dir tmpDir2
  renameDir tmpDir1 dir
  removeDirRecur tmpDir2
  where
    writeAllTo d = do
      ensureDir d
      void $ M.traverseWithKey go $ contentsMapFiles cm
      where
        go p bs = do
          let f = d </> p
          ensureDir $ parent f
          SB.writeFile (fromAbsFile f) bs

isHidden :: Path Rel File -> Bool
isHidden = go
  where
    go :: Path Rel t -> Bool
    go f =
      if toFilePath f == "./"
        then False
        else let p = parent f
              in isHiddenIn p f || go p
