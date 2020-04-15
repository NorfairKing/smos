{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Contents where

import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import qualified Data.Set as S
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Path
import Path.IO
import Smos.API
import Smos.Report.Streaming
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import Smos.Sync.Client.OptParse.Types

readFilteredSyncFiles :: IgnoreFiles -> Path Abs Dir -> IO ContentsMap
readFilteredSyncFiles igf dir = do
  let filePred =
        case igf of
          IgnoreNothing -> const True
          IgnoreHiddenFiles -> not . isHidden
  fs <- fromMaybe [] <$> forgivingAbsence (snd <$> listDirRecurRel dir)
  fmap (ContentsMap . M.fromList . catMaybes)
    $ forM fs
    $ \rp ->
      if filePred rp
        then Just <$> do
          contents <- SB.readFile (fromAbsFile $ dir </> rp)
          pure (rp, contents)
        else pure Nothing -- No need to even read the file, right

readSyncFiles :: Path Abs Dir -> IO ContentsMap
readSyncFiles dir = do
  fs <- snd <$> listDirRecurRel dir
  fmap (ContentsMap . M.fromList)
    $ forM fs
    $ \rp -> do
      contents <- SB.readFile (fromAbsFile $ dir </> rp)
      pure (rp, contents)

filterContentsMap :: IgnoreFiles -> ContentsMap -> ContentsMap
filterContentsMap IgnoreNothing = id
filterContentsMap IgnoreHiddenFiles =
  ContentsMap . M.filterWithKey (\p _ -> not $ isHidden p) . contentsMapFiles

makeContentsMap :: Mergeful.ClientStore FileUUID SyncFile -> ContentsMap
makeContentsMap Mergeful.ClientStore {..} =
  ContentsMap
    $ M.fromList
    $ map (\SyncFile {..} -> (syncFilePath, syncFileContents))
    $ concat
      [ M.elems clientStoreAddedItems,
        M.elems $ M.map Mergeful.timedValue clientStoreSyncedItems,
        M.elems $ M.map Mergeful.timedValue clientStoreSyncedButChangedItems
      ]

saveContentsMap :: IgnoreFiles -> Path Abs Dir -> ContentsMap -> IO ()
saveContentsMap igf dir cm = do
  ensureDir dir
  let filePred =
        case igf of
          IgnoreNothing -> const True
          IgnoreHiddenFiles -> not . isHidden
      filterFunc =
        case igf of
          IgnoreNothing -> id
          IgnoreHiddenFiles -> M.filterWithKey (\p _ -> not $ isHidden p)
  let files = filterFunc $ contentsMapFiles cm
  found <- snd <$> listDirRecurRel dir
  forM_ found $ \p ->
    case M.lookup p files of
      Nothing ->
        if filePred p
          then removeFile (dir </> p) -- Is not supposed to be there anymore
          else pure () -- We should leave it alone
      Just bs -> SB.writeFile (toFilePath $ dir </> p) bs -- TODO this seems a bit wasteful ..
  let leftovers = files `M.difference` M.fromSet (const ()) (S.fromList found)
  ensureDir dir
  void $ M.traverseWithKey go leftovers
  where
    go p bs = do
      let f = dir </> p
      ensureDir $ parent f
      SB.writeFile (fromAbsFile f) bs

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
