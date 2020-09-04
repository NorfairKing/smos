{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Contents where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.DirForest as DF
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Path
import Path.IO
import Smos.API
import Smos.Report.Streaming
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.OptParse.Types

readFilteredSyncFiles :: IgnoreFiles -> Path Abs Dir -> IO ContentsMap
readFilteredSyncFiles igf dir =
  case igf of
    IgnoreNothing -> readSyncFiles dir
    IgnoreHiddenFiles -> ContentsMap <$> DF.readNonHidden dir (SB.readFile . fromAbsFile)

readSyncFiles :: Path Abs Dir -> IO ContentsMap
readSyncFiles dir = ContentsMap <$> DF.read dir (SB.readFile . fromAbsFile)

-- |
--
-- Nothing means the path doesn't exist as a file.
-- This could mean that the file doesn't exist, but it doesn't have to mean that there is nothing at that path.
-- There could be a directory, in which case you also get 'Nothing'
readFileSafely :: MonadIO m => Path Abs File -> m (Maybe ByteString)
readFileSafely af = liftIO $ do
  let readIt = forgivingAbsence $ SB.readFile $ fromAbsFile af
  case parseAbsDir (fromAbsFile af) of
    Nothing -> readIt
    Just ad -> do
      dirExists <- doesDirExist ad
      if dirExists
        then pure Nothing
        else readIt

writeFileSafely :: MonadIO m => Path Abs File -> ByteString -> m ()
writeFileSafely af bs = liftIO $ do
  ensureDir (parent af)
  case parseAbsDir (fromAbsFile af) of
    Nothing -> pure ()
    Just ad -> do
      dirExists <- doesDirExist ad
      when dirExists $ removeDirRecur ad
  SB.writeFile (fromAbsFile af) bs

filterContentsMap :: IgnoreFiles -> ContentsMap -> ContentsMap
filterContentsMap IgnoreNothing = id
filterContentsMap IgnoreHiddenFiles = CM.filterHidden

makeContentsMap :: Mergeful.ClientStore (Path Rel File) (Path Rel File) SyncFile -> ContentsMap
makeContentsMap Mergeful.ClientStore {..} =
  CM.fromListIgnoringCollisions $ M.toList $ M.map syncFileContents $
    M.unions
      [ clientStoreAddedItems,
        M.map Mergeful.timedValue clientStoreSyncedItems,
        M.map Mergeful.timedValue clientStoreSyncedButChangedItems
      ]

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
