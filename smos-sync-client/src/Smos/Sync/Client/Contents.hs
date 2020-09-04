{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Contents where

import Control.Exception
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
import GHC.IO.Exception
import Path
import Path.IO
import Smos.API
import Smos.Report.Streaming
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.OptParse.Types
import qualified System.FilePath as FP

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
  catchJust
    (\(e :: IOException) -> if ioe_type e == InappropriateType then Just e else Nothing)
    (forgivingAbsence (SB.readFile $ fromAbsFile af))
    (const $ pure Nothing)

-- |
--
-- The first argument is the top-level directory
-- The file that will be written will be the combination of the first two arguments.
-- Every file on the path up can be deleted to make space for the one being written, but no further than the first argument
writeFileSafely :: MonadIO m => Path Abs Dir -> Path Rel File -> ByteString -> m ()
writeFileSafely top rf bs = liftIO $ do
  ensureDir top
  let af = top </> rf
  let afp = fromAbsFile af
      writeUncarefully = do
        putStrLn "Writing uncarefully"
        print ("creating dir", parent af)
        ensureDir (parent af)
        print ("writing file", afp)
        SB.writeFile afp bs
      writeOverDir = forM_ (parseAbsDir afp :: Maybe (Path Abs Dir)) $ \ad -> do
        putStrLn "Writing over dir"
        print ("writing over dir", ad)
        removeDirRecur ad
        writeUncarefully
      removeFilesUpwards = do
        putStrLn "Removing files upward"
        print ("starting at", parent rf)
        go (parent rf)
        writeUncarefully
        where
          go rd = case parseRelFile (FP.dropTrailingPathSeparator $ fromRelDir rd) of
            Nothing -> pure ()
            Just rf' -> do
              print ("Checking if this file exists", top </> rf')
              fileExists <- doesFileExist $ top </> rf'
              if fileExists
                then do
                  print ("Removing file", top </> rf')
                  removeFile $ top </> rf'
                else
                  let p = parent rd
                   in if p == [reldir|./|]
                        then pure ()
                        else do
                          print ("Recursing up", p)
                          go p
  catchJust
    (\(e :: IOException) -> if ioe_type e == AlreadyExists then Just e else Nothing)
    ( catchJust
        (\(e :: IOException) -> if ioe_type e == InappropriateType then Just e else Nothing)
        writeUncarefully
        (const writeOverDir)
    )
    (const removeFilesUpwards)

-- ensureDir (parent af)
-- case parseAbsDir (fromAbsFile af) of
--   Nothing -> pure ()
--   Just ad -> do
--     dirExists <- doesDirExist ad
--     when dirExists $ removeDirRecur ad
-- SB.writeFile (fromAbsFile af) bs

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
