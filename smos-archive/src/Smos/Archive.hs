{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Archive
  ( smosArchive
  , module Smos.Archive.Config
  ) where

import Data.Time
import Path
import Path.IO
import System.Exit

import Smos.Archive.Config
import Smos.Archive.OptParse
import Smos.Archive.OptParse.Types

smosArchive :: SmosArchiveConfig -> IO ()
smosArchive = runReaderT $ liftIO getSettings >>= archive

archive :: Settings -> Q ()
archive Settings {..} = do
  let from = setFile
  to <- getToFile setFile
  moveToArchive from to

getToFile :: Path Abs File -> Q (Path Abs File)
getToFile file = do
  workflow <- askWorkDir
  case stripProperPrefix workflow file of
    Nothing ->
      liftIO $
      die $
      unlines
        [ "The smos file"
        , fromAbsFile file
        , "is not in the smos workflow directory"
        , fromAbsDir workflow
        ]
    Just rf -> do
      let arch = workflow </> $(mkRelDir "archive")
          ext = fileExtension rf
      withoutExt <- setFileExtension "" rf
      today <- liftIO $ utctDay <$> getCurrentTime
      let newRelFile =
            fromRelFile withoutExt ++
            "_" ++ formatTime defaultTimeLocale "%F" today
      arf' <- parseRelFile newRelFile
      arf'' <- setFileExtension ext arf'
      pure $ arch </> arf''

moveToArchive :: Path Abs File -> Path Abs File -> Q ()
moveToArchive from to =
  liftIO $ do
    ensureDir $ parent to
    e1 <- doesFileExist from
    if not e1
      then die $
           unwords ["The file to archive does not exist:", fromAbsFile from]
      else do
        e2 <- doesFileExist to
        if e2
          then die $
               unwords
                 ["Proposed archive file", fromAbsFile to, "already exists."]
          else renameFile from to
