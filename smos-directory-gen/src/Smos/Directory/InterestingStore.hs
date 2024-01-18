{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Directory.InterestingStore where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.DirForest (DirForest (..), DirTree (..))
import qualified Data.DirForest as DF
import Data.Either
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.DirForest
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Data.Gen

data InterestingStore = InterestingStore
  { workflowFiles :: DirForest SmosFile,
    projectFiles :: DirForest SmosFile,
    archiveFiles :: DirForest SmosFile,
    archivedProjectFiles :: DirForest SmosFile,
    otherFiles :: DirForest ByteString
  }
  deriving (Show, Eq, Generic)

instance GenValid InterestingStore where
  genValid = do
    workflowFiles <- genDirForestOf genInterestingSmosFile
    projectFiles <- genDirForestOf genInterestingSmosFile
    archiveFiles <- genDirForestOf genInterestingSmosFile
    archivedProjectFiles <- genDirForestOf genInterestingSmosFile
    otherFiles <- genValid
    pure $ InterestingStore {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance Validity InterestingStore where
  validate is =
    mconcat
      [ genericValidate is,
        declare "The files all fit in one dirforest without insertion errors" $ isRight $ interestingStoreSafeSmosFileDF is
      ]

emptyInterestingStore :: InterestingStore
emptyInterestingStore =
  InterestingStore
    { workflowFiles = DF.empty,
      projectFiles = DF.empty,
      archiveFiles = DF.empty,
      archivedProjectFiles = DF.empty,
      otherFiles = DF.empty
    }

writeInterestingStore :: (MonadIO m) => Path Abs Dir -> InterestingStore -> m ()
writeInterestingStore dir is@InterestingStore {..} = do
  let writeSBF :: Path Abs File -> ByteString -> IO ()
      writeSBF p c = SB.writeFile (fromAbsFile p) c
      writeSF :: Path Abs File -> SmosFile -> IO ()
      writeSF p c = writeSBF (fromMaybe p $ replaceExtension ".smos" p) (smosFileYamlBS c)
      smosFileDf = interestingStoreSmosFileDF is
  liftIO $ DF.write dir smosFileDf writeSF
  liftIO $ DF.write dir otherFiles writeSBF

interestingStoreSmosFileDF :: InterestingStore -> DirForest SmosFile
interestingStoreSmosFileDF = fromRight (error "Cannot happen for valid interesting stores") . interestingStoreSafeSmosFileDF

interestingStoreSafeSmosFileDF :: InterestingStore -> Either (DF.InsertionError SmosFile) (DirForest SmosFile)
interestingStoreSafeSmosFileDF InterestingStore {..} =
  DF.unions
    [ DirForest $ M.singleton "projects" $ NodeDir projectFiles,
      DirForest $ M.singleton "archive" $ NodeDir archiveFiles,
      DirForest $ M.singleton "archive" $ NodeDir $ DirForest $ M.singleton "projects" $ NodeDir archivedProjectFiles,
      workflowFiles
    ]
