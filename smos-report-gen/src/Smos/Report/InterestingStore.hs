{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.InterestingStore where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Data.DirForest (DirForest (..), DirTree (..))
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.DirForest
import Data.GenValidity.Path ()
import Data.GenValidity.Tree
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Data.Gen ()
import Test.QuickCheck

data InterestingStore
  = InterestingStore
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

instance Validity InterestingStore

writeInterestingStore :: MonadIO m => Path Abs Dir -> InterestingStore -> m ()
writeInterestingStore dir is@InterestingStore {..} = do
  let writeSBF :: Path Abs File -> ByteString -> IO ()
      writeSBF p c = SB.writeFile (fromAbsFile p) c
      writeSF :: Path Abs File -> SmosFile -> IO ()
      writeSF p c = writeSBF (fromMaybe p $ replaceExtension ".smos" p) (smosFileYamlBS c)
      smosFileDf = interestingStoreSmosFileDF is
  liftIO $ DF.write dir smosFileDf writeSF
  liftIO $ DF.write dir otherFiles writeSBF

interestingStoreSmosFileDF :: InterestingStore -> DirForest SmosFile
interestingStoreSmosFileDF InterestingStore {..} =
  DF.unions
    [ DirForest $ M.singleton "projects" $ NodeDir projectFiles,
      DirForest $ M.singleton "archive" $ NodeDir archiveFiles,
      DirForest $ M.singleton "archive" $ NodeDir $ DirForest $ M.singleton "projects" $ NodeDir archivedProjectFiles,
      workflowFiles
    ]

genInterestingSmosFile :: Gen SmosFile
genInterestingSmosFile = SmosFile <$> genInterestingForest

genInterestingForest :: Gen (Forest Entry)
genInterestingForest =
  frequency
    [ (1, pure []),
      (9, NE.toList <$> genNonEmptyOf genInterestingTree)
    ]

genInterestingTree :: Gen (Tree Entry)
genInterestingTree = genTreeOf genInterestingEntry

genInterestingEntry :: Gen Entry
genInterestingEntry =
  sized $ \size -> do
    (a, b, c, d, e, f, g) <- genSplit7 size
    entryHeader <- resize a genValid
    entryContents <- resize b (genInterestingMaybeOf genValid)
    entryTimestamps <- resize c genInterestingTimestamps
    entryProperties <- resize d (genInterestingMapOf genValid)
    entryStateHistory <- resize e genInterestingStateHistory
    entryTags <- resize f genInterestingTags
    entryLogbook <- resize g genValid
    pure Entry {..}

genInterestingTimestamps :: Gen (Map TimestampName Timestamp)
genInterestingTimestamps = genInterestingMapOf $ (,) <$> genInterestingTimestampName <*> genValid

genInterestingTimestampName :: Gen TimestampName
genInterestingTimestampName =
  oneof
    [ pure "BEGIN",
      pure "END",
      pure "SCHEDULED",
      pure "DEADLINE",
      genValid
    ]

genInterestingStateHistory :: Gen StateHistory
genInterestingStateHistory = StateHistory . sort <$> genInterestingListOf genInterestingStateHistoryEntry

genInterestingStateHistoryEntry :: Gen StateHistoryEntry
genInterestingStateHistoryEntry =
  StateHistoryEntry
    <$> genInterestingMaybeOf genInterestingTodoState
    <*> genValid

genInterestingTodoState :: Gen TodoState
genInterestingTodoState =
  oneof
    [ pure "TODO",
      pure "NEXT",
      pure "STARTED",
      pure "CANCELLED",
      pure "DONE",
      pure "FAILED",
      pure "WAITING",
      genValid
    ]

genInterestingTags :: Gen (Set Tag)
genInterestingTags = genInterestingSetOf genInterestingTag

genInterestingTag :: Gen Tag
genInterestingTag =
  oneof
    [ pure "code",
      pure "external",
      pure "home",
      pure "offline",
      pure "online",
      pure "power",
      pure "toast",
      pure "work",
      genValid
    ]

genInterestingSetOf :: Ord v => Gen v -> Gen (Set v)
genInterestingSetOf g = S.fromList <$> genInterestingListOf g

genInterestingMapOf :: Ord k => Gen (k, v) -> Gen (Map k v)
genInterestingMapOf g = M.fromList <$> genInterestingListOf g

genInterestingListOf :: Gen v -> Gen [v]
genInterestingListOf g =
  frequency
    [ (1, pure []),
      (9, NE.toList <$> genNonEmptyOf g)
    ]

genInterestingMaybeOf :: Gen v -> Gen (Maybe v)
genInterestingMaybeOf g =
  frequency
    [ (1, pure Nothing),
      (9, Just <$> g)
    ]
