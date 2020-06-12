{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.InterestingStore where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Map
import Data.GenValidity.Path ()
import Data.GenValidity.Tree
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import qualified Data.Set as S
import Data.Set (Set)
import GHC.Generics (Generic)
import Path
import Servant.Client
import Smos.Client
import Smos.Data
import Smos.Data.Gen ()
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap)
import Test.QuickCheck

data InterestingStore
  = InterestingStore
      { archiveFiles :: Map (Path Rel File) SmosFile,
        workflowFiles :: Map (Path Rel File) SmosFile,
        projectFiles :: Map (Path Rel File) SmosFile,
        archivedProjectFiles :: Map (Path Rel File) SmosFile,
        otherFiles :: Map (Path Rel File) ByteString
      }
  deriving (Show, Eq, Generic)

instance GenValid InterestingStore where
  genValid = do
    archiveFiles <- genStructurallyValidMapOf ((,) <$> genValid <*> genInterestingSmosFile)
    workflowFiles <- genStructurallyValidMapOf ((,) <$> genValid <*> genInterestingSmosFile)
    projectFiles <- genStructurallyValidMapOf ((,) <$> genValid <*> genInterestingSmosFile)
    archivedProjectFiles <- genStructurallyValidMapOf ((,) <$> genValid <*> genInterestingSmosFile)
    otherFiles <- genValid
    pure $ InterestingStore {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance Validity InterestingStore

interestingStoreToContentsMap :: InterestingStore -> ContentsMap
interestingStoreToContentsMap InterestingStore {..} =
  foldl'
    goM
    CM.empty
    [ M.map smosFileYamlBS workflowFiles,
      M.map smosFileYamlBS (M.mapKeys ([reldir|projects|] </>) projectFiles),
      M.map smosFileYamlBS (M.mapKeys ([reldir|archive|] </>) archiveFiles),
      M.map smosFileYamlBS (M.mapKeys ([reldir|archive/projects|] </>) archivedProjectFiles),
      otherFiles
    ]
  where
    goM :: ContentsMap -> Map (Path Rel File) ByteString -> ContentsMap
    goM cm m = foldl' go cm (M.toList m)
    go :: ContentsMap -> (Path Rel File, ByteString) -> ContentsMap
    go cm (p, bs) = fromMaybe cm $ CM.insert p bs cm

setupInterestingStore :: Token -> InterestingStore -> ClientM ()
setupInterestingStore t is = void $ clientPostSync t sreq
  where
    sreq =
      Mergeful.initialSyncRequest
        { Mergeful.syncRequestNewItems = M.mapWithKey SyncFile $ CM.contentsMapFiles $ interestingStoreToContentsMap is
        }

genInterestingSmosFile :: Gen SmosFile
genInterestingSmosFile = SmosFile <$> genInterestingForest

genInterestingForest :: Gen (Forest Entry)
genInterestingForest = genListOf genInterestingTree

genInterestingTree :: Gen (Tree Entry)
genInterestingTree = genTreeOf genInterestingEntry

genInterestingEntry :: Gen Entry
genInterestingEntry =
  sized $ \size -> do
    (a, b, c, d, e, f, g) <- genSplit7 size
    entryHeader <- resize a genValid
    entryContents <- resize b genValid
    entryTimestamps <- resize c genInterestingTimestamps
    entryProperties <- resize d genValid
    entryStateHistory <- resize e genInterestingStateHistory
    entryTags <- resize f genInterestingTags
    entryLogbook <- resize g genValid
    pure Entry {..}

genInterestingTimestamps :: Gen (Map TimestampName Timestamp)
genInterestingTimestamps = genStructurallyValidMapOf $ (,) <$> genInterestingTimestampName <*> genValid

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
genInterestingStateHistory = StateHistory . sort <$> genListOf genInterestingStateHistoryEntry

genInterestingStateHistoryEntry :: Gen StateHistoryEntry
genInterestingStateHistoryEntry =
  StateHistoryEntry
    <$> frequency [(1, pure Nothing), (9, Just <$> genInterestingTodoState)]
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
genInterestingTags = S.fromList <$> genListOf genInterestingTag

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
