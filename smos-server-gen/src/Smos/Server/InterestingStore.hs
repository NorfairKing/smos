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
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import GHC.Generics (Generic)
import Path
import Servant.Client
import Smos.Client
import Smos.Data
import Smos.Data.Gen ()
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap)

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
  genValid = genValidStructurallyWithoutExtraChecking
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
    sreq = Mergeful.initialSyncRequest {Mergeful.syncRequestNewItems = M.mapWithKey SyncFile $ CM.contentsMapFiles $ interestingStoreToContentsMap is}
