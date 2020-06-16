{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.InterestingStore where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.DirForest as DF
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import Path
import Servant.Client
import Smos.Client
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.InterestingStore
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap)

interestingStoreToContentsMap :: InterestingStore -> ContentsMap
interestingStoreToContentsMap is@InterestingStore {..} =
  let smosDF = interestingStoreSmosFileDF is
      addSmosExt p = fromMaybe p $ addFileExtension ".smos" p
   in foldl'
        goM
        CM.empty
        [ M.mapKeys addSmosExt $ DF.toFileMap $ fmap smosFileYamlBS smosDF,
          DF.toFileMap otherFiles
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
      SyncRequest
        { syncRequestItems =
            Mergeful.initialSyncRequest
              { Mergeful.syncRequestNewItems = M.map SyncFile $ CM.contentsMapFiles $ interestingStoreToContentsMap is
              }
        }
