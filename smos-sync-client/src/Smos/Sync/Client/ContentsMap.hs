{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.ContentsMap where

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

import Smos.Sync.Client.OptParse.Types

newtype ContentsMap =
  ContentsMap
    { contentsMapFiles :: Map (Path Rel File) ByteString
    }
  deriving (Show, Eq, Generic)

instance Validity ContentsMap

contentsMapL :: Lens' ContentsMap (Map (Path Rel File) ByteString)
contentsMapL = lens contentsMapFiles $ \cm m -> cm {contentsMapFiles = m}

empty :: ContentsMap
empty = ContentsMap M.empty

singleton :: Path Rel File -> ByteString -> ContentsMap
singleton k v = ContentsMap $ M.singleton k v

insert :: Path Rel File -> ByteString -> ContentsMap -> Maybe ContentsMap
insert k v (ContentsMap m) = constructValid $ ContentsMap $ M.insert k v m

union :: ContentsMap -> ContentsMap -> Maybe ContentsMap
union (ContentsMap m1) (ContentsMap m2) = constructValid $ ContentsMap $ M.union m1 m2
