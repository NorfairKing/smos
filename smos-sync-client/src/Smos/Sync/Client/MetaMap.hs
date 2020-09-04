{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.MetaMap
  ( MetaMap (..),
    metaMapFiles,
    empty,
    singleton,
    fromListIgnoringCollisions,
  )
where

import Control.DeepSeq
import qualified Data.DirForest as DF
import Data.DirForest (DirForest)
import Data.Foldable
import Data.List (sortOn)
import Data.Map (Map)
import Data.Ord
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Sync.Client.Env

newtype MetaMap
  = MetaMap
      { metaMapDirForest :: DirForest SyncFileMeta -- A dirforest where we don't care about the dirs
      }
  deriving (Show, Eq, Generic)

instance Validity MetaMap where
  validate mm@(MetaMap df) =
    mconcat
      [ genericValidate mm,
        declare "None of the sub-directories are empty" $ DF.null df || not (DF.anyEmptyDir df)
      ]

instance NFData MetaMap

empty :: MetaMap
empty = MetaMap DF.empty

metaMapFiles :: MetaMap -> Map (Path Rel File) SyncFileMeta
metaMapFiles = DF.toFileMap . metaMapDirForest

singleton :: Path Rel File -> SyncFileMeta -> MetaMap
singleton k v = MetaMap $ DF.singletonFile k v

-- |
--
-- This ignores collisions in order
--
-- We can't just use fail here because things will eventually go wrong if two clients add something from different places.
-- So instead we will read the files in reverse order of filenames.
-- This will ensure that if there are these files:
--
-- * foo/bar
-- * foo
--
-- Then foo/bar gets read first and foo ignored.
--
-- I chose this ordering so that if there is more than one file in the 'foo' directory, more files are saved.
fromListIgnoringCollisions :: [(Path Rel File, SyncFileMeta)] -> MetaMap
fromListIgnoringCollisions = MetaMap . foldl' go DF.empty . sortOn (Down . fst)
  where
    go :: DirForest SyncFileMeta -> (Path Rel File, SyncFileMeta) -> DirForest SyncFileMeta
    go df (rf, sfm) = case DF.insertFile rf sfm df of
      Left _ -> df
      Right df' -> df'
