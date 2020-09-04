{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.ContentsMap
  ( ContentsMap (..),
    contentsMapFiles,
    empty,
    insert,
    singleton,
    fromListIgnoringCollisions,
    union,
    unions,
    filterHidden,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.Foldable
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path

newtype ContentsMap
  = ContentsMap
      { contentsMapDirForest :: DirForest ByteString -- A dirforest where we don't care about the dirs
      }
  deriving (Show, Eq, Generic)

instance Validity ContentsMap where
  validate mm@(ContentsMap df) =
    mconcat
      [ genericValidate mm,
        declare "None of the sub-directories are empty" $ DF.null df || not (DF.anyEmptyDir df)
      ]

instance NFData ContentsMap

empty :: ContentsMap
empty = ContentsMap DF.empty

insert :: Path Rel File -> ByteString -> ContentsMap -> Maybe ContentsMap
insert k v (ContentsMap df) = case DF.insertFile k v df of
  Left _ -> Nothing
  Right df' -> Just $ ContentsMap df'

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
fromListIgnoringCollisions :: [(Path Rel File, ByteString)] -> ContentsMap
fromListIgnoringCollisions = ContentsMap . foldl' go DF.empty . sortOn (Down . fst)
  where
    go :: DirForest ByteString -> (Path Rel File, ByteString) -> DirForest ByteString
    go df (rf, sfm) = case DF.insertFile rf sfm df of
      Left _ -> df
      Right df' -> df'

contentsMapFiles :: ContentsMap -> Map (Path Rel File) ByteString
contentsMapFiles = DF.toFileMap . contentsMapDirForest

singleton :: Path Rel File -> ByteString -> ContentsMap
singleton k v = ContentsMap $ DF.singletonFile k v

filterHidden :: ContentsMap -> ContentsMap
filterHidden = ContentsMap . DF.filterHidden . contentsMapDirForest

-- This is only used for testing anyway
union :: ContentsMap -> ContentsMap -> Maybe ContentsMap
union (ContentsMap df1) (ContentsMap df2) = case DF.union df1 df2 of
  DF.InsertionErrors _ -> Nothing
  DF.NoInsertionErrors df' -> Just $ ContentsMap df'

unions :: [ContentsMap] -> Maybe ContentsMap
unions = foldlM union empty
