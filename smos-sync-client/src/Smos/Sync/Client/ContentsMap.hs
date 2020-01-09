{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.ContentsMap
  ( ContentsMap(..)
  , empty
  , singleton
  , insert
  , union
  , unions
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()

import Control.DeepSeq
import Control.Monad

import Path

import Smos.Sync.Client.DirForest

newtype ContentsMap =
  ContentsMap
    { contentsMapFiles :: Map (Path Rel File) ByteString
    }
  deriving (Show, Eq, Generic)

instance Validity ContentsMap where
  validate cm =
    mconcat
      [ genericValidate cm
      , decorate "The map can be translated to a valid DirForest" $
        case makeDirForest $ contentsMapFiles cm of
          Left fp -> invalid $ "Duplicate path: " <> fp
          Right df -> validate df
      ]

instance NFData ContentsMap

empty :: ContentsMap
empty = ContentsMap M.empty

singleton :: Path Rel File -> ByteString -> ContentsMap
singleton k v = ContentsMap $ M.singleton k v

insert :: Path Rel File -> ByteString -> ContentsMap -> Maybe ContentsMap
insert k v (ContentsMap m) = constructValid $ ContentsMap $ M.insert k v m

union :: ContentsMap -> ContentsMap -> Maybe ContentsMap
union (ContentsMap m1) (ContentsMap m2) = constructValid $ ContentsMap $ M.union m1 m2

unions :: [ContentsMap] -> Maybe ContentsMap
unions = foldM union empty
