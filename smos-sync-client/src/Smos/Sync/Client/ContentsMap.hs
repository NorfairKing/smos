{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.ContentsMap
  ( ContentsMap(..)
  , contentsMapFiles
  , empty
  , singleton
  , insert
  , union
  , unions
  , DirForest(..)
  , makeDirForest
  ) where

import Debug.Trace

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Function
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
import Path.Internal

import Smos.Report.Streaming

import Smos.Sync.API

import Smos.Sync.Client.OptParse.Types

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

newtype DirForest =
  DirForest
    { dirForestMap :: Map FilePath DirOrFile
    }
  deriving (Show, Eq, Generic)

instance Validity DirForest where
  validate df =
    mconcat
      [ genericValidate df
      , decorateList (M.toList (dirForestMap df)) $ \(fp, _) ->
          declare "does not conain separators" $ length (FP.splitDirectories fp) == 1
      ]

data DirOrFile
  = Dir DirForest
  | File ByteString
  deriving (Show, Eq, Generic)

instance Validity DirOrFile

makeDirForest :: Map (Path Rel File) ByteString -> Either FilePath DirForest
makeDirForest = fmap DirForest . foldM go M.empty . M.toList
  where
    go ::
         Map FilePath DirOrFile
      -> (Path Rel File, ByteString)
      -> Either FilePath (Map FilePath DirOrFile)
    go m' (rp, bs) = go2 m' (FP.splitDirectories $ fromRelFile rp)
      where
        go2 :: Map FilePath DirOrFile -> [FilePath] -> Either FilePath (Map FilePath DirOrFile)
        go2 m [fp] =
          case M.lookup fp m of
            Nothing -> pure $ M.insert fp (File bs) m
            Just _ -> Left fp
        go2 m (dp:rest) =
          case M.lookup dp m of
            Nothing -> do
              m' <- go2 M.empty rest
              pure $ M.insert dp (Dir $ DirForest m') m
            Just dof ->
              case dof of
                Dir (DirForest df) -> go2 df rest
                File _ -> Left dp

relRoot :: Path Rel Dir
relRoot = [reldir|.|]
