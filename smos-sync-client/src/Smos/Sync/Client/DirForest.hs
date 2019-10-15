{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.DirForest
  ( DirForest(..)
  , DirOrFile(..)
  , makeDirForest
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()

import qualified System.FilePath as FP

import Control.Monad

import Path

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
        go2 m [] = pure m
        go2 m [fp] =
          case M.lookup fp m of
            Nothing -> pure $ M.insert fp (File bs) m
            Just _ -> Left fp
        go2 m (dp:rest) =
          case M.lookup dp m of
            Nothing -> do
              m'' <- go2 M.empty rest
              pure $ M.insert dp (Dir $ DirForest m'') m
            Just dof ->
              case dof of
                Dir (DirForest df) -> go2 df rest
                File _ -> Left dp
