{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- Import this module qualified
module Smos.Sync.Client.DirForest
  ( DirForest (..),
    DirOrFile (..),
    makeDirForest,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import qualified System.FilePath as FP

newtype DirForest a
  = DirForest
      { dirForestMap :: Map FilePath (DirOrFile a)
      }
  deriving (Show, Eq, Generic)

instance Validity a => Validity (DirForest a) where
  validate df =
    mconcat
      [ genericValidate df,
        decorateList (M.toList (dirForestMap df)) $ \(fp, _) ->
          declare "does not conain separators" $ length (FP.splitDirectories fp) == 1
      ]

instance NFData a => NFData (DirForest a)

data DirOrFile a
  = Dir (DirForest a)
  | File a
  deriving (Show, Eq, Generic)

instance Validity a => Validity (DirOrFile a)

instance NFData a => NFData (DirOrFile a)

makeDirForest :: forall a. Map (Path Rel File) a -> Either FilePath (DirForest a)
makeDirForest = fmap DirForest . foldM go M.empty . M.toList
  where
    go ::
      Map FilePath (DirOrFile a) ->
      (Path Rel File, a) ->
      Either FilePath (Map FilePath (DirOrFile a))
    go m' (rp, bs) = go2 m' (FP.splitDirectories $ fromRelFile rp)
      where
        go2 ::
          Map FilePath (DirOrFile a) ->
          [FilePath] ->
          Either FilePath (Map FilePath (DirOrFile a))
        go2 m [] = pure m
        go2 m [fp] =
          case M.lookup fp m of
            Nothing -> pure $ M.insert fp (File bs) m
            Just _ -> Left fp
        go2 m (dp : rest) =
          case M.lookup dp m of
            Nothing -> do
              m'' <- go2 M.empty rest
              pure $ M.insert dp (Dir $ DirForest m'') m
            Just dof ->
              case dof of
                Dir (DirForest df) -> go2 df rest
                File _ -> Left dp
