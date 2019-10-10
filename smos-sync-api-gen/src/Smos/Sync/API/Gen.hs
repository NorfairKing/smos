{-# OPTIONS -fno-warn-orphans #-}

module Smos.Sync.API.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import Data.GenValidity.UUID.Typed ()

import qualified Data.Text as T

import Smos.Sync.API

instance GenValid UsernameChar where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid =
    Username . T.pack . map unUsernameChar <$>
    ((:) <$> genValid <*> ((:) <$> genValid <*> ((:) <$> genValid <*> genValid)))
  shrinkValid = shrinkValidStructurally

instance GenValid Register where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncFile where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncResponse where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
