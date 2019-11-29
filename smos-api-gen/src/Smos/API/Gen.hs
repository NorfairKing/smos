{-# OPTIONS -fno-warn-orphans #-}

module Smos.API.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import Data.GenValidity.UUID.Typed ()

import qualified Data.Text as T

import Test.QuickCheck

import Smos.API

instance GenValid UsernameChar where
  genValid = UsernameChar <$> elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid =
    Username . T.pack . map unUsernameChar <$>
    ((:) <$> genValid <*> ((:) <$> genValid <*> ((:) <$> genValid <*> genValid)))
  shrinkValid = shrinkValidStructurally

instance GenValid PasswordChar where
  genValid = PasswordChar <$> elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
  shrinkValid = shrinkValidStructurally

instance GenValid Password where
  genValid =
    Password . T.pack . map unPasswordChar <$>
    ((:) <$> genValid <*> ((:) <$> genValid <*> ((:) <$> genValid <*> genValid)))
  shrinkValid = shrinkValidStructurally

instance GenValid Register where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Login where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncFile where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncResponse where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
