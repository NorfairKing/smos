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
import Smos.API
import Test.QuickCheck

instance GenValid UsernameChar where
  genValid = UsernameChar <$> elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid =
    Username . T.pack . map unUsernameChar
      <$> ((:) <$> genValid <*> ((:) <$> genValid <*> ((:) <$> genValid <*> genValid)))
  shrinkValid = shrinkValidStructurally

instance GenValid Register where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Login where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncFile where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- Don't shrink passwords
