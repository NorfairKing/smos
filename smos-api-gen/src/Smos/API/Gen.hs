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
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid SHA256 where
  genValid = hashBytes <$> genValid
  shrinkValid _ = [] -- No point in shrinking a hash, I think.

instance GenValid UsernameChar where
  genValid = UsernameChar <$> elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid =
    Username . T.pack . map unUsernameChar
      <$> ((:) <$> genValid <*> ((:) <$> genValid <*> ((:) <$> genValid <*> genValid)))
  shrinkValid = shrinkValidStructurally

instance GenValid UserPermissions where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- Don't shrink passwords

instance GenValid Register where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Login where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncFile where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncRequest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BackupInfo where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SubscriptionStatus where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UserInfo where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
