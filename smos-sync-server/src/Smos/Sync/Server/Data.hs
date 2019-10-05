{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Server.Data where

import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Path
import Path.Internal

import Database.Persist
import Database.Persist.Sql

import Data.Mergeful.Timed

deriving instance PersistFieldSql (Path Rel File) -- TODO Not entirely safe

deriving instance PersistField (Path Rel File) -- TODO Not entirely safe

deriving instance PersistFieldSql ServerTime

deriving instance PersistField ServerTime

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue (PersistByteString bs) =
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalidy Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid
  fromPersistValue pv = Left $ "Invalid Persist value to parse to UUID: " <> T.pack (show pv)

instance PersistFieldSql (UUID a) where
  sqlType Proxy = SqlBlob
