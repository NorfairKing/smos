{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Server.DB.CASHash
  ( CASHash (..),
    hashContents,
  )
where

import Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import Database.Persist.Sql (PersistField, PersistFieldSql)

newtype CASHash = CASHash {unCASHash :: ByteString}
  deriving (Show, Eq)
  deriving newtype (PersistField, PersistFieldSql)

hashContents :: ByteString -> CASHash
hashContents chunk = CASHash $ SHA256.hash chunk
