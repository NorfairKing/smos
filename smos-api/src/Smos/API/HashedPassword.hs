{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.API.HashedPassword
  ( passwordHash
  , HashedPassword()
  , validatePassword
  ) where

import GHC.Generics (Generic)

import Data.Validity

import Data.ByteString (ByteString)

import qualified Crypto.BCrypt as BCrypt

import Database.Persist.Sql

import Smos.API.Password

newtype HashedPassword =
  HashedPassword ByteString
  deriving (Show, Eq, Read, Generic, PersistField, PersistFieldSql)

instance Validity HashedPassword where
  validate (HashedPassword password) =
    declare "The password uses our chosen hashing policy" $
    BCrypt.hashUsesPolicy hashingpolicy password

hashingpolicy :: BCrypt.HashingPolicy
hashingpolicy = BCrypt.fastBcryptHashingPolicy

passwordHash :: Password -> IO (Maybe HashedPassword)
passwordHash =
  fmap (fmap HashedPassword) . BCrypt.hashPasswordUsingPolicy hashingpolicy . passwordByteString

validatePassword :: HashedPassword -> Password -> Bool
validatePassword (HashedPassword hp) = BCrypt.validatePassword hp . passwordByteString
