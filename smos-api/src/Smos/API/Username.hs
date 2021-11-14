{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.API.Username
  ( Username (..),
    usernameString,
    parseUsername,
    parseUsernameWithError,
    validUsernameChar,
    UsernameChar (..),
  )
where

import Autodocodec
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Fail as Fail
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON, ToJSONKey)
import qualified Data.Char as Char
import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.HttpApiData
import Web.PathPieces

newtype Username = Username
  { usernameText :: Text
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (IsString, NFData, Hashable, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long.",
        decorateList (map UsernameChar $ T.unpack t) validate
      ]

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) =
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance FromJSONKey Username where
  fromJSONKey = FromJSONKeyTextParser parseUsername

instance HasCodec Username where
  codec = bimapCodec prettyValidate id $ dimapCodec Username usernameText codec

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

instance ToHttpApiData Username where
  toUrlPiece = usernameText
  toQueryParam = usernameText

instance FromHttpApiData Username where
  parseUrlPiece = left T.pack . parseUsernameWithError
  parseQueryParam = left T.pack . parseUsernameWithError

usernameString :: Username -> String
usernameString = T.unpack . usernameText

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> Fail.fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError t = prettyValidate $ Username t

newtype UsernameChar = UsernameChar
  { unUsernameChar :: Char
  }
  deriving (Show, Eq, Generic)

instance Validity UsernameChar where
  validate (UsernameChar '-') = mempty
  validate (UsernameChar '_') = mempty
  validate (UsernameChar c) =
    mconcat
      [ check (not (Char.isControl c)) "The character is not a control character.",
        check (Char.isAlphaNum c) "The character is alphanumeric.",
        check (Char.isLatin1 c) "The character is part of Latin1."
      ]

validUsernameChar :: Char -> Bool
validUsernameChar = isValid . UsernameChar
