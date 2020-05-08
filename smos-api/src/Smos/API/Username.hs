{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import Control.DeepSeq
import Control.Monad.Fail as Fail
import Data.Aeson as JSON
import Data.Aeson.Types as JSON (toJSONKeyText)
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.PathPieces
import YamlParse.Applicative

newtype Username
  = Username
      { usernameText :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long.",
        decorateList (map UsernameChar $ T.unpack t) validate
      ]

instance NFData Username

instance Hashable Username

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

instance FromJSON Username where
  parseJSON = viaYamlSchema

instance YamlSchema Username where
  yamlSchema = Username <$> yamlSchema

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

usernameString :: Username -> String
usernameString = T.unpack . usernameText

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> Fail.fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError t = prettyValidate $ Username t

instance ToJSON Username where
  toJSON = toJSON . usernameText

instance ToJSONKey Username where
  toJSONKey = toJSONKeyText usernameText

newtype UsernameChar
  = UsernameChar
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
