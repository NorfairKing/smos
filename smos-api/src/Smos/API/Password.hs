{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.API.Password
  ( Password (..),
    passwordString,
    passwordByteString,
    parsePassword,
    parsePasswordWithError,
    validPasswordChar,
    PasswordChar (..),
  )
where

import Control.DeepSeq
import Control.Monad.Fail as Fail
import Data.Aeson as JSON
import Data.Aeson.Types as JSON (toJSONKeyText)
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Database.Persist.Sql
import GHC.Generics (Generic)
import Web.PathPieces
import YamlParse.Applicative

newtype Password
  = Password
      { passwordText :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Password where
  validate (Password t) =
    mconcat
      [ check (not (T.null t)) "The password is not empty.",
        check (T.length t >= 3) "The password is at least three characters long.",
        decorateList (map PasswordChar $ T.unpack t) validate
      ]

instance Hashable Password

instance NFData Password

instance PersistField Password where
  toPersistValue (Password t) = PersistText t
  fromPersistValue (PersistText t) =
    case parsePassword t of
      Nothing -> Left "Text isn't a valid password"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Password where
  sqlType _ = SqlString

instance FromJSONKey Password where
  fromJSONKey = FromJSONKeyTextParser parsePassword

instance FromJSON Password where
  parseJSON = viaYamlSchema

instance YamlSchema Password where
  yamlSchema = Password <$> yamlSchema

instance PathPiece Password where
  fromPathPiece = parsePassword
  toPathPiece = passwordText

passwordString :: Password -> String
passwordString = T.unpack . passwordText

passwordByteString :: Password -> ByteString
passwordByteString = TE.encodeUtf8 . passwordText

parsePassword :: MonadFail m => Text -> m Password
parsePassword t =
  case parsePasswordWithError t of
    Left err -> Fail.fail err
    Right un -> pure un

parsePasswordWithError :: Text -> Either String Password
parsePasswordWithError t = prettyValidate $ Password t

instance ToJSON Password where
  toJSON = toJSON . passwordText

instance ToJSONKey Password where
  toJSONKey = toJSONKeyText passwordText

newtype PasswordChar
  = PasswordChar
      { unPasswordChar :: Char
      }
  deriving (Show, Eq, Generic)

instance Validity PasswordChar where
  validate (PasswordChar '-') = mempty
  validate (PasswordChar '_') = mempty
  validate (PasswordChar c) =
    mconcat
      [ check (not (Char.isControl c)) "The character is not a control character.",
        check (Char.isAlphaNum c) "The character is alphanumeric.",
        check (Char.isLatin1 c) "The character is part of Latin1."
      ]

validPasswordChar :: Char -> Bool
validPasswordChar = isValid . PasswordChar
