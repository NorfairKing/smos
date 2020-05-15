{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.API
  ( module Smos.API,
    module X,
  )
where

import Control.DeepSeq
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Timed
import Data.Proxy
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.UUID.Typed as UUID
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Path
import Path.Internal
import Servant.API as X
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Smos.API.HashedPassword as X
import Smos.API.Password as X
import Smos.API.Username as X

syncAPI :: Proxy SyncAPI
syncAPI = Proxy

type SyncAPI = ToServantApi APIRoutes

data APIRoutes route
  = APIRoutes
      { unprotectedRoutes :: route :- ToServantApi UnprotectedRoutes,
        protectedRoutes :: route :- ToServantApi ProtectedRoutes
      }
  deriving (Generic)

syncUnprotectedAPI :: Proxy SyncUnprotectedAPI
syncUnprotectedAPI = Proxy

type SyncUnprotectedAPI = ToServantApi UnprotectedRoutes

data UnprotectedRoutes route
  = UnprotectedRoutes
      { postRegister :: !(route :- PostRegister),
        postLogin :: !(route :- PostLogin)
      }
  deriving (Generic)

syncProtectedAPI :: Proxy SyncProtectedAPI
syncProtectedAPI = Proxy

type ProtectAPI = Auth '[JWT] AuthCookie

newtype AuthCookie
  = AuthCookie
      { authCookieUsername :: Username
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

type SyncProtectedAPI = ToServantApi ProtectedRoutes

newtype ProtectedRoutes route
  = ProtectedRoutes
      { postSync :: route :- ProtectAPI :> PostSync
      }
  deriving (Generic)

type PostRegister = "register" :> ReqBody '[JSON] Register :> PostNoContent '[JSON] NoContent

data Register
  = Register
      { registerUsername :: Username,
        registerPassword :: Password
      }
  deriving (Show, Eq, Generic)

instance Validity Register

instance NFData Register

instance ToJSON Register

instance FromJSON Register

type PostLogin =
  "login" :> ReqBody '[JSON] Login :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" T.Text] NoContent)

data Login
  = Login
      { loginUsername :: Username,
        loginPassword :: Password
      }
  deriving (Show, Eq, Generic)

instance Validity Login

instance NFData Login

instance ToJSON Login

instance FromJSON Login

type PostSync = "sync" :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResponse

type FileUUID = UUID SyncFile

data SyncServer

type ServerUUID = UUID SyncServer

instance FromJSONKey (Path Rel File) where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
      case parseRelFile (T.unpack t) of
        Nothing -> fail "failed to parse relative file"
        Just rf -> pure rf

instance ToJSONKey (Path Rel File) where
  toJSONKey = toJSONKeyText $ T.pack . fromRelFile

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

data SyncFile
  = SyncFile
      { syncFilePath :: Path Rel File,
        syncFileContents :: ByteString
      }
  deriving (Show, Eq, Generic)

instance Validity SyncFile

instance NFData SyncFile

instance FromJSON SyncFile where
  parseJSON =
    withObject "SyncFile" $ \o ->
      SyncFile <$> o .: "path"
        <*> ( do
                base64Contents <- SB8.pack <$> o .: "contents"
                case Base64.decode base64Contents of
                  Left err -> fail err
                  Right r -> pure r
            )

instance ToJSON SyncFile where
  toJSON SyncFile {..} =
    object ["path" .= syncFilePath, "contents" .= SB8.unpack (Base64.encode syncFileContents)]

type SyncRequest = Mergeful.SyncRequest (Path Rel File) FileUUID SyncFile

data SyncResponse
  = SyncResponse
      { syncResponseServerId :: ServerUUID,
        syncResponseItems :: Mergeful.SyncResponse (Path Rel File) FileUUID SyncFile
      }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance NFData SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o -> SyncResponse <$> o .: "server-id" <*> o .: "items"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object ["server-id" .= syncResponseServerId, "items" .= syncResponseItems]
