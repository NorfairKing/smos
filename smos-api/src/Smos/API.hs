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

import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.DirForest (DirForest (..))
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Persistent ()
import Data.Proxy
import Data.SemVer as Version
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import Data.UUID.Typed as UUID
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Path
import Path.Internal
import Servant.API as X
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Smos.API.Password as X
import Smos.API.SHA256 as X
import Smos.API.Username as X
import Smos.Data hiding (Header)
import Smos.Report.Agenda
import Smos.Report.Next

apiVersion :: Version
apiVersion = version 0 1 0 [] []

smosAPI :: Proxy SmosAPI
smosAPI = Proxy

type SmosAPI = ToServantApi APIRoutes

data APIRoutes route = APIRoutes
  { unprotectedRoutes :: route :- ToServantApi UnprotectedRoutes,
    protectedRoutes :: route :- ToServantApi ProtectedRoutes,
    adminRoutes :: route :- ToServantApi AdminRoutes
  }
  deriving (Generic)

smosUnprotectedAPI :: Proxy SmosUnprotectedAPI
smosUnprotectedAPI = Proxy

type SmosUnprotectedAPI = ToServantApi UnprotectedRoutes

data UnprotectedRoutes route = UnprotectedRoutes
  { getApiVersion :: !(route :- GetAPIVersion),
    postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin)
  }
  deriving (Generic)

smosProtectedAPI :: Proxy SmosProtectedAPI
smosProtectedAPI = Proxy

type ProtectAPI = Auth '[JWT] AuthCookie

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie where
  parseJSON = withObject "AuthCookie" $ \o ->
    AuthCookie
      <$> o .: "username"

instance ToJSON AuthCookie where
  toJSON AuthCookie {..} =
    object
      [ "username" .= authCookieUsername
      ]

instance FromJWT AuthCookie

instance ToJWT AuthCookie

type SmosProtectedAPI = ToServantApi ProtectedRoutes

data ProtectedRoutes route = ProtectedRoutes
  { getUserPermissions :: !(route :- ProtectAPI :> GetUserPermissions),
    deleteUser :: !(route :- ProtectAPI :> DeleteUser),
    postSync :: !(route :- ProtectAPI :> PostSync),
    getListBackups :: !(route :- ProtectAPI :> GetListBackups),
    postBackup :: !(route :- ProtectAPI :> PostBackup),
    getBackup :: !(route :- ProtectAPI :> GetBackup),
    putRestoreBackup :: !(route :- ProtectAPI :> PutRestoreBackup),
    deleteBackup :: !(route :- ProtectAPI :> DeleteBackup),
    getListSmosFiles :: !(route :- ProtectAPI :> GetListSmosFiles),
    getSmosFile :: !(route :- ProtectAPI :> GetSmosFile),
    putSmosFile :: !(route :- ProtectAPI :> PutSmosFile),
    reportRoutes :: !(route :- ToServantApi ReportRoutes)
  }
  deriving (Generic)

type GetAPIVersion = "api-version" :> Get '[JSON] Version

type PostRegister = "register" :> ReqBody '[JSON] Register :> PostNoContent '[JSON] NoContent

data Register = Register
  { registerUsername :: Username,
    registerPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Register

instance NFData Register

instance ToJSON Register

instance FromJSON Register

type PostLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" T.Text] NoContent)

data Login = Login
  { loginUsername :: Username,
    loginPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Login

instance NFData Login

instance ToJSON Login

instance FromJSON Login

type GetUserPermissions = "user" :> "permissions" :> Get '[JSON] UserPermissions

data UserPermissions = UserPermissions
  { userPermissionsIsAdmin :: Bool
  }
  deriving (Show, Eq, Generic)

instance Validity UserPermissions

instance NFData UserPermissions

instance FromJSON UserPermissions where
  parseJSON = withObject "UserPermissions" $ \o ->
    UserPermissions
      <$> o .:? "admin" .!= False

instance ToJSON UserPermissions where
  toJSON UserPermissions {..} =
    object
      [ "admin" .= userPermissionsIsAdmin
      ]

type DeleteUser = "user" :> DeleteNoContent '[JSON] NoContent

type PostSync = "sync" :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResponse

data BackupInfo = BackupInfo
  { backupInfoUUID :: !BackupUUID,
    -- | When the backup was made
    backupInfoTime :: !UTCTime,
    -- | In bytes
    backupInfoSize :: !Word64
  }
  deriving (Show, Eq, Generic)

instance Validity BackupInfo

instance NFData BackupInfo

instance FromJSON BackupInfo where
  parseJSON = withObject "Backup" $ \o ->
    BackupInfo
      <$> o .: "uuid"
      <*> o .: "time"
      <*> o .: "size"

instance ToJSON BackupInfo where
  toJSON BackupInfo {..} =
    object
      [ "uuid" .= backupInfoUUID,
        "time" .= backupInfoTime,
        "size" .= backupInfoSize
      ]

type BackupUUID = UUID BackupInfo

type GetListBackups = "backups" :> Get '[JSON] [BackupInfo]

type PostBackup = "backup" :> Post '[JSON] BackupUUID

type GetBackup = "backup" :> Capture "backup" BackupUUID :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type PutRestoreBackup = "backup" :> Capture "backup" BackupUUID :> "restore" :> PutNoContent '[JSON] NoContent

type DeleteBackup = "backup" :> Capture "backup" BackupUUID :> DeleteNoContent '[JSON] NoContent

data SyncServer

type ServerUUID = UUID SyncServer

deriving instance PersistFieldSql (Path Rel File) -- TODO Not entirely safe

deriving instance PersistField (Path Rel File) -- TODO Not entirely safe

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue (PersistByteString bs) =
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalidy Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid
  fromPersistValue pv = Left $ "Invalid Persist value to parse to UUID: " <> T.pack (show pv)

instance PersistFieldSql (UUID a) where
  sqlType Proxy = SqlBlob

newtype SyncFile = SyncFile
  { syncFileContents :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity SyncFile

instance NFData SyncFile

instance FromJSON SyncFile where
  parseJSON =
    withObject "SyncFile" $ \o ->
      SyncFile
        <$> ( do
                base64Contents <- SB8.pack <$> o .: "contents"
                case Base64.decode base64Contents of
                  Left err -> fail err
                  Right r -> pure r
            )

instance ToJSON SyncFile where
  toJSON SyncFile {..} =
    object ["contents" .= SB8.unpack (Base64.encode syncFileContents)]

data SyncRequest = SyncRequest
  { syncRequestItems :: Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
  }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance NFData SyncRequest

instance FromJSON SyncRequest where
  parseJSON = withObject "SyncRequest" $ \o -> SyncRequest <$> o .: "items"

instance ToJSON SyncRequest where
  toJSON SyncRequest {..} =
    object ["items" .= syncRequestItems]

data SyncResponse = SyncResponse
  { syncResponseServerId :: ServerUUID,
    syncResponseItems :: Mergeful.SyncResponse (Path Rel File) (Path Rel File) SyncFile
  }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance NFData SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o -> SyncResponse <$> o .: "server-id" <*> o .: "items"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object ["server-id" .= syncResponseServerId, "items" .= syncResponseItems]

instance FromHttpApiData (Path Rel File) where
  parseQueryParam t = left (T.pack . displayException :: SomeException -> Text) $ parseRelFile (T.unpack t)

instance ToHttpApiData (Path Rel File) where
  toQueryParam = T.pack . fromRelFile

type GetListSmosFiles = "files" :> Get '[JSON] (DirForest SmosFile)

type GetSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> Get '[JSON] SmosFile

type PutSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> ReqBody '[JSON] SmosFile :> PutNoContent '[JSON] NoContent

type ReportsAPI = ToServantApi ReportRoutes

data ReportRoutes route = ReportRoutes
  { getNextActionReport :: !(route :- ProtectAPI :> GetNextActionReport),
    getAgendaReport :: !(route :- ProtectAPI :> GetAgendaReport)
  }
  deriving (Generic)

type GetNextActionReport = "report" :> "next" :> Get '[JSON] NextActionReport

type GetAgendaReport = "report" :> "agenda" :> Get '[JSON] AgendaReport

reportsAPI :: Proxy ReportsAPI
reportsAPI = Proxy

type ProtectAdmin = Auth '[JWT] AuthCookie

newtype AdminCookie = AdminCookie {adminCookieUsername :: Username}
  deriving (Show, Eq, Ord, Generic)

data AdminRoutes route = AdminRoutes
  { getUsers :: !(route :- ProtectAdmin :> GetUsers)
  }
  deriving (Generic)

type GetUsers = "admin" :> "users" :> Get '[JSON] [UserInfo]

data UserInfo = UserInfo
  { userInfoUsername :: !Username,
    userInfoAdmin :: !Bool,
    userInfoCreated :: !UTCTime,
    userInfoLastLogin :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

instance Validity UserInfo

instance NFData UserInfo

instance ToJSON UserInfo where
  toJSON UserInfo {..} =
    object
      [ "name" .= userInfoUsername,
        "admin" .= userInfoAdmin,
        "created" .= userInfoCreated,
        "last-login" .= userInfoLastLogin
      ]

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o ->
    UserInfo
      <$> o .: "name"
      <*> o .: "admin"
      <*> o .: "created"
      <*> o .:? "last-login"
