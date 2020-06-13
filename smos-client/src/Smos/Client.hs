{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Client
  ( module Smos.Client,
    module X,
    Token,
    SetCookie,
  )
where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Path
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client
import Smos.API as X
import Smos.Data hiding (Header)
import Smos.Report.Next
import System.Exit
import Web.Cookie

clientPostRegister :: Register -> ClientM NoContent

clientPostLogin ::
  Login ->
  ClientM (Headers '[Header "Set-Cookie" T.Text] NoContent)
clientPostRegister :<|> clientPostLogin = client (flatten syncUnprotectedAPI)

clientPostSync :: Token -> SyncRequest -> ClientM SyncResponse

clientGetSmosFile :: Token -> Path Rel File -> ClientM SmosFile

clientPutSmosFile :: Token -> Path Rel File -> SmosFile -> ClientM NoContent

clientGetNextActionReport :: Token -> ClientM NextActionReport
clientPostSync :<|> clientGetSmosFile :<|> clientPutSmosFile :<|> clientGetNextActionReport = client (flatten syncProtectedAPI)

clientLogin :: Login -> ClientM (Either HeaderProblem Token)
clientLogin = fmap (fmap sessionToToken) . clientLoginSession

clientLoginSession :: Login -> ClientM (Either HeaderProblem SetCookie)
clientLoginSession lf = do
  res <- clientPostLogin lf
  pure $
    case res of
      Headers NoContent (HCons sessionHeader HNil) ->
        case sessionHeader of
          MissingHeader -> Left ProblemMissingHeader
          UndecodableHeader b -> Left $ ProblemUndecodableHeader b
          Header sessionText ->
            let cookies =
                  parseSetCookie . encodeUtf8 <$> T.lines sessionText
                jwtCookie =
                  find ((== "JWT-Cookie") . setCookieName) cookies
             in case jwtCookie of
                  Nothing -> Left ProblemMissingJWTCookie
                  Just setCookie -> Right setCookie

sessionToToken :: SetCookie -> Token
sessionToToken = Token . setCookieValue

data HeaderProblem
  = ProblemMissingHeader
  | ProblemUndecodableHeader ByteString
  | ProblemMissingJWTCookie
  deriving (Show, Eq, Generic)

login :: ClientEnv -> Login -> IO (Either LoginError Token)
login cenv lf = do
  errOrRes <- runClient cenv $ clientLogin lf
  pure $
    case errOrRes of
      Left se -> Left $ LoginServantError se
      Right (Left hp) -> Left $ LoginHeaderProblem hp
      Right (Right t) -> Right t

data LoginError
  = LoginServantError ClientError
  | LoginHeaderProblem HeaderProblem
  deriving (Show, Eq, Generic)

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrDie :: ClientEnv -> ClientM a -> IO a
runClientOrDie cenv func = do
  errOrResp <- runClient cenv func
  case errOrResp of
    Left err -> liftIO $ die $ show err
    Right resp -> pure resp
