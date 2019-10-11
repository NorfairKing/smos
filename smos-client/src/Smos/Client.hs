{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Smos.Client
  ( module Smos.Client
  , module X
  ) where

import GHC.Generics

import Data.ByteString (ByteString)
import Web.Cookie

import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client

import Smos.API as X

clientPostRegister :: Register -> ClientM NoContent
clientPostLogin ::
     Login
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientPostRegister :<|> clientPostLogin = client (flatten syncUnprotectedAPI)

clientPostSync :: Token -> SyncRequest -> ClientM SyncResponse
clientPostSync = client (flatten syncProtectedAPI)

clientLogin :: Login -> ClientM (Either HeaderProblem Token)
clientLogin = fmap (fmap sessionToToken) . clientLoginSession

clientLoginSession :: Login -> ClientM (Either HeaderProblem SetCookie)
clientLoginSession lf = do
  res <- clientPostLogin lf
  pure $
    case res of
      Headers NoContent (HCons _ (HCons sessionHeader HNil)) ->
        case sessionHeader of
          MissingHeader -> Left ProblemMissingHeader
          UndecodableHeader b -> Left $ ProblemUndecodableHeader b
          Header session -> Right session

sessionToToken :: SetCookie -> Token
sessionToToken = Token . setCookieValue

data HeaderProblem
  = ProblemMissingHeader
  | ProblemUndecodableHeader ByteString
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
