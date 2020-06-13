module Smos.Server.Handler.Import
  ( module X,
    withUserEntity,
    withUserId,
    withUser,
  )
where

import Control.Monad.Except as X
import Control.Monad.Reader as X
import Data.UUID.Typed as X
import Servant.API as X
import Servant.Server as X
import Smos.API as X
import Smos.Server.DB as X
import Smos.Server.Env as X
import Text.Show.Pretty as X

withUserEntity :: Username -> (Entity User -> SyncHandler a) -> SyncHandler a
withUserEntity un func = do
  mu <- runDB $ getBy $ UniqueUsername un
  case mu of
    Nothing -> throwError err404
    Just e -> func e

withUser :: Username -> (User -> SyncHandler a) -> SyncHandler a
withUser un func = withUserEntity un $ func . entityVal

withUserId :: Username -> (UserId -> SyncHandler a) -> SyncHandler a
withUserId un func = withUserEntity un $ func . entityKey
