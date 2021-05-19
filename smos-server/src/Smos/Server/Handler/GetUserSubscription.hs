module Smos.Server.Handler.GetUserSubscription
  ( serveGetUserSubscription,
  )
where

import Smos.Server.Handler.Import

serveGetUserSubscription :: AuthNCookie -> ServerHandler UserSubscription
serveGetUserSubscription ac = withUserId ac $ \uid -> do
  mSubscription <- runDB $ getBy $ UniqueSubscriptionUser uid
  pure
    UserSubscription
      { userSubscriptionEnd = subscriptionEnd . entityVal <$> mSubscription
      }
