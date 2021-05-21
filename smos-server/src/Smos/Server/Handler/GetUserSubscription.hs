module Smos.Server.Handler.GetUserSubscription
  ( serveGetUserSubscription,
  )
where

import Smos.Server.Handler.Import
import Smos.Server.Subscription

serveGetUserSubscription :: AuthNCookie -> ServerHandler SubscriptionStatus
serveGetUserSubscription ac = withUserId ac $ \_ -> getSubscriptionStatusForUser (authNCookieUsername ac)
