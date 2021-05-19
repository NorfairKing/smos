{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.PutUserSubscription
  ( servePutUserSubscription,
  )
where

import Smos.Server.Handler.Import

servePutUserSubscription ::
  AuthNCookie ->
  Username ->
  -- | New end date
  UTCTime ->
  ServerHandler NoContent
servePutUserSubscription ac un end = asAdmin (authNCookieUsername ac) $ do
  mUser <- runDB $ getBy $ UniqueUsername un
  case mUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just (Entity uid User {..}) -> do
      _ <-
        runDB $
          upsertBy
            (UniqueSubscriptionUser uid)
            ( Subscription
                { subscriptionUser = uid,
                  subscriptionEnd = end
                }
            )
            [ SubscriptionEnd =. end
            ]
      pure NoContent
