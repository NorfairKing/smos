{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Account
  ( getAccountR,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LB
import qualified Network.HTTP.Types as Http
import Servant.Types.SourceT as Source
import Smos.Web.Server.Handler.Import
import qualified Yesod

getAccountR :: Handler Html
getAccountR = withLogin $ \t -> do
  UserSubscription {..} <- runClientOrErr $ clientGetUserSubscription t
  now <- liftIO getCurrentTime
  withNavBar $ do
    token <- genToken
    $(widgetFile "account")
