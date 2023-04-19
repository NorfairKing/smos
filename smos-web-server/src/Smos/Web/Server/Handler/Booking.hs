{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Booking
  ( getBookingR,
    getBookUserR,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LB
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Network.HTTP.Types as Http
import Servant.Types.SourceT as Source
import Smos.Web.Server.Handler.Import
import qualified Yesod

getBookingR :: Handler Html
getBookingR = withLogin $ \t -> do
  now <- liftIO getCurrentTime
  status <- runClientOrErr $ clientGetUserSubscription t
  let showBookingPage = status /= NotSubscribed
  withNavBar $ do
    $(widgetFile "booking")

getBookUserR :: Username -> Handler Html
getBookUserR username = do
  let timezones :: [(TZLabel, TZ)]
      timezones = map (\tzl -> (tzl, tzByLabel tzl)) [minBound .. maxBound]
  let options :: [(Day, [(TimeOfDay, NominalDiffTime)])]
      options =
        [ ( fromGregorian 2022 04 18,
            [ (TimeOfDay 23 00 0, 60 * 15),
              (TimeOfDay 23 15 0, 60 * 15),
              (TimeOfDay 23 30 0, 60 * 15)
            ]
          ),
          ( fromGregorian 2022 04 19,
            [ (TimeOfDay 23 00 0, 60 * 15),
              (TimeOfDay 23 15 0, 60 * 15),
              (TimeOfDay 23 30 0, 60 * 15)
            ]
          )
        ]
  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user")
