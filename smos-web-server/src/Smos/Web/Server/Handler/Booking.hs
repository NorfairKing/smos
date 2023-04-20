{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Booking
  ( getBookingR,
    getBookUserR,
    postBookUser,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import Data.Time.Zones
import Data.Time.Zones.All
import GHC.Generics (Generic)
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

getClientTZLabel :: Handler TZLabel
getClientTZLabel = do
  mTimezoneName <- lookupGetParam "timezone"
  let label = fromMaybe Etc__UTC $ mTimezoneName >>= (fromTZName . TE.encodeUtf8)
  pure label

getBookUserR :: Username -> Handler Html
getBookUserR username = do
  let timezones :: [(TZLabel, TZ)]
      timezones = map (\tzl -> (tzl, tzByLabel tzl)) [minBound .. maxBound]

  -- TODO: make the user timezone configurable
  let userTimeZoneLabel = Europe__Zurich
  let userTimeZone = tzByLabel userTimeZoneLabel

  let userOptions :: [(LocalTime, NominalDiffTime)]
      userOptions =
        [ (LocalTime (fromGregorian 2023 04 20) (TimeOfDay 23 00 0), 60 * 15),
          (LocalTime (fromGregorian 2023 04 20) (TimeOfDay 23 15 0), 60 * 15),
          (LocalTime (fromGregorian 2023 04 20) (TimeOfDay 23 30 0), 60 * 15),
          (LocalTime (fromGregorian 2023 04 21) (TimeOfDay 23 00 0), 60 * 15),
          (LocalTime (fromGregorian 2023 04 21) (TimeOfDay 23 15 0), 60 * 15),
          (LocalTime (fromGregorian 2023 04 21) (TimeOfDay 23 30 0), 60 * 15)
        ]

  clientTimeZoneLabel <- getClientTZLabel
  let clientTimeZone = tzByLabel clientTimeZoneLabel

  let toUserLocalTime :: LocalTime -> LocalTime
      toUserLocalTime = utcToLocalTimeTZ clientTimeZone . localTimeToUTCTZ userTimeZone

  let clientOptions :: Map Day (Set (TimeOfDay, NominalDiffTime))
      clientOptions =
        M.fromListWith S.union $
          map
            (\(lt, dur) -> let LocalTime d tod = toUserLocalTime lt in (d, S.singleton (tod, dur)))
            userOptions

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user")

data BookForm = BookForm
  { bookFormUserTime :: !LocalTime,
    bookFormClientTime :: !LocalTime,
    bookFormDuratiion :: !NominalDiffTime
  }
  deriving (Show, Eq, Generic)

bookForm :: FormInput Handler BookForm
bookForm = error "TODO"

postBookUser :: Username -> Handler Html
postBookUser username = do
  bf@BookForm {..} <- runInputPost bookForm
  liftIO $ print bf
  redirect $ BookUserR username
