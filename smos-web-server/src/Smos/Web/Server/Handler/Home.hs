{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import qualified Data.Text as T
import Smos.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  mDocsUrl <- getsYesod appDocsBaseUrl
  mMonetisation <- runClientOrErr clientGetMonetisation
  let priceText m =
        T.unwords
          [ -- by 12 to get the price per month in cents, by 100 to get the price pr month in units
            T.pack (printf "%d" (monetisationStripePricePerYear m `div` 12 `div` 100)),
            T.toUpper (monetisationStripePriceCurrency m),
            "/",
            "Month"
          ]
  withNavBar $ do
    addScript $ StaticR bulma_carousel_js
    addStylesheet $ StaticR asciinema_player_css

    $(widgetFile "home")
