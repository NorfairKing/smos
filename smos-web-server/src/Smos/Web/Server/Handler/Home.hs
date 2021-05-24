{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  mDocsUrl <- getsYesod appDocsBaseUrl
  mMonetisation <- runClientOrErr clientGetMonetisation
  withNavBar $ do
    addScript $ StaticR bulma_carousel_js
    addScript $ StaticR asciinema_player_js
    addStylesheet $ StaticR asciinema_player_css

    $(widgetFile "home")
