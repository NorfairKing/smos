{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Servant.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Yesod hiding (Header)

getHomeR :: Handler Html
getHomeR = do
  mDocsUrl <- getsYesod appDocsBaseUrl
  withNavBar $ do
    addScript $ StaticR bulma_carousel_js
    addScript $ StaticR asciinema_player_js
    addStylesheet $ StaticR asciinema_player_css

    $(widgetFile "home")
