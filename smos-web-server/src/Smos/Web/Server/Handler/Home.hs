{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Static
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod hiding (Header)

getHomeR :: Handler Html
getHomeR =
  withNavBar $ do
    addScriptRemote "https://cdn.jsdelivr.net/npm/bulma-carousel@4.0.3/dist/js/bulma-carousel.min.js"
    $(widgetFile "home")