{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Smos.Docs.Site.Foundation where

import Data.Text (Text)
import qualified Data.Text as T
import Smos.Docs.Site.Widget
import Text.Hamlet
import Yesod
import Yesod.EmbeddedStatic

data App = App {appAssets :: EmbeddedStatic}

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    pageContent <-
      widgetToPageContent $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css"
        addStylesheetRemote
          "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
        toWidgetHead
          [hamlet|<link rel="icon" href="https://cs-syd.eu/logo/res/favicon.ico" sizes="32x32" type="image/x-icon">|]
        let menu = $(widgetFile "menu")
        $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Smos Documentation"
  $(widgetFile "home")

getAssetsR :: [Text] -> Handler Html
getAssetsR t = do
  neverExpires
  redirect $ T.intercalate "/" $ "/assets-static/res" : t
