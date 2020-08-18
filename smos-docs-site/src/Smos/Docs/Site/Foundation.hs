{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Foundation
  ( module Smos.Docs.Site.Foundation,
    module Smos.Docs.Site.Static,
    module Smos.Docs.Site.Assets,
    module Smos.Docs.Site.Widget,
    module Yesod,
  )
where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Smos.Docs.Site.Assets
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Text.Hamlet
import Text.Read
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
        addScript $ AssetsStaticR asciinema_player_js
        addStylesheet $ AssetsStaticR asciinema_player_css
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

lookupPage :: Text -> Handler DocPage
lookupPage url =
  case find ((== url) . docPageUrl) docPages of
    Nothing -> notFound
    Just dp -> pure dp

-- Nice little hack to make this work.
instance Read DocPage where
  readPrec = do
    url <- readPrec
    case find ((== url) . docPageUrl) docPages of
      Nothing -> fail "No such page"
      Just dp -> pure dp

instance PathMultiPiece DocPage where
  fromPathMultiPiece url =
    find ((== T.intercalate "/" url) . docPageUrl) docPages
  toPathMultiPiece = T.splitOn "/" . docPageUrl
