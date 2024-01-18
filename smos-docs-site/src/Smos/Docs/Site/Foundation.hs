{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Foundation
  ( module Smos.Docs.Site.Foundation,
    module Smos.Docs.Site.Assets,
    module Smos.Docs.Site.Casts,
    module Smos.Docs.Site.Static,
    module Smos.Docs.Site.Changelog,
    module Smos.Docs.Site.Widget,
    module Yesod,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Language.Haskell.TH.Load
import Smos.Docs.Site.Assets
import Smos.Docs.Site.Casts
import Smos.Docs.Site.Changelog
import Smos.Docs.Site.Constants
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Smos.Web.Style
import Text.Hamlet
import Yesod
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appWebserverUrl :: !(Maybe Text),
    appAssets :: !EmbeddedStatic,
    appCasts :: !EmbeddedStatic,
    appStyle :: !EmbeddedStatic,
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    app <- getYesod
    let addReloadWidget = if development then (<> autoReloadWidgetFor ReloadR) else id
    pageContent <-
      widgetToPageContent $ do
        addStylesheet $ StyleR index_css
        addStylesheet $ AssetsStaticR font_awesome_css
        toWidgetHead
          [hamlet|<link rel="icon" href=@{AssetsR ["logo.svg"]} sizes="16x16 32x32 48x48 64x84" type="image/x-icon">|]
        addScript $ AssetsStaticR asciinema_player_js
        addStylesheet $ AssetsStaticR asciinema_player_css
        let menu = $(widgetFile "menu")
        addReloadWidget $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  errorHandler NotFound = fmap toTypedContent $
    defaultLayout $
      do
        setTitle "Page not found"
        [whamlet|
      <h1>
        Page not found
      |]
  errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Smos Documentation"
  setDescriptionIdemp "Documentation for the Smos Self-Management Tool"
  mWebUrl <- getsYesod appWebserverUrl
  $(widgetFile "home")

getCastsR :: [Text] -> Handler Html
getCastsR t = do
  neverExpires
  redirect $ T.intercalate "/" $ "/casts-static/res" : t

getAssetsR :: [Text] -> Handler Html
getAssetsR t = do
  neverExpires
  redirect $ T.intercalate "/" $ "/assets-static/res" : t

lookupPage :: Text -> Handler DocPage
lookupPage url = do
  dps <- loadDocPages
  case M.lookup [url] dps of
    Nothing -> notFound
    Just dp -> pure dp

lookupPage' :: [Text] -> Handler DocPage
lookupPage' url = do
  dps <- loadDocPages
  case M.lookup url dps of
    Nothing -> notFound
    Just dp -> pure dp

lookupPagesIn :: [Text] -> Handler [([Text], DocPage)]
lookupPagesIn dir =
  mapMaybe (\(k, v) -> (,) <$> stripPrefix dir k <*> pure v)
    . M.toList
    . M.filterWithKey (\parts _ -> dir `isPrefixOf` parts)
    <$> loadDocPages

setSmosTitle :: (MonadWidget m) => Html -> m ()
setSmosTitle t = setTitle $ "Smos Documentation - " <> t

loadDocPages :: (MonadHandler m) => m (Map [Text] DocPage)
loadDocPages = loadIO docPages

yamlDesc :: forall a. (HasCodec a) => Text
yamlDesc = yamlDescVia (codec @a)

yamlDescVia :: forall a. JSONCodec a -> Text
yamlDescVia = renderPlainSchemaVia

confDocsWithKey :: forall o. (HasCodec o) => Text -> Text
confDocsWithKey key = yamlDescVia $ Autodocodec.object "Configuration" $ optionalFieldWith' key (codec @o)
