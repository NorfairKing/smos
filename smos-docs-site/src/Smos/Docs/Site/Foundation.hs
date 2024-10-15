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
    module Smos.Docs.Site.Static,
    module Smos.Docs.Site.Changelog,
    module Smos.Docs.Site.Widget,
    module Smos.Web.Assets,
    module Yesod,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Language.Haskell.TH.Load
import qualified OptEnvConf
import qualified OptEnvConf.Args as OptEnvConf
import Smos.Docs.Site.Assets
import Smos.Docs.Site.Changelog
import Smos.Docs.Site.Constants
import Smos.Docs.Site.Static
import Smos.Docs.Site.Widget
import Smos.Web.Assets
import Text.Colour
import Text.Hamlet
import Yesod
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appWebserverUrl :: !(Maybe Text),
    appAssets :: !EmbeddedStatic,
    appWebAssets :: !EmbeddedStatic,
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
        addStylesheet $ WebAssetsStaticR index_css
        addStylesheet $ AssetsStaticR font_awesome_css
        toWidgetHead
          [hamlet|<link rel="icon" href=@{AssetsR ["logo.svg"]} sizes="16x16 32x32 48x48 64x84" type="image/x-icon">|]
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

getWebAssetsR :: [Text] -> Handler Html
getWebAssetsR t = do
  neverExpires
  redirect $ T.intercalate "/" $ "/web-assets-static/res" : t

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

makeSettingsPage :: forall a. (OptEnvConf.HasParser a) => String -> Handler Html
makeSettingsPage progname = do
  DocPage {..} <- lookupPage $ T.pack progname
  defaultLayout $ do
    let docs = OptEnvConf.parserDocs (OptEnvConf.settingsParser :: OptEnvConf.Parser a)
    let docsChunks = OptEnvConf.renderReferenceDocumentation progname docs
    let referenceDocs = renderChunksText WithoutColours docsChunks
    setSmosTitle $ toHtml docPageTitle
    setDescriptionIdemp docPageDescription
    $(widgetFile "settings")

makeCommandSettingsPage :: forall a. (OptEnvConf.HasParser a) => String -> Text -> Handler Html
makeCommandSettingsPage progname command = do
  DocPage {..} <- lookupPage' [T.pack progname, command]

  errOrHelpDoc <-
    liftIO $
      OptEnvConf.runHelpParser
        Nothing
        (OptEnvConf.parseArgs [T.unpack command])
        (OptEnvConf.settingsParser :: OptEnvConf.Parser a)
  case errOrHelpDoc of
    Left err -> error $ show err -- Will be caught by yesod
    Right Nothing -> error "Command not found"
    Right (Just (path, cDoc)) -> do
      let docsChunks = OptEnvConf.renderCommandHelpPage progname path cDoc

      defaultLayout $ do
        let referenceDocs = renderChunksText WithoutColours docsChunks
        setSmosTitle $ toHtml docPageTitle
        setDescriptionIdemp docPageDescription
        $(widgetFile "settings")
