{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Hakyll
import           Hakyll.Web.Sass
import           Smos.Actions
import           Smos.Default
import           Smos.Keys
import           Smos.Types
import           System.Directory
import           System.IO

smosDocsSite :: IO ()
smosDocsSite = do
  createDirectoryIfMissing True "_generated"
  generateKeybindingDocs "Help"
  hakyll $ do
    match "assets/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route $ setExtension "css"
      compile (sassCompilerWith sassDefConfig {sassIncludePaths = Just ["_sass"]})
    match "pages/*" $ do
      route $ composeRoutes (gsubRoute "pages/" (const "")) (setExtension "html")
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls
    match "_generated/*" $ do
      route $ composeRoutes (gsubRoute "_generated/" (const "")) (setExtension "html")
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/page.html" pageCtx
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        pages <- recentFirst =<< loadAll "pages/*"
        let indexCtx =
              listField "pages" pageCtx (return pages) `mappend` constField "title" "Home"
                `mappend` defaultContext
        getResourceBody >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
    match "templates/*" $ compile templateCompiler

pageCtx :: Context String
pageCtx = defaultContext


renderKeyMapping :: KeyMapping -> (Text, Text, Text)
renderKeyMapping keyMapping =
  case keyMapping of
    (MapVtyExactly kp act) -> (renderKeyPress kp, actionNameText $ actionName act, actionDescription act)
    (MapAnyTypeableChar au) -> ("??", actionNameText $ actionUsingName au, actionUsingDescription au)
    (MapCatchAll act) -> ("any char", actionNameText $ actionName act, actionDescription act)
    (MapCombination kp km) ->
      let (kp', cmd, desc) =  renderKeyMapping km
      in (renderKeyPress kp <> kp', cmd, desc)

generateKeybindingDocs :: Text -> IO ()
generateKeybindingDocs label =
  withFile "_generated/keybindings.markdown" WriteMode $ \h -> do
  T.hPutStrLn h "---"
  T.hPutStrLn h "title: Key Bindings"
  T.hPutStrLn h "---"
  T.hPutStrLn h ""
  T.hPutStrLn h $ "## " <> label
  T.hPutStrLn h "Key | Command | Description"
  T.hPutStrLn h "---:+:-------:+:-----------:"
  forM_ (keyMapHelpMatchers $ configKeyMap defaultConfig) $ \keyMapping ->
      T.hPutStrLn h $ renderRow $ renderKeyMapping keyMapping

  where
    renderRow :: (Text, Text, Text) -> Text
    renderRow (kb, cmd, desc) = (T.pack $ escapeHtml $ T.unpack kb) <> " | " <> cmd <> " | " <> desc
