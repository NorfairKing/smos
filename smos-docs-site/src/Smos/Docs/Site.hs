{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site
  ( smosDocsSite
  ) where

import Hakyll
import Hakyll.Web.Sass

smosDocsSite :: IO ()
smosDocsSite =
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
        pandocCompiler >>= loadAndApplyTemplate "templates/page.html" pageCtx >>=
        loadAndApplyTemplate "templates/default.html" pageCtx >>=
        relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        pages <- recentFirst =<< loadAll "pages/*"
        let indexCtx =
              listField "pages" pageCtx (return pages) `mappend` constField "title" "Home" `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateCompiler

pageCtx :: Context String
pageCtx = defaultContext
