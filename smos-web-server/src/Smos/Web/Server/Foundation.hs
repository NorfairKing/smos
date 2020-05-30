{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Web.Server.Foundation where

import Control.Monad.Logger
import Smos.Web.Server.Widget
import Text.Hamlet
import Yesod

data App
  = App
      { appLogLevel :: LogLevel
      }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    app <- getYesod
    pageContent <- widgetToPageContent $ do
      $(widgetFile "default-body")
    withUrlRenderer $ do
      $(hamletFile "templates/default-page.hamlet")
