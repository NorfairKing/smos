{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Web.Server.Foundation where

import Yesod

data App
  = App
      {
      }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App
