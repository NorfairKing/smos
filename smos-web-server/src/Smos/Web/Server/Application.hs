{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Web.Server.Application where

import Smos.Web.Server.Foundation
import Smos.Web.Server.Handler
import Yesod

mkYesodDispatch "App" resourcesApp
