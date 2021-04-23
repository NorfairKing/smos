{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Web.Server.Application where

import Smos.Web.Server.Foundation
import Smos.Web.Server.Handler

mkYesodDispatch "App" resourcesApp
