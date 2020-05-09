{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Application where

import Smos.Docs.Site.Foundation
import Smos.Docs.Site.Handler

mkYesodDispatch "App" resourcesApp
