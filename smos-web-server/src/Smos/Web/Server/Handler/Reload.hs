module Smos.Web.Server.Handler.Reload where

import Smos.Web.Server.Handler.Import
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR
