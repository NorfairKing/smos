module Smos.Web.Server.Handler.Reload where

import Smos.Web.Server.Foundation
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR
