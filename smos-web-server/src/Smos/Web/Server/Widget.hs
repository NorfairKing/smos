module Smos.Web.Server.Widget where

import Data.Default
import Language.Haskell.TH.Syntax (Exp, Q)
import Smos.Web.Server.Constants
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFile :: String -> Q Exp
widgetFile =
  if development
    then widgetFileReload widgetFileSettings
    else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
