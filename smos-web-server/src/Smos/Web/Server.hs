module Smos.Web.Server where

import Control.Concurrent.Async
import qualified Smos.Server.Serve as API
import Smos.Web.Server.OptParse
import Smos.Web.Server.Serve
import Text.Show.Pretty

smosWebServer :: IO ()
smosWebServer = do
  Instructions dispatch Settings <- getInstructions
  case dispatch of
    DispatchServe ss -> runBoth ss

runBoth :: ServeSettings -> IO ()
runBoth ss = do
  pPrint ss
  race_ (API.runSmosServer (serveSetAPISettings ss)) (runSmosWebServer ss)
