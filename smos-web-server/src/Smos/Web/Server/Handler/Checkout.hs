module Smos.Web.Server.Handler.Checkout where

import Control.Monad.Logger
import Smos.Web.Server.Handler.Import

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
