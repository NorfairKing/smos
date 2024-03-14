module Smos.Instance where

import Conduit
import qualified Graphics.Vty.CrossPlatform as Vty (mkVty)
import qualified Graphics.Vty as Vty (defaultConfig)
-- import qualified Graphics.Vty.Platform.Unix.Settings  as Vty(settingInputFd,settingOutputFd,defaultSettings) --
-- import Graphics.Vty.Platform.Unix.Output.TerminfoBased  (setWindowSize)
import Smos
import Smos.Terminal
import Smos.Types

{-


withSmosInstance :: (MonadUnliftIO m) => SmosConfig -> Maybe StartingPath -> (TerminalHandle -> m a) -> m a
withSmosInstance config mStartingPath = withTerminal $ \_ slaveFd ->
  let vtyBuilder = do

        vty <-
          Vty.mkVty $
          Vty.defaultConfig
              -- { Vty.settingInputFd =  slaveFd,
              --   Vty.settingOutputFd =  slaveFd
              -- }
        setWindowSize slaveFd (80, 24)
        pure vty
      runSmos :: (MonadIO m) => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
   in runSmos

-}