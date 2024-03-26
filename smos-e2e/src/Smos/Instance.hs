module Smos.Instance where

import Conduit
import qualified Graphics.Vty as Vty (defaultConfig)
import qualified Graphics.Vty.Platform.Unix as Vty (mkVtyWithSettings)
import qualified Graphics.Vty.Platform.Unix.Settings as Vty (UnixSettings (..))
import Smos
import Smos.Terminal
import Smos.Types

withSmosInstance :: (MonadUnliftIO m) => SmosConfig -> Maybe StartingPath -> (TerminalHandle -> m a) -> m a
withSmosInstance config mStartingPath = withTerminal $ \_ slaveFd ->
  let vtyBuilder = do
        let vtyConfig = Vty.defaultConfig
        let unixSettings =
              Vty.UnixSettings
                { Vty.settingVmin = 1,
                  Vty.settingVtime = 100,
                  Vty.settingInputFd = slaveFd,
                  Vty.settingOutputFd = slaveFd,
                  Vty.settingTermName = "xterm-256color"
                }
        Vty.mkVtyWithSettings vtyConfig unixSettings
      runSmos :: (MonadIO m) => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
   in runSmos
