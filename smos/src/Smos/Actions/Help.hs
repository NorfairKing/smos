{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Help
    ( helpUp
    , helpDown
    ) where

import Smos.Types

import Smos.Actions.Utils

helpUp :: Action
helpUp =
    Action
        { actionName = "helpUp"
        , actionDescription = "Scroll up in the help screen"
        , actionFunc = modifyHelpCursorM helpCursorUp
        }

helpDown :: Action
helpDown =
    Action
        { actionName = "helpDown"
        , actionDescription = "Scroll down in the help screen"
        , actionFunc = modifyHelpCursorM helpCursorDown
        }
