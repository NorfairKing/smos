{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Help
    ( helpUp
    , helpDown
    , helpStart
    , helpEnd
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

helpStart :: Action
helpStart =
    Action
        { actionName = "helpStart"
        , actionDescription = "Scroll to the start of the screen"
        , actionFunc = modifyHelpCursor helpCursorStart
        }

helpEnd :: Action
helpEnd =
    Action
        { actionName = "helpEnd"
        , actionDescription = "Scroll to the end of the help screen"
        , actionFunc = modifyHelpCursor helpCursorEnd
        }
