{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Help
  ( helpInsert
  , helpAppend
  , helpRemove
  , helpDelete
  , helpUp
  , helpDown
  , helpStart
  , helpEnd
  ) where

import Smos.Types

import Smos.Actions.Utils

helpInsert :: ActionUsing Char
helpInsert =
  ActionUsing
    { actionUsingName = "helpInsert"
    , actionUsingDescription = "insert a character into the help search bar"
    , actionUsingFunc = \a -> modifyHelpCursorM $ helpCursorInsert a
    }

helpAppend :: ActionUsing Char
helpAppend =
  ActionUsing
    { actionUsingName = "helpAppend"
    , actionUsingDescription = "insert a character into the help search bar"
    , actionUsingFunc = \a -> modifyHelpCursorM $ helpCursorAppend a
    }

helpRemove :: Action
helpRemove =
  Action
    { actionName = "helpRemove"
    , actionDescription = "remove a character from the help search bar"
    , actionFunc = modifyHelpCursorM helpCursorRemove
    }

helpDelete :: Action
helpDelete =
  Action
    { actionName = "helpDelete"
    , actionDescription = "remove a character from the help search bar"
    , actionFunc = modifyHelpCursorM helpCursorDelete
    }

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
