{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Help
  ( allHelpPlainActions,
    allHelpUsingCharActions,
    selectHelp,
    helpUp,
    helpDown,
    helpStart,
    helpEnd,
    helpSelectSearch,
    helpInsert,
    helpAppend,
    helpRemove,
    helpDelete,
    helpSelectHelp,
    helpToggleSelection,
    exitHelp,
  )
where

import Smos.Actions.Browser
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Types

allHelpPlainActions :: [Action]
allHelpPlainActions =
  [ selectHelp,
    helpUp,
    helpDown,
    helpStart,
    helpEnd,
    helpSelectSearch,
    helpRemove,
    helpDelete,
    helpSelectHelp,
    helpToggleSelection,
    exitHelp
  ]

allHelpUsingCharActions :: [ActionUsing Char]
allHelpUsingCharActions = [helpInsert, helpAppend]

selectHelp :: Action
selectHelp =
  Action
    { actionName = "selectHelp",
      actionFunc = modifyEditorCursorS $ \ec -> do
        km <- asks configKeyMap
        pure $ editorCursorSwitchToHelp km ec,
      actionDescription = "Show the (contextual) help screen"
    }

helpUp :: Action
helpUp =
  Action
    { actionName = "helpUp",
      actionDescription = "Scroll up in the help screen",
      actionFunc = modifyHelpCursorM helpCursorUp
    }

helpDown :: Action
helpDown =
  Action
    { actionName = "helpDown",
      actionDescription = "Scroll down in the help screen",
      actionFunc = modifyHelpCursorM helpCursorDown
    }

helpStart :: Action
helpStart =
  Action
    { actionName = "helpStart",
      actionDescription = "Scroll to the start of the screen",
      actionFunc = modifyHelpCursor helpCursorStart
    }

helpEnd :: Action
helpEnd =
  Action
    { actionName = "helpEnd",
      actionDescription = "Scroll to the end of the help screen",
      actionFunc = modifyHelpCursor helpCursorEnd
    }

helpSelectSearch :: Action
helpSelectSearch =
  Action
    { actionName = "helpSelectSearch",
      actionDescription = "Select the help search bar",
      actionFunc = modifyHelpCursorM helpCursorSelectSearch
    }

helpInsert :: ActionUsing Char
helpInsert =
  ActionUsing
    { actionUsingName = "helpInsert",
      actionUsingDescription = "insert a character into the help search bar",
      actionUsingFunc = \a -> modifyHelpCursorM $ helpCursorInsert a
    }

helpAppend :: ActionUsing Char
helpAppend =
  ActionUsing
    { actionUsingName = "helpAppend",
      actionUsingDescription = "insert a character into the help search bar",
      actionUsingFunc = \a -> modifyHelpCursorM $ helpCursorAppend a
    }

helpRemove :: Action
helpRemove =
  Action
    { actionName = "helpRemove",
      actionDescription = "remove a character from the help search bar",
      actionFunc = modifyHelpCursorM helpCursorRemove
    }

helpDelete :: Action
helpDelete =
  Action
    { actionName = "helpDelete",
      actionDescription = "remove a character from the help search bar",
      actionFunc = modifyHelpCursorM helpCursorDelete
    }

helpSelectHelp :: Action
helpSelectHelp =
  Action
    { actionName = "helpSelectHelp",
      actionDescription = "Deselect the help search bar",
      actionFunc = modifyHelpCursorM helpCursorSelectHelp
    }

helpToggleSelection :: Action
helpToggleSelection =
  Action
    { actionName = "helpToggleSelection",
      actionDescription = "Toggle between selecting and deselecting the search bar",
      actionFunc = modifyHelpCursor helpCursorToggleSelection
    }

exitHelp :: Action
exitHelp =
  Action
    { actionName = "exitHelp",
      actionDescription = "Exit the help screen",
      actionFunc = do
        ec <- gets smosStateCursor
        if editorCursorSelection ec == HelpSelected
          then case editorCursorFileCursor ec of
            Just _ -> modifyEditorCursor $ editorCursorSelect FileSelected
            Nothing -> case editorCursorLastOpenedFile ec of
              Just fp -> switchToFile fp
              Nothing -> case editorCursorBrowserCursor ec of
                Just _ -> modifyEditorCursor $ editorCursorSelect BrowserSelected
                Nothing -> case editorCursorReportCursor ec of
                  Just _ -> modifyEditorCursor $ editorCursorSelect ReportSelected
                  Nothing -> actionFunc selectBrowserWorkflow
          else pure ()
    }
