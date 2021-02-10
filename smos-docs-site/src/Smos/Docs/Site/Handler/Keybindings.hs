{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Docs.Site.Handler.Keybindings
  ( getSmosKeybindingsR,
  )
where

import Smos.Default
import Smos.Docs.Site.Foundation
import Smos.Types

getSmosKeybindingsR :: Handler Html
getSmosKeybindingsR = do
  -- Just to force a compilation error if we forget to add new bindings here.
  let KeyMap _ _ _ _ _ = undefined
      FileKeyMap _ _ _ _ _ _ _ _ _ _ = undefined
      BrowserKeyMap _ _ _ _ _ = undefined
      ReportsKeyMap _ _ _ _ _ _ = undefined
      NextActionReportKeyMap _ _ _ = undefined
      WaitingReportKeyMap _ _ _ = undefined
      TimestampsReportKeyMap _ _ _ = undefined
      StuckReportKeyMap _ _ = undefined
      HelpKeyMap _ _ _ = undefined
  defaultLayout $ do
    setSmosTitle "Default Keybindings"
    setDescription "The default keybindings for the Smos TUI"
    $(widgetFile "smos-keybindings")

keyMapTable :: KeyMappings -> Widget
keyMapTable = keyHelpCursorsTable . makeKeyHelpCursors

keyHelpCursorsTable :: [KeyHelpCursor] -> Widget
keyHelpCursorsTable khs =
  let ws = map keyHelpCursorRow khs
   in [whamlet|
    <table .is-bordered .is-striped .is-fullwidth>
      <colgroup>
        <col span="1" style="width: 15%;">
        <col span="1" style="width: 15%;">
        <col span="1" style="width: 70%;">
      <thead>
        <tr>
          <th>
            Key combination
          <th>
            Action name
          <th>
            Description
      <tbody>
        $forall w <- ws
          ^{w}
  |]

keyHelpCursorRow :: KeyHelpCursor -> Widget
keyHelpCursorRow KeyHelpCursor {..} =
  [whamlet|
    <tr>
      <td>
        $forall kc <- keyHelpCursorKeyBinding
          <code>
            #{renderKeyCombination kc}
      <td>
        <code>
          #{actionNameText keyHelpCursorName}
      <td>
        #{keyHelpCursorDescription}

  |]
