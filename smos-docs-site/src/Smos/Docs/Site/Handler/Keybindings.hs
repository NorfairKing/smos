{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Keybindings
  ( getSmosKeybindingsR,
  )
where

import Smos.Default
import Smos.Docs.Site.Foundation
import Smos.Types

getSmosKeybindingsR :: Handler Html
getSmosKeybindingsR =
  defaultLayout
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
