{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Actions
  ( getSmosActionsR,
  )
where

import Data.List
import Smos.Actions
import Smos.Docs.Site.Foundation
import Smos.Types

getSmosActionsR :: Handler Html
getSmosActionsR =
  defaultLayout
    $(widgetFile "smos-actions")

actionTable :: [AnyAction] -> Widget
actionTable aas =
  let ws = map actionRow aas
   in [whamlet|
    <table .is-bordered .is-striped .is-fullwidth>
      <colgroup>
        <col span="1" style="width: 15%;">
        <col span="1" style="width: 85%;">
      <thead>
        <tr>
          <th>
            Name
          <th>
            Description
      <tbody>
        $forall w <- ws
          ^{w}
  |]

actionRow :: AnyAction -> Widget
actionRow aa =
  [whamlet|
    <tr>
      <td>
        <code>
          #{actionNameText $ anyActionName aa}
      <td>
        #{anyActionDescription aa}
  |]
