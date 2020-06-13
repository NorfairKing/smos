{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Agenda where

import Data.Text (Text)
import Path
import Smos.Client
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.TimeBlock
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod

getReportAgendaR :: Handler Html
getReportAgendaR = withLogin $ \t -> do
  report <- runClientOrErr $ clientGetAgendaReport t
  defaultLayout $(widgetFile "agenda")

agendaReportWidget :: AgendaReport -> Widget
agendaReportWidget AgendaReport {..} =
  [whamlet|
    <h2>
      Past

    ^{agendaTableBlocksWidget agendaReportPast}


    <h2>
      Present
    ^{agendaTodayWidget agendaReportPresent}


    <h2>
      Future

    ^{agendaTableBlocksWidget agendaReportFuture}
  |]

agendaTableBlocksWidget :: [AgendaTableBlock Text] -> Widget
agendaTableBlocksWidget = \case
  [] -> mempty
  [b] -> agendaEntriesTableWidget (blockEntries b)
  bs -> mconcat $ map agendaTableBlockWidget bs

agendaTableBlockWidget :: AgendaTableBlock Text -> Widget
agendaTableBlockWidget Block {..} =
  [whamlet|
    <h3>
      #{blockTitle}
    ^{agendaEntriesTableWidget blockEntries}
  |]

agendaTodayWidget :: AgendaTodayReport -> Widget
agendaTodayWidget = agendaEntriesTableWidget . agendaTodayReportEntries

agendaEntriesTableWidget :: [AgendaEntry] -> Widget
agendaEntriesTableWidget es =
  [whamlet|
    <table>
      <tr>
        <th> File
        <th> Header
        <th> State
        <th> TimestampName
        <th> Timestamp
      ^{agendaEntriesWidget es}
  |]

agendaEntriesWidget :: [AgendaEntry] -> Widget
agendaEntriesWidget = mconcat . map agendaEntryWidget

agendaEntryWidget :: AgendaEntry -> Widget
agendaEntryWidget AgendaEntry {..} =
  [whamlet|
     <tr>
      <td>
        <a href=@{fileR agendaEntryFilePath}>
          #{fromRelFile agendaEntryFilePath}
      <td>
        #{headerText agendaEntryHeader}
      <td>
        #{maybe "" todoStateText agendaEntryTodoState}
      <td>
        #{timestampNameText agendaEntryTimestampName}
      <td>
        #{timestampPrettyText agendaEntryTimestamp}
  |]
