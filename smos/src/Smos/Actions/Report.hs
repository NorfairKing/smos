module Smos.Actions.Report where

import Smos.Actions.Report.Next
import Smos.Actions.Report.Timestamps
import Smos.Actions.Report.Waiting
import Smos.Types

allPlainReportActions :: [Action]
allPlainReportActions =
  concat
    [ allPlainReportNextActions,
      allPlainReportWaitingActions,
      allPlainReportTimestampsActions
    ]

allReportUsingActions :: [ActionUsing Char]
allReportUsingActions =
  concat
    [ allReportNextActionsUsingActions,
      allReportWaitingUsingActions,
      allReportTimestampsUsingActions
    ]
