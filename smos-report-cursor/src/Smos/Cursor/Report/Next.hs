module Smos.Cursor.Report.Next where

import qualified Data.List.NonEmpty as NE

import Cursor.Simple.List.NonEmpty

import Smos.Data

import Smos.Report.Next

type NextActionReportCursor = NonEmptyCursor NextActionEntry

makeNextActionReportCursor :: [NextActionEntry] -> Maybe NextActionReportCursor
makeNextActionReportCursor = fmap makeNonEmptyCursor . NE.nonEmpty

nextActionReportCursorNext ::
       NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorNext = nonEmptyCursorSelectNext

nextActionReportCursorPrev ::
       NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorPrev = nonEmptyCursorSelectPrev
