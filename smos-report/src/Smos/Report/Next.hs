{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Smos.Data
import Smos.Report.Config
import Smos.Report.Path
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReport :: SmosReportConfig -> IO [NextActionEntry]
produceNextActionReport src = do
  wd <- resolveReportWorkflowDir src
  sourceToList $
    sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .| parseSmosFiles
      .| printShouldPrint PrintWarning
      .| smosFileEntries
      .| C.filter (isNextAction . snd)
      .| C.map (uncurry makeNextActionEntry)

isNextAction :: Entry -> Bool
isNextAction = maybe False isNextTodoState . entryState

isNextTodoState :: TodoState -> Bool
isNextTodoState = (`elem` mapMaybe todoState ["NEXT", "STARTED"])

makeNextActionReport :: [(RootedPath, Entry)] -> NextActionReport
makeNextActionReport = NextActionReport . map (uncurry makeNextActionEntry)

makeNextActionEntry :: RootedPath -> Entry -> NextActionEntry
makeNextActionEntry rf e =
  NextActionEntry
    { nextActionEntryTodoState = entryState e,
      nextActionEntryHeader = entryHeader e,
      nextActionEntryFilePath = rf
    }

newtype NextActionReport
  = NextActionReport
      { nextActionReportEntries :: [NextActionEntry]
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionReport

data NextActionEntry
  = NextActionEntry
      { nextActionEntryTodoState :: Maybe TodoState,
        nextActionEntryHeader :: Header,
        nextActionEntryFilePath :: RootedPath
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntry
