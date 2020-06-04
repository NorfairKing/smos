{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Next where

import Conduit
import Data.Aeson
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
import YamlParse.Applicative

produceNextActionReport :: SmosReportConfig -> IO NextActionReport
produceNextActionReport src = do
  wd <- resolveReportWorkflowDir src
  runConduit $
    sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .| parseSmosFiles
      .| printShouldPrint PrintWarning
      .| nextActionReportConduit

nextActionReportConduit :: Monad m => ConduitT (RootedPath, SmosFile) o m NextActionReport
nextActionReportConduit =
  NextActionReport
    <$> ( smosFileEntries
            .| C.filter (isNextAction . snd)
            .| C.map (uncurry makeNextActionEntry)
            .| sinkList
        )

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
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance Validity NextActionReport

instance FromJSON NextActionReport where
  parseJSON = viaYamlSchema

instance YamlSchema NextActionReport where
  yamlSchema = NextActionReport <$> yamlSchema

instance ToJSON NextActionReport where
  toJSON = toJSON . nextActionReportEntries

data NextActionEntry
  = NextActionEntry
      { nextActionEntryTodoState :: Maybe TodoState,
        nextActionEntryHeader :: Header,
        nextActionEntryFilePath :: RootedPath
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntry

instance FromJSON NextActionEntry where
  parseJSON = viaYamlSchema

instance YamlSchema NextActionEntry where
  yamlSchema = objectParser "NextActionEntry" $ NextActionEntry <$> requiredField "state" "The TODO state of the entry" <*> requiredField "header" "The header of the entry" <*> requiredField "path" "The path of the file in which this entry was found"

instance ToJSON NextActionEntry where
  toJSON NextActionEntry {..} = object ["state" .= nextActionEntryTodoState, "header" .= nextActionEntryHeader, "path" .= nextActionEntryFilePath]
