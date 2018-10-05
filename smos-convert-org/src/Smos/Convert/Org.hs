{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.Org
    ( convertOrg
    ) where

import GHC.Generics (Generic)

import Control.Arrow
import Control.Monad
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as SB
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.OrgMode.Parse as Org (parseDocument)
import Data.OrgMode.Types as Org
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Validity
import Path
import System.Exit

import Smos.Data as Smos

import Smos.Convert.Org.OptParse
import Smos.Convert.Org.OptParse.Types

convertOrg :: IO ()
convertOrg = do
    sets@Settings {..} <- getSettings
    print sets
    t <- T.readFile $ fromAbsFile setFromFile
    let errOrDocument =
            Attoparsec.parseOnly
                (Org.parseDocument
                     ["TODO", "NEXT", "WAITING", "READY", "CANCELLED", "DONE"])
                t
    case errOrDocument of
        Left err ->
            die $
            unlines
                [ "Failed to parse orgmode document"
                , fromAbsFile setFromFile
                , "with error"
                , err
                ]
        Right doc ->
            case convertDocument doc of
                Left err -> die $ unlines ["Failed to convert:", show err]
                Right sf ->
                    case prettyValidation sf of
                        Left err ->
                            die $
                            unlines
                                ["The converted smos file was not valid:", err]
                        Right sf' ->
                            case setToFile of
                                Nothing -> SB.putStr $ smosFileYamlBS sf'
                                Just p -> writeSmosFile p sf'

type Convert = Either ConvertErr

data ConvertErr
    = InvalidHeader String
    | InvalidContents String
    | InvalidTimestampName String
    | InvalidTimestamp String
    | InvalidTodoState String
    | InvalidTag String
    | InvalidLogbook String
    | InvalidPropertyName String
    | InvalidPropertyValue String
    | InvalidUTCTime String
    | InvalidDay String
    deriving (Show, Eq, Generic)

-- | Convert an org-mode document to a smos file
--
-- TODO:
--
-- * Deal with priority
-- * Deal with state history
-- * Deal with drawers
convertDocument :: Org.Document -> Convert SmosFile
convertDocument Document {..} =
    SmosFile <$> mapM convertHeadline documentHeadlines

convertHeadline :: Org.Headline -> Convert (Tree Smos.Entry)
convertHeadline h = do
    entryHeader <- left InvalidHeader $ Smos.parseHeader $ Org.title h
    let s = Org.section h
    entryContents <-
        case Org.sectionParagraph s of
            "" -> pure Nothing
            t -> fmap Just $ left InvalidContents $ Smos.parseContents t
    entryTimestamps <- convertPlannings $ Org.sectionPlannings s
    entryProperties <- convertProperties $ Org.sectionProperties s
    entryStateHistory <- convertStateHistory $ Org.stateKeyword h
    entryTags <- mapM convertTag $ Org.tags h
    entryLogbook <- convertLogbook $ Org.sectionLogbook s
    let e = Entry {..}
    subForest <- mapM convertHeadline $ Org.subHeadlines h
    pure $ Node e subForest

convertPlannings ::
       Org.Plannings -> Convert (Map Smos.TimestampName Smos.Timestamp)
convertPlannings (Plns hm) =
    fmap M.fromList $
    forM (HM.toList hm) $ \(kw, ots) -> do
        tsn <- left InvalidTimestampName $ parseTimestampName $ T.pack $ show kw
        ts <- fmap Smos.Timestamp $ constructDay $ yearMonthDay $ tsTime ots
        pure (tsn, ts)

convertProperties ::
       Org.Properties -> Convert (Map Smos.PropertyName Smos.PropertyValue)
convertProperties ps =
    fmap M.fromList $
    forM (HM.toList $ unProperties ps) $ \(kt, vt) -> do
        k <- left InvalidPropertyName $ parsePropertyName kt
        v <- left InvalidPropertyValue $ parsePropertyValue vt
        pure (k, v)

convertStateHistory :: Maybe Org.StateKeyword -> Convert Smos.StateHistory
convertStateHistory mkw = do
    mts <-
        forM mkw $ \kw ->
            left InvalidTodoState $ parseTodoState $ unStateKeyword kw
    pure $
        StateHistory
            [ StateHistoryEntry
                  { stateHistoryEntryNewState = mts
                  , stateHistoryEntryTimestamp =
                        UTCTime (fromGregorian 1970 1 1) 0 -- TODO what timestamp should we use?
                  }
            ]

convertTag :: Org.Tag -> Convert Smos.Tag
convertTag = left InvalidTag . parseTag

convertLogbook :: Org.Logbook -> Convert Smos.Logbook
convertLogbook (Org.Logbook cs) = do
    let mtss = map (fst . unClock) cs
        tups = map (\ts -> (tsTime ts, tsEndTime ts)) $ catMaybes mtss
    constructLogbook $
        sortOn ((\dt -> Down (yearMonthDay dt, hourMinute dt)) . fst) tups
  where
    constructLogbook ::
           [(Org.DateTime, Maybe Org.DateTime)] -> Convert Smos.Logbook
    constructLogbook [] = pure emptyLogbook
    constructLogbook ((begin, mend):rest) = do
        lb <- constructLogbook rest
        case lb of
            LogOpen _ _ ->
                Left $ InvalidLogbook "Cannot prepend to an open logbook."
            LogClosed es ->
                case mend of
                    Nothing -> do
                        beginT <- constructUTCTime begin
                        pure $ LogOpen beginT es
                    Just end -> do
                        beginT <- constructUTCTime begin
                        endT <- constructUTCTime end
                        let lbe =
                                LogbookEntry
                                    { logbookEntryStart = beginT
                                    , logbookEntryEnd = endT
                                    }
                        pure $ LogClosed $ lbe : es
      where
        constructUTCTime :: DateTime -> Convert UTCTime
        constructUTCTime DateTime {..} = do
            day <- constructDay yearMonthDay
            dt <- maybe (pure 0) constructDiffTime hourMinute
            left InvalidUTCTime $
                prettyValidation $ UTCTime {utctDay = day, utctDayTime = dt}
          where
            constructDiffTime :: (Int, Int) -> Convert DiffTime
            constructDiffTime (h, m) =
                pure $
                secondsToDiffTime $
                60 * (fromIntegral m + (60 * fromIntegral h))

constructDay :: YearMonthDay -> Convert Day
constructDay YearMonthDay {..} =
    left InvalidDay $
    prettyValidation $ fromGregorian (fromIntegral ymdYear) ymdMonth ymdDay
