{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.TimeBlock where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Function
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Yaml.Builder (ToYaml(..))
import qualified Data.Yaml.Builder as Yaml

data TimeBlock
  = OneBlock
  | DayBlock
  deriving (Show, Eq, Generic)

instance Validity TimeBlock

data Block a b =
  Block
    { blockTitle :: a
    , blockEntries :: [b]
    }
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (Block a b)

instance (ToJSON a, ToJSON b) => ToJSON (Block a b) where
  toJSON Block {..} = object ["title" .= blockTitle, "entries" .= blockEntries]

instance (ToYaml a, ToYaml b) => ToYaml (Block a b) where
  toYaml Block {..} = Yaml.mapping [("title", toYaml blockTitle), ("entries", toYaml blockEntries)]

mapBlockTitle :: (a -> b) -> Block a c -> Block b c
mapBlockTitle func b = b {blockTitle = func $ blockTitle b}

mapBlockEntries :: ([b] -> [c]) -> Block a b -> Block a c
mapBlockEntries func b = b {blockEntries = func $ blockEntries b}

divideIntoBlocks :: (b -> Day) -> TimeBlock -> [b] -> [Block Text b]
divideIntoBlocks func tb es =
  case tb of
    OneBlock -> [Block {blockTitle = "All Time", blockEntries = es}]
    DayBlock -> map (mapBlockTitle (T.pack . show)) $ divideIntoDayBlocks func es

divideIntoDayBlocks :: (b -> Day) -> [b] -> [Block Day b]
divideIntoDayBlocks func =
  sortOn blockTitle . combineBlocksByName . map (turnIntoSingletonBlock func)

turnIntoSingletonBlock :: (b -> Day) -> b -> Block Day b
turnIntoSingletonBlock func b = Block {blockTitle = func b, blockEntries = [b]}

combineBlocksByName :: Ord a => [Block a b] -> [Block a b]
combineBlocksByName = map comb . groupBy ((==) `on` blockTitle) . sortOn blockTitle
  where
    comb :: [Block a b] -> Block a b
    comb [] = error "cannot happen due to 'groupBy' above"
    comb bs@(b:_) = Block {blockTitle = blockTitle b, blockEntries = concatMap blockEntries bs}
