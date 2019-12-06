{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Properties.Gen where

import Data.GenValidity

import Cursor.List.NonEmpty
import Cursor.Map
import Cursor.Map.Gen ()
import Cursor.Text.Gen

import Control.Monad

import Test.QuickCheck

import Smos.Data.Gen

import Smos.Cursor.Properties

instance GenValid PropertiesCursor where
  genValid = do
    let tupGen = (,) <$> genValid <*> genValid
        curGen =
          oneof
            [ do tc <- textCursorWithGen genPropertyNameChar
                 KeyValueCursorKey <$> pure tc <*> genValid
            , do tc <- textCursorWithGen genPropertyValueChar
                 KeyValueCursorValue <$> genValid <*> pure tc
            ]
    ne <-
      sized $ \n -> do
        part <- arbPartition n
        case part of
          [] -> singletonNonEmptyCursor <$> resize 0 genValid
          (s:ss) -> do
            i <- choose (0, length ss)
            let (as, bs) = splitAt i ss
            nonEmptyCursorPrev <- forM as $ \s_ -> resize s_ tupGen
            nonEmptyCursorCurrent <- resize s curGen
            nonEmptyCursorNext <- forM bs $ \s_ -> resize s_ tupGen
            pure NonEmptyCursor {..}
    pure $ PropertiesCursor {propertiesCursorMapCursor = MapCursor {mapCursorList = ne}}
  shrinkValid = shrinkValidStructurally
