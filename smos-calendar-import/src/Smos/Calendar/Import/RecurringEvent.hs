{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.RecurringEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Validity
import GHC.Generics
import ICal.Extended ()
import qualified ICal.Property as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static
import Text.Read

newtype RecurringEvents = RecurringEvents {recurringEvents :: Map ICal.UID (Set RecurringEvent)}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RecurringEvents)

instance Validity RecurringEvents

-- TODO validity constraints on timezone ids

instance HasCodec RecurringEvents where
  codec = dimapCodec RecurringEvents recurringEvents eventsCodec
    where
      eventsCodec :: JSONCodec (Map ICal.UID (Set RecurringEvent))
      eventsCodec = dimapCodec f2 g2 $ eitherCodec codec codec
        where
          f2 ::
            Either (Map ICal.UID (Set RecurringEvent)) [RecurringEvent] ->
            Map ICal.UID (Set RecurringEvent)
          f2 = \case
            Left m -> m
            Right is -> M.fromList $ zipWith (\i e -> (ICal.UID (T.pack (show (i :: Word))), S.singleton e)) [0 ..] is
          g2 ::
            Map ICal.UID (Set RecurringEvent) ->
            Either (Map ICal.UID (Set RecurringEvent)) [RecurringEvent]
          g2 m =
            let tups = M.toList m
                mSingles =
                  map
                    ( \(uid, s) -> case S.toList s of
                        [v] -> case (readMaybe :: String -> Maybe Word) (T.unpack (ICal.unUID uid)) of
                          Just _ -> Just v
                          Nothing -> Nothing
                        _ -> Nothing
                    )
                    tups
             in if all isJust mSingles
                  then Right $ catMaybes mSingles
                  else Left m

data RecurringEvent = RecurringEvent
  { recurringEventStatic :: !Static,
    recurringEventEvent :: !ICal.RecurringEvent
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurringEvent

instance HasCodec RecurringEvent where
  codec =
    named "RecurringEvent" $
      object "RecurringEvent" $
        RecurringEvent
          <$> objectCodec .= recurringEventStatic
          <*> objectCodec .= recurringEventEvent
