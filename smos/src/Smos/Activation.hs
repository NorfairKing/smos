{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Activation
    ( findActivations
    , Activation(..)
    ) where

import Import

import Data.Ord as Ord
import qualified Data.Sequence as Seq

import qualified Graphics.Vty as Vty

import Smos.Types

findActivations :: Seq KeyPress -> KeyPress -> KeyMappings -> [Activation]
findActivations history kp mappings =
    sortActivations $
    concatMap (`findExactActivations` mappings) $ tails $ toList $ history |> kp

findExactActivations :: [KeyPress] -> KeyMappings -> [Activation]
findExactActivations history mappings =
    flip mapMaybe mappings $ \case
        MapVtyExactly kp_ a ->
            case history of
                [kp] ->
                    if keyPressMatch kp kp_
                        then Just
                                 Activation
                                     { activationPriority = MatchExact
                                     , activationMatch = Seq.singleton kp
                                     , activationName = actionName a
                                     , activationFunc = actionFunc a ()
                                     }
                        else Nothing
                _ -> Nothing
        MapAnyTypeableChar a ->
            case history of
                [kp@(KeyPress k [])] ->
                    case k of
                        Vty.KChar c ->
                            Just
                                Activation
                                    { activationPriority = MatchAnyChar
                                    , activationMatch = Seq.singleton kp
                                    , activationName = actionName a
                                    , activationFunc = actionFunc a c
                                    }
                        _ -> Nothing
                _ -> Nothing
        mc@(MapCombination _ _) ->
            let go :: [KeyPress] -- History
                   -> KeyMapping
                   -> Seq KeyPress -- Match
                   -> Maybe Activation
                go hs km acc =
                    case (hs, km) of
                        ([], _) -> Nothing
                        ([hkp], MapVtyExactly kp_ a) ->
                            if keyPressMatch hkp kp_
                                then Just
                                         Activation
                                             { activationPriority = MatchExact
                                             , activationMatch = acc |> hkp
                                             , activationName = actionName a
                                             , activationFunc = actionFunc a ()
                                             }
                                else Nothing
                        ([hkp@(KeyPress hk _)], MapAnyTypeableChar a) ->
                            case hk of
                                Vty.KChar c ->
                                    Just
                                        Activation
                                            { activationPriority = MatchAnyChar
                                            , activationMatch = acc |> hkp
                                            , activationName = actionName a
                                            , activationFunc = actionFunc a c
                                            }
                                _ -> Nothing
                        (hkp:hkps, MapCombination kp_ km_) ->
                            if keyPressMatch hkp kp_
                                then go hkps km_ (acc |> hkp)
                                else Nothing
                        (_, _) -> Nothing
             in go history mc Seq.empty

keyPressMatch :: KeyPress -> KeyPress -> Bool
keyPressMatch (KeyPress k1 mods1) (KeyPress k2 mods2) =
    k1 == k2 && sort mods1 == sort mods2

data Activation = Activation
    { activationPriority :: Priority
    , activationMatch :: Seq KeyPress
    , activationName :: Text
    , activationFunc :: SmosM ()
    }

sortActivations :: [Activation] -> [Activation]
sortActivations =
    sortOn
        (\Activation {..} ->
             (Ord.Down activationPriority, Ord.Down $ length activationMatch))
