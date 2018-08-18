{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Activation
    ( findActivations
    ) where

import Import

import Data.Ord as Ord
import qualified Data.Sequence as Seq



import qualified Graphics.Vty as Vty


import Smos.Types

findActivations ::
       Seq KeyPress
    -> KeyPress
    -> KeyMappings
    -> [(Priority, Seq KeyPress, Text, SmosM ())]
findActivations history kp@(KeyPress k _) mappings
    -- Remember, the history is in reverse, so newest presses first
 =
    let toMatch = kp <| history
        ls =
            flip mapMaybe mappings $ \case
                MapVtyExactly kp_ a ->
                    if keyPressMatch kp kp_
                        then Just
                                 ( MatchExact
                                 , Seq.singleton kp
                                 , actionName a
                                 , actionFunc a ())
                        else Nothing
                MapAnyTypeableChar a ->
                    case k of
                        Vty.KChar c ->
                            Just
                                ( MatchAnyChar
                                , Seq.singleton kp
                                , actionName a
                                , actionFunc a c)
                        _ -> Nothing
                mc@(MapCombination _ _) ->
                    let go :: Seq KeyPress -- History
                           -> KeyMapping
                           -> Seq KeyPress -- Match
                           -> Maybe (Priority, Seq KeyPress, Text, SmosM ())
                        go hs km acc =
                            case (hs, km) of
                                (Empty, _) -> Nothing
                                (hkp :<| _, MapVtyExactly kp_ a) ->
                                    if keyPressMatch hkp kp_
                                        then Just
                                                 ( MatchExact
                                                 , acc |> hkp
                                                 , actionName a
                                                 , actionFunc a ())
                                        else Nothing
                                (hkp@(KeyPress hk _) :<| _, MapAnyTypeableChar a) ->
                                    case hk of
                                        Vty.KChar c ->
                                            Just
                                                ( MatchAnyChar
                                                , acc |> hkp
                                                , actionName a
                                                , actionFunc a c)
                                        _ -> Nothing
                                (hkp :<| hkps, MapCombination kp_ km_) ->
                                    if keyPressMatch hkp kp_
                                        then go hkps km_ (acc |> hkp)
                                        else Nothing
                     in go toMatch mc Seq.empty
     in sortOn (\(p, match, _, _) -> Ord.Down (p, length match)) ls

keyPressMatch :: KeyPress -> KeyPress -> Bool
keyPressMatch (KeyPress k1 mods1) (KeyPress k2 mods2) =
    k1 == k2 && sort mods1 == sort mods2
