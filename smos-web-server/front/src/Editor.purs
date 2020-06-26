module Editor where

import Prelude
import Control.Monad.State (modify_)
import Cursor.Tree.Movement (PathToClickedEntry(..))
import Cursor.Tree.Types (Forest, cTree, treeCursorCurrentL)
import Cursor.Types (DeleteOrUpdate(..), dullDelete)
import Cursor.Forest (ForestCursor, forestCursorAddChildToTreeAtStartAndSelect, forestCursorAppendAndSelect, forestCursorDeleteElem, forestCursorDeleteSubTree, forestCursorDemoteElem, forestCursorDemoteSubTree, forestCursorMoveUsingPath, forestCursorPromoteElem, forestCursorPromoteSubTree, forestCursorSelectAbove, forestCursorSelectBelowAtEnd, forestCursorSelectNext, forestCursorSelectPrev, forestCursorSelectedTreeL, forestCursorSwapNext, forestCursorSwapPrev, forestCursorToggleCurrentForest, forestCursorToggleCurrentForestRecursively, makeForestCursor, singletonForestCursor)
import Data.Const (Const)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Data.Lens ((.~))
import Web.Event.Event as WEE
import Web.HTML (window) as Web
import Halogen.Query.EventSource as ES
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as WUEK
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as WUEM
import Web.UIEvent.MouseEvent.EventTypes as MET
import Header as Header
import Render (Action(..), State, render)
import Data.Symbol (SProxy(..))

type ChildSlots
  = ( header ::
      H.Slot (Const Void) -- No queries
        String -- The built header
        Unit -- Only one slot, so use unit as index
    )

_header :: SProxy "header"
_header = SProxy

component :: forall q i o. Forest String -> H.Component HH.HTML q i o Aff
component t =
  H.mkComponent
    { initialState:
      \_ ->
        { cursor:
          makeForestCursor identity <<< map (cTree true) <$> NE.fromList t
        , headerSelected: Nothing
        }
    , render: render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handle
            , initialize = Just Init
            }
    }

handle :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handle =
  let
    forestMModM :: (Maybe (ForestCursor String String) -> Maybe (ForestCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestMModM func = modify_ (\s -> s { cursor = func s.cursor })

    forestMMod :: (Maybe (ForestCursor String String) -> ForestCursor String String) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestMMod func = forestMModM $ Just <<< func

    forestMod :: (ForestCursor String String -> ForestCursor String String) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestMod func = forestMModM $ map func

    forestModM :: (ForestCursor String String -> Maybe (ForestCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModM func = forestMod (\tc -> fromMaybe tc (func tc))

    forestModDOU :: (ForestCursor String String -> DeleteOrUpdate (ForestCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModDOU func =
      forestMModM
        $ \mfc -> case mfc of
            Nothing -> Nothing
            Just fc -> dullDelete (func fc)

    forestModDOUM :: (ForestCursor String String -> Maybe (DeleteOrUpdate (ForestCursor String String))) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModDOUM func = forestModDOU (\fc -> fromMaybe (Updated fc) (func fc))
  in
    case _ of
      Init -> do
        document <- H.liftEffect $ Web.document =<< Web.window
        H.subscribe' \sid -> do
          ES.eventListenerEventSource
            KET.keyup
            (HTMLDocument.toEventTarget document)
            (map (KeyPressed sid) <<< WUEK.fromEvent)
        H.subscribe' \sid -> do
          ES.eventListenerEventSource
            MET.click
            (HTMLDocument.toEventTarget document)
            (map (MouseClicked sid) <<< WUEM.fromEvent)
      EntryClicked p me -> do
        H.liftEffect $ WEE.stopPropagation (WUEM.toEvent me)
        case p of
          ClickedEqualsSelected -> modify_ (_ { headerSelected = Just Header.End })
          _ -> do
            modify_ (_ { headerSelected = Nothing })
            forestModM (forestCursorMoveUsingPath identity identity p)
      MouseClicked _ _ -> do
        modify_ (_ { headerSelected = Nothing })
      KeyPressed _ ke -> do
        s <- H.get
        let
          k = WUEK.key ke

          ak = WUEK.altKey ke

          ck = WUEK.ctrlKey ke

          sk = WUEK.shiftKey ke
        H.liftEffect (Console.log k)
        if isJust s.headerSelected then
          pure unit
        else do
          case unit of
            _
              | not ak && (k == "ArrowDown" || k == "j") -> forestModM (forestCursorSelectNext identity identity)
              | not ak && (k == "ArrowUp" || k == "k") -> forestModM (forestCursorSelectPrev identity identity)
              | not ak && (k == "ArrowLeft" || k == "h") -> forestModM (forestCursorSelectAbove identity identity)
              | not ak && (k == "ArrowRight" || k == "l") -> forestModM (forestCursorSelectBelowAtEnd identity identity)
              | k == "e" -> do
                forestMMod
                  $ \mfc -> case mfc of
                      Nothing -> singletonForestCursor ""
                      Just fc -> forestCursorAppendAndSelect identity identity "" fc
                modify_ (_ { headerSelected = Just Header.Beginning })
              | k == "E" -> do
                forestMMod
                  $ \mfc -> case mfc of
                      Nothing -> singletonForestCursor ""
                      Just fc -> forestCursorAddChildToTreeAtStartAndSelect identity identity "" fc
                modify_ (_ { headerSelected = Just Header.Beginning })
              | k == "a" || k == "A" -> modify_ (_ { headerSelected = Just Header.End })
              | k == "i" || k == "I" -> modify_ (_ { headerSelected = Just Header.Beginning })
              | k == "d" -> forestModDOU (forestCursorDeleteElem identity)
              | k == "D" -> forestModDOU (forestCursorDeleteSubTree identity)
              | k == "D" -> forestModDOU (forestCursorDeleteSubTree identity)
              | ak && (k == "ArrowDown" || k == "j") -> forestModM forestCursorSwapNext
              | ak && (k == "ArrowUp" || k == "k") -> forestModM forestCursorSwapPrev
              | ak && k == "ArrowLeft" -> forestModM (forestCursorPromoteElem identity identity)
              | ak && sk && (k == "ArrowLeft" || k == "H") -> forestModM (forestCursorPromoteSubTree identity identity)
              | ak && k == "ArrowRight" -> forestModM (forestCursorDemoteElem identity identity)
              | ak && sk && (k == "ArrowRight" || k == "L") -> forestModM (forestCursorDemoteSubTree identity identity)
              | k == "c" -> forestModM forestCursorToggleCurrentForest
              | k == "C" -> forestModM forestCursorToggleCurrentForestRecursively
            _ -> pure unit
      HandleHeader str -> do
        modify_ (\s -> s { headerSelected = Nothing })
        forestMod ((forestCursorSelectedTreeL <<< treeCursorCurrentL) .~ str)
