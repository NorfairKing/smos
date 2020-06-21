module Editor where

import Prelude
import CSS (marginLeft, minHeight, px) as CSS
import Control.Monad.State (modify_)
import Cursor.Tree.Base (foldTreeCursor)
import Cursor.Tree.Movement (PathToClickedEntry(..))
import Cursor.Tree.Types (CForest(..), CTree(..), Tree(..), TreeCursor, cTree, treeCursorCurrentL, Forest)
import Cursor.Types (DeleteOrUpdate, dullDelete, dullMDelete)
import Cursor.Forest (ForestCursor, foldForestCursor, forestCursorAddChildToTreeAtStartAndSelect, forestCursorAppendAndSelect, forestCursorDeleteElem, forestCursorDeleteSubTree, forestCursorDemoteElem, forestCursorDemoteSubTree, forestCursorMoveUsingPath, forestCursorPromoteElem, forestCursorPromoteSubTree, forestCursorSelectAbove, forestCursorSelectBelowAtEnd, forestCursorSelectNext, forestCursorSelectPrev, forestCursorSelectedTreeL, forestCursorSwapNext, forestCursorSwapPrev, forestCursorToggleCurrentForest, forestCursorToggleCurrentForestRecursively, makeForestCursor)
import Data.Array as Array
import Data.Const (Const)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Data.Lens ((.~))
import Web.Event.Event as WEE
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as WUEK
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as WUEM
import Web.UIEvent.MouseEvent.EventTypes as MET
import Header as Header
import Data.Symbol (SProxy(..))

type State
  = { cursor :: ForestCursor String String
    , headerSelected :: Maybe Header.StartingPosition
    }

type ChildSlots
  = ( header ::
      H.Slot (Const Void) -- No queries
        String -- The built header
        Unit -- Only one slot, so use unit as index
    )

_header :: SProxy "header"
_header = SProxy

data Action
  = Init
  | KeyPressed H.SubscriptionId WUEK.KeyboardEvent
  | MouseClicked H.SubscriptionId WUEM.MouseEvent
  | EntryClicked PathToClickedEntry WUEM.MouseEvent
  | HandleHeader String

component :: forall q i o. Forest String -> H.Component HH.HTML q i o Aff
component t =
  H.mkComponent
    { initialState:
      \_ ->
        { cursor:
          makeForestCursor identity $ map (cTree true)
            $ case NE.fromList t of
                Nothing -> NE.singleton (Tree { rootLabel: "empty", subForest: Nil })
                Just ne -> ne
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

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ HP.ref (H.RefLabel "cursor")
    ]
    [ renderForestCursor state.headerSelected state.cursor
    ]

renderForestCursor ::
  Maybe Header.StartingPosition ->
  ForestCursor String String ->
  H.ComponentHTML Action ChildSlots Aff
renderForestCursor selected = foldForestCursor go
  where
  go ::
    List (CTree String) ->
    TreeCursor String String ->
    List (CTree String) ->
    H.ComponentHTML Action ChildSlots Aff
  go befores current afters =
    HH.div_
      $ Array.concat
          [ Array.fromFoldable (map (renderCTree selected ClickedEqualsSelected) befores)
          , [ renderTreeCursor selected current ]
          , Array.fromFoldable (map (renderCTree selected ClickedEqualsSelected) afters)
          ]

renderTreeCursor ::
  Maybe Header.StartingPosition ->
  TreeCursor String String ->
  H.ComponentHTML Action ChildSlots Aff
renderTreeCursor selected = snd <<< foldTreeCursor wrap cur
  where
  wrap ::
    List (CTree String) ->
    String ->
    List (CTree String) ->
    Tuple PathToClickedEntry (H.ComponentHTML Action ChildSlots Aff) ->
    Tuple PathToClickedEntry (H.ComponentHTML Action ChildSlots Aff)
  wrap lefts above rights (Tuple p current) =
    Tuple (GoToParent p)
      ( HH.div_
          [ renderUnseletected selected (GoToParent p) above emptyCForestIndication
          , HH.div
              [ CSS.style do
                  CSS.marginLeft (CSS.px 30.0)
              ]
              [ HH.div_
                  ( Array.fromFoldable
                      ( List.mapWithIndex
                          ( \i ct ->
                              renderCTree selected
                                ( GoToChild i
                                    ( GoToParent p
                                    )
                                )
                                ct
                          )
                          lefts
                      )
                  )
              , current
              , HH.div_
                  ( Array.fromFoldable
                      ( List.mapWithIndex
                          ( \i ct ->
                              renderCTree selected
                                ( GoToChild (i + 1 + List.length lefts)
                                    ( GoToParent p
                                    )
                                )
                                ct
                          )
                          rights
                      )
                  )
              ]
          ]
      )

  cur :: String -> CForest String -> Tuple PathToClickedEntry (H.ComponentHTML Action ChildSlots Aff)
  cur current subForest =
    Tuple ClickedEqualsSelected
      ( HH.div_
          $ [ renderSelected selected current (cForestIndication subForest)
            , HH.div
                [ CSS.style do
                    CSS.marginLeft (CSS.px 30.0)
                ]
                [ renderCForest selected ClickedEqualsSelected subForest ]
            ]
      )

renderCForest :: Maybe Header.StartingPosition -> PathToClickedEntry -> CForest String -> H.ComponentHTML Action ChildSlots Aff
renderCForest selected p' = case _ of
  EmptyCForest -> HH.text ""
  ClosedForest _ -> HH.text ""
  OpenForest ne ->
    HH.div_
      ( Array.fromFoldable
          ( NE.mapWithIndex (\i ct -> renderCTree selected (GoToChild i p') ct) ne
          )
      )

renderCTree :: Maybe Header.StartingPosition -> PathToClickedEntry -> CTree String -> H.ComponentHTML Action ChildSlots Aff
renderCTree selected p (CTree cn) =
  HH.div_
    [ renderUnseletected selected p cn.rootLabel (cForestIndication cn.subForest)
    , HH.div
        [ CSS.style do
            CSS.marginLeft (CSS.px 30.0)
        ]
        [ renderCForest selected p cn.subForest ]
    ]

renderUnseletected :: Maybe Header.StartingPosition -> PathToClickedEntry -> String -> CForestIndication -> H.ComponentHTML Action ChildSlots Aff
renderUnseletected selected p s cfi =
  HH.p
    [ CSS.style do
        CSS.minHeight (CSS.px 30.0)
    , HE.onClick (\me -> Just (EntryClicked p me))
    ]
    [ HH.text s
    , renderCForestIndication cfi
    ]

renderSelected :: Maybe Header.StartingPosition -> String -> CForestIndication -> H.ComponentHTML Action ChildSlots Aff
renderSelected selected s cfi = case selected of
  Just sp ->
    HH.p_
      [ HH.slot _header unit Header.component { contents: s, startingPosition: sp } (Just <<< HandleHeader)
      , renderCForestIndication cfi
      ]
  Nothing ->
    HH.p
      [ CSS.style do
          CSS.minHeight (CSS.px 30.0)
      , HE.onClick (\me -> Just (EntryClicked ClickedEqualsSelected me))
      ]
      [ HH.text s
      , renderCForestIndication cfi
      , HH.text " <--"
      ]

renderCForestIndication :: CForestIndication -> H.ComponentHTML Action ChildSlots Aff
renderCForestIndication { hiddenNodesBelow } = if hiddenNodesBelow then HH.text " +++" else HH.text ""

type CForestIndication
  = { hiddenNodesBelow :: Boolean }

emptyCForestIndication :: CForestIndication
emptyCForestIndication = { hiddenNodesBelow: false }

cForestIndication :: CForest String -> CForestIndication
cForestIndication cf =
  { hiddenNodesBelow:
    case cf of
      EmptyCForest -> false
      OpenForest _ -> false
      ClosedForest _ -> true
  }

handle :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handle =
  let
    forestMod :: (ForestCursor String String -> ForestCursor String String) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestMod func = modify_ (\s -> s { cursor = func s.cursor })

    forestModM :: (ForestCursor String String -> Maybe (ForestCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModM func = forestMod (\tc -> fromMaybe tc (func tc))

    forestModDOU :: (ForestCursor String String -> DeleteOrUpdate (ForestCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModDOU func = forestModM (\tc -> dullDelete (func tc))

    forestModDOUM :: (ForestCursor String String -> Maybe (DeleteOrUpdate (ForestCursor String String))) -> H.HalogenM State Action ChildSlots o Aff Unit
    forestModDOUM func = forestModM (\tc -> dullMDelete (func tc))
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
              | k == "e" -> forestMod (forestCursorAppendAndSelect identity identity "")
              | k == "E" -> forestMod (forestCursorAddChildToTreeAtStartAndSelect identity identity "")
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
      HandleHeader str -> modify_ (\s -> s { headerSelected = Nothing, cursor = s.cursor # (forestCursorSelectedTreeL <<< treeCursorCurrentL) .~ str })
