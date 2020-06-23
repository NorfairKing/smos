module Render where

import Prelude
import CSS (marginLeft, minHeight, px) as CSS
import Cursor.Tree.Base (foldTreeCursor)
import Cursor.Tree.Movement (PathToClickedEntry(..))
import Cursor.Tree.Types (CForest(..), CTree(..), TreeCursor)
import Cursor.Forest (ForestCursor, foldForestCursor)
import Data.Array as Array
import Data.Const (Const)
import Data.List (List, mapWithIndex)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as WUEK
import Web.UIEvent.MouseEvent as WUEM
import Header as Header
import Data.Symbol (SProxy(..))

type State
  = { cursor :: Maybe (ForestCursor String String)
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

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ HP.ref (H.RefLabel "cursor")
    ]
    [ case state.cursor of
        Nothing -> HH.text "Empty file"
        Just sfc -> renderForestCursor state.headerSelected sfc
    ]

data Action
  = Init
  | KeyPressed H.SubscriptionId WUEK.KeyboardEvent
  | MouseClicked H.SubscriptionId WUEM.MouseEvent
  | EntryClicked PathToClickedEntry WUEM.MouseEvent
  | HandleHeader String

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
    let
      Tuple treePath treeHtml = renderTreeCursor selected current
    in
      HH.div_
        $ Array.concat
            [ Array.fromFoldable (mapWithIndex (\index -> renderCTree selected (GoToSibling index treePath)) befores)
            , [ treeHtml ]
            , Array.fromFoldable (mapWithIndex (\index -> renderCTree selected (GoToSibling (index + 1 + List.length befores) treePath)) afters)
            ]

renderTreeCursor ::
  Maybe Header.StartingPosition ->
  TreeCursor String String ->
  Tuple PathToClickedEntry (H.ComponentHTML Action ChildSlots Aff)
renderTreeCursor selected = foldTreeCursor wrap cur
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
                              renderCTree selected (GoToSibling i p) ct
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
