module TreeCursor where

import CSS as CSS
import Control.Monad.State (modify_)
import Control.MonadZero (guard)
import Cursor.Tree.Base
import Cursor.Tree.Insert
import Cursor.Tree.Movement
import Cursor.Tree.Types
import Data.Array as Array
import Data.Const
import Data.Foldable (traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Maybe (Maybe(..))
import Data.NonEmpty
import Data.Tuple
import Debug.Trace as Debug
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Data.Lens
import Prelude
import Unsafe.Coerce
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as WUEK
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Header as Header
import Data.Symbol

type State
  = { cursor :: TreeCursor String String
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
  | EntryClicked PathToClickedEntry
  | HandleHeader String

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState:
      \_ ->
        { cursor:
          -- hello
          -- |- left
          -- |- world  <- 
          -- | |- below
          -- |- right
          { treeAbove:
            Just
              ( TreeAbove
                  { treeAboveLefts: CTree { rootLabel: "left", subForest: EmptyCForest } : Nil
                  , treeAboveAbove: Nothing
                  , treeAboveNode: "hello"
                  , treeAboveRights: CTree { rootLabel: "right", subForest: EmptyCForest } : Nil
                  }
              )
          , treeBelow:
            OpenForest
              ( NonEmptyList
                  ( CTree { rootLabel: "below", subForest: EmptyCForest } :| Nil
                  )
              )
          , treeCurrent: "world"
          }
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

render :: forall m. State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ HP.ref (H.RefLabel "cursor")
    ]
    [ renderTreeCursor state.headerSelected state.cursor
    ]

renderTreeCursor ::
  forall m.
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
          [ goUnseletected (GoToParent p) above
          , HH.div
              [ CSS.style do
                  CSS.marginLeft (CSS.px 30.0)
              ]
              [ HH.div_
                  ( Array.fromFoldable
                      ( List.mapWithIndex
                          ( \i ct ->
                              goCTree
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
                              goCTree
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
          $ [ goSelected current
            , HH.div
                [ CSS.style do
                    CSS.marginLeft (CSS.px 30.0)
                ]
                [ goCForest ClickedEqualsSelected subForest ]
            ]
      )

  goCForest :: PathToClickedEntry -> CForest String -> H.ComponentHTML Action ChildSlots Aff
  goCForest p' = case _ of
    EmptyCForest -> HH.text ""
    ClosedForest _ -> HH.text ""
    OpenForest ne ->
      HH.div_
        ( Array.fromFoldable
            ( NE.mapWithIndex (\i ct -> goCTree (GoToChild i p') ct) ne
            )
        )

  goCTree :: PathToClickedEntry -> CTree String -> H.ComponentHTML Action ChildSlots Aff
  goCTree p (CTree cn) =
    HH.div_
      [ goUnseletected p cn.rootLabel
      , HH.div
          [ CSS.style do
              CSS.marginLeft (CSS.px 30.0)
          ]
          [ goCForest p cn.subForest ]
      ]

  goUnseletected :: PathToClickedEntry -> String -> H.ComponentHTML Action ChildSlots Aff
  goUnseletected p s =
    HH.p
      [ CSS.style do
          CSS.minHeight (CSS.px 30.0)
      , HE.onClick (\_ -> Just (EntryClicked p))
      ]
      [ HH.text s
      ]

  goSelected :: String -> H.ComponentHTML Action ChildSlots Aff
  goSelected s = case selected of
    Just sp -> HH.slot _header unit Header.component { contents: s, startingPosition: sp } (Just <<< HandleHeader)
    Nothing ->
      HH.p
        [ CSS.style do
            CSS.minHeight (CSS.px 30.0)
        , HE.onClick (\_ -> Just (EntryClicked ClickedEqualsSelected))
        ]
        [ HH.text s
        , HH.text " <--"
        ]

handle :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handle =
  let
    treeMod :: (TreeCursor String String -> TreeCursor String String) -> H.HalogenM State Action ChildSlots o Aff Unit
    treeMod func = modify_ (\s -> s { cursor = func s.cursor })

    treeModM :: (TreeCursor String String -> Maybe (TreeCursor String String)) -> H.HalogenM State Action ChildSlots o Aff Unit
    treeModM func = treeMod (\tc -> fromMaybe tc (func tc))
  in
    case _ of
      Init -> do
        document <- H.liftEffect $ Web.document =<< Web.window
        H.subscribe' \sid ->
          ES.eventListenerEventSource
            KET.keyup
            (HTMLDocument.toEventTarget document)
            (map (KeyPressed sid) <<< WUEK.fromEvent)
      EntryClicked p -> case p of
        ClickedEqualsSelected -> modify_ (_ { headerSelected = Just Header.End })
        _ -> do
          modify_ (_ { headerSelected = Nothing })
          treeModM (moveUsingPath identity identity p)
      KeyPressed _ ke -> do
        s <- H.get
        let
          k = WUEK.key ke
        H.liftEffect (Console.log k)
        if isJust s.headerSelected then
          pure unit
        else do
          case unit of
            _
              | k == "ArrowDown" || k == "j" -> treeModM (treeCursorSelectNext identity identity)
              | k == "ArrowUp" || k == "k" -> treeModM (treeCursorSelectPrev identity identity)
              | k == "ArrowLeft" || k == "h" -> treeModM (treeCursorSelectAbove identity identity)
              | k == "ArrowRight" || k == "l" -> treeModM (treeCursorSelectBelowAtEnd identity identity)
              | k == "e" -> treeModM (treeCursorInsertAndSelect identity identity (Tree { rootLabel: "new", subForest: Nil }))
              | k == "E" -> treeMod (treeCursorAddChildAtStartAndSelect identity identity (Tree { rootLabel: "new", subForest: Nil }))
              | k == "a" || k == "A" -> modify_ (_ { headerSelected = Just Header.End })
              | k == "i" || k == "I" -> modify_ (_ { headerSelected = Just Header.Beginning })
            _ -> pure unit
      HandleHeader str -> modify_ (\s -> s { headerSelected = Nothing, cursor = s.cursor # treeCursorCurrentL .~ str })
