module TreeCursor where

import Data.Maybe
import CSS as CSS
import Control.Monad.State (modify_)
import Web.HTML.Window (document) as Web
import Web.HTML (window) as Web
import Halogen.Query.EventSource as ES
import Control.MonadZero (guard)
import Cursor.Tree.Base
import Cursor.Tree.Movement
import Cursor.Tree.Types
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.List
import Data.List.NonEmpty
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.NonEmpty
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Unsafe.Coerce
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLDocument as HTMLDocument
import Web.UIEvent.KeyboardEvent as WUEK
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State
  = TreeCursor String String

data Action
  = Init
  | KeyPressed H.SubscriptionId WUEK.KeyboardEvent

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState:
      \_ ->
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
    , render: render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handle
            , initialize = Just Init
            }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.ref (H.RefLabel "cursor")
    ]
    [ renderTreeCursor state
    ]

renderTreeCursor :: forall m. TreeCursor String String -> H.ComponentHTML Action () m
renderTreeCursor = foldTreeCursor wrap cur
  where
  wrap :: List (CTree String) -> String -> List (CTree String) -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
  wrap lefts above rights current =
    HH.div_
      [ goUnseletected above
      , HH.div
          [ CSS.style do
              CSS.marginLeft (CSS.px 30.0)
          ]
          [ HH.div_ (Array.fromFoldable (map goCTree lefts))
          , current
          , HH.div_ (Array.fromFoldable (map goCTree rights))
          ]
      ]

  cur :: String -> CForest String -> H.ComponentHTML Action () m
  cur current subForest =
    HH.div_
      $ [ goSelected current
        , HH.div
            [ CSS.style do
                CSS.marginLeft (CSS.px 30.0)
            ]
            [ goCForest subForest ]
        ]

  goCForest :: CForest String -> H.ComponentHTML Action () m
  goCForest = case _ of
    EmptyCForest -> HH.text ""
    ClosedForest _ -> HH.text ""
    OpenForest ne -> HH.div_ (Array.fromFoldable (map goCTree (NE.toList ne)))

  goCTree :: CTree String -> H.ComponentHTML Action () m
  goCTree (CTree cn) =
    HH.div_
      [ goUnseletected cn.rootLabel
      , HH.div
          [ CSS.style do
              CSS.marginLeft (CSS.px 30.0)
          ]
          [ goCForest cn.subForest ]
      ]

  goUnseletected :: String -> H.ComponentHTML Action () m
  goUnseletected s =
    HH.p_
      [ HH.text s
      ]

  goSelected :: String -> H.ComponentHTML Action () m
  goSelected s =
    HH.p_
      [ HH.text s
      , HH.text " <--"
      ]

handle :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handle = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (KeyPressed sid) <<< WUEK.fromEvent)
  KeyPressed _ ke -> do
    H.liftEffect (Console.log (WUEK.key ke))
    let
      treeMod :: (TreeCursor String String -> TreeCursor String String) -> H.HalogenM State Action () o Aff Unit
      treeMod func = modify_ func

      treeModM :: (TreeCursor String String -> Maybe (TreeCursor String String)) -> H.HalogenM State Action () o Aff Unit
      treeModM func = treeMod (\tc -> fromMaybe tc (func tc))

      k = WUEK.key ke
    case unit of
      _
        | k == "ArrowDown" || k == "j" -> treeModM (treeCursorSelectNext identity identity)
        | k == "ArrowUp" || k == "k" -> treeModM (treeCursorSelectPrev identity identity)
        | k == "ArrowLeft" || k == "h" -> treeModM (treeCursorSelectAbove identity identity)
        | k == "ArrowRight" || k == "l" -> treeModM (treeCursorSelectBelowAtEnd identity identity)
      _ -> pure unit
