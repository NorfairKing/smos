module TreeCursor where

import CSS as CSS
import Prelude
import Data.List.NonEmpty as NE
import Data.Foldable (traverse_)
import Unsafe.Coerce
import Control.MonadZero (guard)
import Cursor.Tree.Base
import Data.Array as Array
import Cursor.Tree.Types
import Data.List.NonEmpty
import Data.NonEmpty
import Data.List
import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Web.HTML.HTMLElement as WHHE
import Web.UIEvent.KeyboardEvent as WUEK
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE

type State
  = TreeCursor String String

data Action
  = Action

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
    , eval: H.mkEval $ H.defaultEval { handleAction = handle }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state = foldTreeCursor wrap cur state
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
  Action -> pure unit
